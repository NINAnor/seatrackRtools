push_db_activity <- function() {
    # Scan the "all" directory
    # Get the information
    import_directory <- file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL")

    log_info("Scan import directory for files...")
    all_files <- list.files(import_directory, pattern = "*.deg|*.act|*.sst|*.tem|*.lux|*.lig", recursive = TRUE, full.names = TRUE)
    all_files_split <- strsplit(basename(all_files), "_")
    file_info_list <- lapply(all_files_split, function(x) {
        data.frame(logger_id = x[1], year_downloaded = x[2], model = tools::file_path_sans_ext(x[3]), id_year = paste(x[1], x[2], sep = "_"))
    })
    file_info <- do.call(rbind, file_info_list)
    file_info <- data.frame(full_path = all_files, file_info)
    file_info$extension <- tools::file_ext(all_files)
    file_info$filename <- basename(file_info$full_path)

    db_sessions <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logging_session"))

    db_loggers <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
    db_sessions <- dplyr::left_join(db_sessions, dplyr::select(
        db_loggers,
        logger_id, logger_serial_no, logger_model
    ), by = "logger_id")

    file_archive <- dplyr::tbl(con, dbplyr::in_schema("loggers", "file_archive"))
    db_sessions <- dplyr::right_join(db_sessions, file_archive, by = "session_id")

    db_sessions <- dplyr::filter(db_sessions, filename %in% file_info$filename)

    # Check which files are missing and log this
    missing_files <- dplyr::setdiff(file_info$filename, dplyr::pull(db_sessions, filename))
    missing_session_summary <- tibble::tibble(dplyr::distinct(file_info[file_info$filename %in% missing_files, c("logger_id", "year_downloaded", "model")]))
    log_warn(
        "There are ", length(missing_files), " files in the import directory that do not have a corresponding session in the database. \n",
        paste(capture.output(print(missing_session_summary, n = nrow(missing_session_summary)))[c(-1, -3)], collapse = "\n")
    )

    db_deployments <- dplyr::tbl(con, dbplyr::in_schema("loggers", "deployment"))
    db_retrievals <- dplyr::tbl(con, dbplyr::in_schema("loggers", "retrieval"))
    db_sessions <- dplyr::left_join(db_sessions, db_deployments, by = "session_id")
    db_sessions <- dplyr::left_join(db_sessions, db_retrievals, by = "session_id")

    recording_types <- list(
        accelerometer = list(table_name = "accelerometer_raw", process_function = NULL, extensions = c()),
        activity = list(table_name = "activity_raw", process_function = NULL, extensions = c(".deg", ".act")),
        light = list(table_name = "light_raw", process_function = load_light_data, extensions = c(".lux", ".lig")),
        temperature = list(table_name = "temperature_raw", process_function = NULL, extensions = c(".sst", ".tem"))
    )


    # Of the sessions present in the database, check whether they exist in the recording tables
    missing_raw_data_files <- sapply(recording_types, function(recording_type) {
        recording_table <- dplyr::tbl(con, dbplyr::in_schema("recordings", recording_type$table_name))
        missing_sessions <- dplyr::anti_join(db_sessions, dplyr::select(recording_table, session_id), by = "session_id")
        current_formats <- recording_type$extensions
        missing_sessions <- dplyr::filter(missing_sessions, file_basename %in% current_formats)
        return_list <- list(dplyr::collect(dplyr::select(missing_sessions, session_id, individ_id, filename, deployment_date, retrieval_date)))
        return(return_list)
    })

    for (recording_type_name in names(recording_types)) {
        recording_type <- recording_types[[recording_type_name]]
        recording_files <- missing_raw_data_files[[recording_type_name]]
        recording_files <- dplyr::left_join(recording_files, file_info, by = "filename", suffix = c("", "y"))
        for (i in seq_along(recording_files)) {
            handle_activity(recording_files[i, ], recording_type)
        }
    }

    # Files not in archive get loaded and uploaded
}

handle_activity <- function(file_info, recording_type) {
    activity_data <- recording_type$process_function(file_info)
    if (is.null(activity_data)) {
        log_warn(glue::glue("{file_info$filename} will not be handled."))
        return()
    }
    # Clip to deployment and retrieval dates
    clipped_activity_data <- dplyr::filter(activity_data, as.Date(date_time) >= as.Date(file_info$deployment_date), as.Date(date_time) <= as.Date(file_info$retrieval_date))
    if (nrow(clipped_activity_data) == 0) {
        log_warn(glue::glue("{file_info$filename} has no data within the deployment and retrieval dates."))
        return()
    }
    clipped_activity_data <- tibble(session_id = file_info$session_id, filename = file_info$filename, individ_id = file_info$individ_id, clipped_activity_data)
    # Push to database

    # Save a temp file, push to archive.
}

load_light_data <- function(file_info) {
    # load a light data file
    if (file_info$extension == "lig") {
        light_data <- handle_lotek(file_info$full_path)
    } else if (file_info$extension == "lux") {
        light_data <- handle_migrate(file_info$full_path)
    }
    return(light_data)
}

handle_migrate <- function(filepath) {
    # FROM VEGARD'S ORIGINAL SCRIPT

    # Read header
    file_header <- readLines(filepath, n = 11)
    clipped <- FALSE
    if (length(grep("clipped", file_header[5])) == 1 || length(grep("clipped ON", file_header[2])) == 1) {
        clipped <- TRUE
    } else {
        clipped <- FALSE
    }

    file <- read.table(filepath, sep = "\t", header = FALSE, fill = TRUE, skip = 20)

    if (nrow(file) < 5) {
        log_warn(glue::glue("{basename(filepath)} has too few lines."))
        return(NULL)
    }


    if (ncol(file) == 5) file <- file %>% dplyr::select(1, 4)
    colnames(file) <- c("V1", "V2")

    if (length(grep(",", file$V2[1])) == 1) file$V2 <- as.numeric(gsub(file$V2, ",", ".", fixed = TRUE))

    ## Error in light:
    # remove empty data rows from light
    file <- file[!is.na(file$V2), ]

    # remove logger data that exceed maximum possible value of light
    if (clipped == "TRUE") {
        file <- file[file$V2 < 1200, ]
    }

    # remove logger data that has only 0 values of light
    if (max(file$V2) == 0) {
        log_warn(glue::glue("{basename(filepath)} has only 0 values of light."))
        return(NULL)
    }
    # remove logger data that has negative values of light
    if (min(file$V2) < 0) {
        log_warn(glue::glue("{basename(filepath)} has negative values of light."))
        return(NULL)
    }

    # date format
    if (substr(file$V1[1], 3, 3) == ".") {
        file$V3 <- as.Date(substr(file$V1, 1, 10), "%d.%m.%Y")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%d.%m.%Y %H:%M:%S")
    }
    if (substr(file$V1[1], 2, 2) == ".") {
        file$V3 <- as.Date(substr(file$V1, 1, 9), "%d.%m.%Y")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%d.%m.%Y %H:%M:%S")
    }
    if (substr(file$V1[1], 4, 4) == ".") {
        file$V3 <- as.Date(substr(file$V1, 1, 8), "%d.%m.%Y")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%d.%m.%Y %H:%M:%S")
    }
    if (substr(file$V1[1], 3, 3) == "/") {
        file$V3 <- as.Date(substr(file$V1, 1, 10), "%d/%m/%Y")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%d/%m/%Y %H:%M:%S")
    }
    if (substr(file$V1[1], 5, 5) == "-") {
        file$V3 <- as.Date(substr(file$V1, 1, 10), "%Y-%m-%d")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%Y-%m-%d %H:%M:%S")
    }

    ## Error in date and time:
    # remove if data holes exceed 3 months or difference in dates are negative
    diff_time <- as.numeric(difftime(file$V1[2:length(file$V1)], file$V1[1:(length(file$V1) - 1)], "GMT", units = c("mins")))
    if (min(diff_time) < 0) {
        log_warn(glue::glue("{basename(filepath)} has negative time differences between rows."))
        return(NULL)
    }

    if (max(diff_time) > 129600) {
        log_warn(glue::glue("{basename(filepath)} has time differences between rows that exceed 3 months."))
        return(NULL)
    }

    file_final <- data.frame(
        date_time = lubridate::as_datetime(file$V1),
        clipped = clipped,
        raw_light = as.numeric(file$V2),
        std_light = as.numeric(file$V2) / max(file$V2)
    )

    return(file_final)
}

handle_lotek <- function(filename) {
    file <- read.table(filename, sep = ",", header = FALSE, fill = TRUE, skip = 1)
    if (nrow(file) < 5) {
        log_warn(glue::glue("{basename(filepath)} has too few lines."))
        return(NULL)
    }

    file$V2 <- as.POSIXct(file$V2, format = "%d/%m/%y %H:%M:%S", tz = "GMT")
    file$V5 <- as.Date(substr(file$V2, 1, 10))

    ## Error in light:
    # remove NA's data from light
    file <- file[!is.na(file$V4), ]
    # remove 'suspect' lines (light_bt$V1)
    file <- file[file$V1 == "ok", ]

    # remove logger data that exceed maximum possible value of light
    if (max(file$V4) > 64) {
        log_warn(glue::glue("{basename(filepath)} has values of light that exceed the maximum possible value of 64."))
        return(NULL)
    }
    # remove logger data that has only 0 values of light
    if (max(file$V4) == 0) {
        log_warn(glue::glue("{basename(filepath)} has only 0 values of light."))
        return(NULL)
    }
    # remove logger data that has negative values of light
    if (min(file$V4) < 0) {
        log_warn(glue::glue("{basename(filepath)} has negative values of light."))
    }

    ## Error in date and time:
    # remove if data holes exceed 3 months or difference in dates are negative
    diff_time <- as.numeric(difftime(file$V2[2:length(file$V2)], file$V2[1:(length(file$V2) - 1)], "GMT", units = c("mins")))
    if (min(diff_time) < 0) {
        log_warn(glue::glue("{basename(filepath)} has negative time differences between rows."))
        return(NULL)
    }
    if (max(diff_time) > 129600) {
        log_warn(glue::glue("{basename(filepath)} has time differences between rows that exceed 3 months."))
        return(NULL)
    }

    file_final <- data.frame(
        date_time = lubridate::as_datetime(file$V2),
        clipped = "TRUE",
        raw_light = as.numeric(file$V4),
        std_light = as.numeric(file$V4)
    )

    return(file_final)
}
