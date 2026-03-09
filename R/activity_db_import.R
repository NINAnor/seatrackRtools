push_db_activity <- function() {
    recording_types <- list(
        accelerometer = list(table_name = "accelerometer_raw", process_function = NULL, extensions = c(), argname = "accelerationData"),
        activity = list(table_name = "activity_raw", process_function = load_immersion_data, extensions = c(".deg", ".act"), argname = "activityData"),
        light = list(table_name = "light_raw", process_function = load_light_data, extensions = c(".lux", ".lig"), argname = "lightData"),
        temperature = list(table_name = "temperature_raw", process_function = load_temperature_data, extensions = c(".sst", ".tem"), argname = "temperatureData")
    )

    all_formats <- sapply(recording_types, function(recording_type) {
        if (length(recording_type$extensions) == 0) {
            return(NA)
        }
        paste(sapply(recording_type$extensions, function(x) {
            paste0("*", x)
        }), collapse = "|")
    })

    all_formats_pattern <- paste(all_formats[!is.na(all_formats)], collapse = "|")

    # Scan the "all" directory
    # Get the information
    import_directory <- file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL")

    log_info("Scan import directory for files...")
    all_files <- list.files(import_directory, pattern = all_formats_pattern, recursive = TRUE, full.names = TRUE)
    all_files_split <- strsplit(basename(all_files), "_")
    file_info_list <- lapply(all_files_split, function(x) {
        data.frame(logger_id = x[1], year_downloaded = x[2], model = tools::file_path_sans_ext(x[3]), id_year = paste(x[1], x[2], sep = "_"))
    })
    file_info <- do.call(rbind, file_info_list)
    file_info <- data.frame(full_path = all_files, file_info)
    file_info$extension <- tools::file_ext(all_files)
    file_info$filename <- basename(file_info$full_path)

    log_info("Check database for existing files...")
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
        n_files <- nrow(recording_files)
        if (n_files == 0) {
            log_info("No missing files for recording type: ", recording_type_name)
            next
        }
        log_info("Handling ", n_files, " files for recording type: ", recording_type_name)
        recording_files <- dplyr::left_join(recording_files, file_info, by = "filename", suffix = c("", "y"))
        for (i in seq_len(n_files)) {
            result <- handle_activity(recording_files[i, ], recording_type)
        }
    }
}

handle_activity <- function(file_info, recording_type) {
    log_trace(glue::glue("{file_info$filename} - begin handling."))
    if (is.null(recording_type$process_function)) {
        log_warn(glue::glue("No handling function for {recording_type$table_name}"))
        activity_data <- NULL
    } else {
        activity_data <- tryCatch(
            {
                recording_type$process_function(file_info)
            },
            error = function(e) {
                log_error(glue::glue("{file_info$filename} - Error in processing file: {e$message}"))
                return(NULL)
            }
        )
    }

    if (is.null(activity_data)) {
        log_warn(glue::glue("{file_info$filename} will not be handled."))
        return(invisible())
    }
    log_trace("{file_info$filename} - Clip data")
    # Clip to deployment and retrieval dates
    clipped_activity_data <- dplyr::filter(activity_data, as.Date(date_time) >= as.Date(file_info$deployment_date), as.Date(date_time) <= as.Date(file_info$retrieval_date))
    if (nrow(clipped_activity_data) == 0) {
        log_warn(glue::glue("{file_info$filename} has no data within the deployment and retrieval dates."))
        return(invisible())
    }

    clipped_activity_data <- tibble(session_id = file_info$session_id, filename = file_info$filename, individ_id = file_info$individ_id, clipped_activity_data)
    clipped_data_list <- list(act_data = clipped_activity_data)
    names(clipped_data_list)[names(clipped_data_list) == "act_data"] <- recording_type$argname
    # Push to database
    db_upload_success <- tryCatch(
        {
            do.call(seatrackR::writeRecordings, clipped_data_list)
            log_success(glue::glue("{file_info$filename} - uploaded to database"))
            return(TRUE)
        },
        error = function(e) {
            log_error(glue::glue("{file_info$filename} - Error in pushing to database: {e$message}"))
            return(FALSE)
        }
    )

    if (db_upload_success) {
        # Save raw file to archive
        seatrackR::uploadFiles(file_info$full_path)
        log_success(glue::glue("{file_info$filename} - uploaded to archive"))
    }
    return(invisible())
}
