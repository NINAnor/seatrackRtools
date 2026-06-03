#' Get activity database sessions
#'
#' This function scans the import directory for activity data files, checks for corresponding sessions in the database, and identifies any missing files or sessions. It returns a list containing information about the available files, missing sessions, and missing raw data files for each recording type.
#' @param recording_types A list of recording types, where each recording type is a list containing the table name, processing function, file extensions, and argument name for the recording type.
#' @param import_directory The directory to scan for activity data files. Defaults to the "ALL" folder in the SeaTrack database imports directory.
#' @param compare_file_to_db A boolean indicating whether to compare the files in the import directory to the records in the database to identify files that may need to be re-uploaded. As this can be heavy, this defaults to FALSE.
#' @param min_date A string representing the minimum modification date for files to be considered, in the format "YYYY-MM-DD". Defaults to "2000-01-01". Strongly reccomended when compare_file_to_db is TRUE to avoid comparing a large number of files that have not been modified recently.
#' @return A list containing the following elements:
#' - file_info: A dataframe containing information about the available files in the import directory, including full path, logger ID, year downloaded, model, ID year, extension, and filename.
#' - missing_sessions: A tibble summarizing the missing sessions in the database based on the available files in the import directory, including logger ID, year downloaded, and model.
#' - missing_raw_data_files: A list of dataframes for each recording type, containing information about the missing raw data files for each session, including session ID, individ ID, filename, deployment date, and retrieval date.
#' - updated_raw_data_files: A list of dataframes for each recording type, containing information about the files that have been modified and may need to be re-uploaded, including session ID, individ ID, filename, deployment date, and retrieval date.
#' - missing_files: A list of dataframes for each recording type, containing information about the files that are expected to be in the database but are missing from both the database and the import directory, including session ID and filename.
#' @export
#' @concept activity_db_import
get_activity_db_sessions <- function(recording_types, import_directory = file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL"), compare_file_to_db = FALSE, min_date = "2000-01-01") {
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

    log_info("Scan import directory for files...")
    all_files <- list.files(import_directory, pattern = all_formats_pattern, recursive = TRUE, full.names = TRUE)
    file_info <- file.info(all_files, extra_cols = FALSE)
    all_files <- all_files[as.Date(file_info$mtime) >= as.Date(min_date)]
    if (length(all_files) == 0) {
        log_info("No files found in import directory after filtering by date.")
        return(list(file_info = data.frame(), missing_sessions = data.frame(), missing_raw_data_files = data.frame(), updated_raw_data_files = data.frame(), to_archive = data.frame()))
    } else if (compare_file_to_db && length(all_files) > 5000) {
        log_warn(glue::glue("There are {length(all_files)} files in the import directory that have been modified after {min_date}. Comparing files to database records may take a long time."))
    }

    all_files_split <- strsplit(basename(all_files), "_")
    file_info_list <- lapply(all_files_split, function(x) {
        data.frame(logger_id = x[1], year_downloaded = x[2], model = tools::file_path_sans_ext(paste(x[3:length(x)], collapse = "_")), id_year = paste(x[1], x[2], sep = "_"))
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
        "There are ", length(missing_files), " files in the import directory that do not have a corresponding session in the database. \n"
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

    # Files that are expected to be in the database, but have no data and are not present in the import folder
    missing_files <- sapply(recording_types, function(recording_type) {
        recording_table <- dplyr::tbl(con, dbplyr::in_schema("recordings", recording_type$table_name))
        current_formats <- recording_type$extensions
        recording_file_info <- file_info[paste0(".", file_info$extension) %in% current_formats, ]
        expected_db_files <- dplyr::anti_join(file_archive, dplyr::select(recording_table, session_id), by = "session_id") %>%
            dplyr::filter(file_basename %in% !!current_formats & !filename %in% !!recording_file_info$filename) %>%
            collect()
        return_list <- list(dplyr::collect(dplyr::select(expected_db_files, session_id, filename)))
    })


    if (compare_file_to_db) {
        log_info("Comparing files in import directory to database records...")

        updated_raw_data_files <- sapply(recording_types, function(recording_type) {
            recording_table <- dplyr::tbl(con, dbplyr::in_schema("recordings", recording_type$table_name))
            current_formats <- recording_type$extensions
            recording_file_info <- file_info[paste0(".", file_info$extension) %in% current_formats, ]

            existing_sessions <- db_sessions %>%
                dplyr::filter(filename %in% recording_file_info$filename) %>%
                dplyr::semi_join(recording_table, by = "session_id")

            existing_session_info <- dplyr::collect(dplyr::select(existing_sessions, session_id, individ_id, filename, deployment_date, retrieval_date, logger_serial_no))
            recording_files <- dplyr::inner_join(existing_session_info, recording_file_info, by = "filename", suffix = c("", "y"))

            # For each session - open the file and get the first row. Compare this to the first row in the database. If they are different, add to list of files to re-upload
            to_reupload <- list()
            if (nrow(recording_files) == 0) {
                return(list(recording_files))
            }
            for (i in 1:nrow(recording_files)) {
                log_info(glue::glue("Comparing file {i}/{nrow(recording_files)}  {recording_files$filename[i]} to database records..."))
                existing_session_i <- recording_files[i, ]

                # Load file
                clipped_activity_data <- load_activity(existing_session_i, recording_type)
                if (is.null(clipped_activity_data)) {
                    log_warn(glue::glue("{existing_session_i$filename} cannot be loaded. Skipping comparison for this file."))
                    next
                }
                file_time <- clipped_activity_data$date_time[1]

                # Get first row in activity table
                recording_table_i <- dplyr::filter(recording_table, filename == existing_session_i$filename) %>%
                    head(1) %>%
                    dplyr::collect()
                recording_table_i_filename <- dplyr::filter(recording_table, session_id == existing_session_i$session_id) %>%
                    dplyr::distinct(filename) %>%
                    dplyr::pull(filename)

                if (nrow(recording_table_i) == 0) {
                    log_info(glue::glue("{existing_session_i$filename} - filename differs from database recording table ({recording_table_i_filename}). Marking for re-upload."))
                    to_reupload <- c(to_reupload, list(existing_session_i))
                    next
                }

                db_time <- recording_table_i$date_time

                # If the timestamp of the first row differs
                if (db_time != file_time) {
                    log_info(glue::glue("{existing_session_i$filename} - timestamp in file ({file_time}) differs from timestamp in database ({db_time}). Marking for re-upload."))
                    to_reupload <- c(to_reupload, list(existing_session_i))
                }
            }

            to_reupload_df <- do.call(rbind, to_reupload)

            return_list <- list(dplyr::select(to_reupload_df, session_id, individ_id, filename, deployment_date, retrieval_date))
            return(return_list)
        })
    } else {
        updated_raw_data_files <- NULL
    }



    return(list(file_info = file_info, missing_sessions = missing_session_summary, missing_raw_data_files = missing_raw_data_files, updated_raw_data_files = updated_raw_data_files, missing_files = missing_files))
}

#' Push activity data to database
#'
#' This function scans the import directory for activity data files, checks for corresponding sessions in the database, processes the files, and uploads the data to the SEATRACK database. It also handles error checking and logs the progress of the upload.
#' The function is designed to be flexible and can handle different types of activity data (e.g., accelerometer, light, temperature) based on the file extensions and specified processing functions.
#' @param import_directory The directory to scan for activity data files. Defaults to the "ALL" folder in the SeaTrack database imports directory.
#' @param compare_file_to_db A boolean indicating whether to compare the files in the import directory to the records in the database to identify files that may need to be re-uploaded. As this can be heavy, this defaults to FALSE.
#' @param min_date A string representing the minimum modification date for files to be considered for upload, in the format "YYYY-MM-DD". Defaults to "2000-01-01". Strongly reccomended when compare_file_to_db is TRUE to avoid comparing a large number of files that have not been modified recently.
#' @return None. The function uploads the activity data to the SEATRACK database and logs the progress and any issues encountered during the process.
#' @export
#' @concept activity_db_import
push_db_activity <- function(import_directory = file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL"), compare_file_to_db = FALSE, min_date = "2000-01-01") {
    recording_types <- list(
        accelerometer = list(table_name = "accelerometer_raw", process_function = NULL, extensions = c(), argname = "accelerationData"),
        activity = list(table_name = "activity_raw", process_function = load_immersion_data, extensions = c(".deg", ".act"), argname = "activityData"),
        light = list(table_name = "light_raw", process_function = load_light_data, extensions = c(".lux", ".lig"), argname = "lightData"),
        temperature = list(table_name = "temperature_raw", process_function = load_temperature_data, extensions = c(".sst", ".tem"), argname = "temperatureData")
    )

    available_files <- get_activity_db_sessions(recording_types = recording_types, compare_file_to_db = compare_file_to_db, min_date = min_date)
    missing_raw_data_files <- available_files$missing_raw_data_files
    updated_raw_data_files <- available_files$updated_raw_data_files
    file_info <- available_files$file_info

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
            log_info("Handling ", recording_type_name, " file ", i, " of ", n_files, ": ", recording_files$filename[i])
            result <- handle_activity(recording_files[i, ], recording_type)
        }
        if (!is.null(updated_raw_data_files)) {
            recording_files <- updated_raw_data_files[[recording_type_name]]
            n_files <- nrow(recording_files)
            if (n_files == 0) {
                log_info("No files to be updated for recording type: ", recording_type_name)
                next
            }
            log_info("Updating ", n_files, " files for recording type: ", recording_type_name)
            recording_files <- dplyr::left_join(recording_files, file_info, by = "filename", suffix = c("", "y"))
            for (i in seq_len(n_files)) {
                log_info("Updating ", recording_type_name, " file ", i, " of ", n_files, ": ", recording_files$filename[i])
                result <- handle_activity(recording_files[i, ], recording_type, remove_existing = TRUE)
            }
        }
    }
    log_info("Uploading missing files to archive")
    # Upload missing files to archive
    archive_summary <- seatrackR::listFileArchive()
    to_archive <- file_info[file_info$filename %in% archive_summary$filesNotInArchive, ]

    seatrackR::uploadFiles(to_archive$full_path)
}

#' Load activity data from file
#'
#' This function loads activity data from a specified file based on the recording type. It uses the appropriate processing function for the recording type to read the data, clips the data to the deployment and retrieval dates, and returns the clipped activity data as a dataframe. If there is an error in processing the file or if there is no data within the deployment and retrieval dates, it logs a warning and returns NULL.
#' @param file_info A dataframe containing information about the file to be processed, including session_id, filename, individ_id, deployment_date, retrieval_date, full_path, and extension.
#' @param recording_type A list containing information about the recording type, including table_name, process_function, extensions, and argname.
#' @return A dataframe containing the clipped activity data for the specified file and recording type, or NULL if there was an error in processing the file or if there is no data within the deployment and retrieval dates.
#' @concept activity_db_import
load_activity <- function(file_info, recording_type) {
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
        log_warn(glue::glue("{file_info$filename} cannot be loaded."))
        return(invisible())
    }
    log_trace("{file_info$filename} - Clip data")
    # Clip to deployment and retrieval dates
    clipped_activity_data <- dplyr::filter(activity_data, as.Date(date_time) > as.Date(file_info$deployment_date), as.Date(date_time) < as.Date(file_info$retrieval_date))
    if (nrow(clipped_activity_data) == 0) {
        log_warn(glue::glue("{file_info$filename} has no data within the deployment and retrieval dates."))
        return(invisible())
    }
    clipped_activity_data <- tibble(session_id = file_info$session_id, filename = file_info$filename, individ_id = file_info$individ_id, clipped_activity_data)

    return(clipped_activity_data)
}

#' Handle activity data file
#'
#' This function processes a single activity data file based on the specified recording type. It reads the file, clips the data to the deployment and retrieval dates, and uploads the data to the SEATRACK database. It also handles error checking and logs the progress of the upload.
#' @param file_info A dataframe containing information about the file to be processed, including session_id, filename, individ_id, deployment_date, retrieval_date, full_path, and extension.
#' @param recording_type A list containing information about the recording type, including table_name, process_function, extensions, and argname.
#' @return None. The function uploads the processed activity data to the SEATRACK database and logs the progress and any issues encountered during the process.
#' @export
#' @concept activity_db_import
#' @details The function performs the following steps:
#' 1. Reads the activity data file using the specified processing function for the recording type
#' 2. Clips the data to the deployment and retrieval dates specified in the file_info
#' 3. Uploads the clipped data to the SEATRACK database using the appropriate table based on the recording type
#' 4. Logs the progress of the upload and any issues encountered during the process
#' 5. If the upload is successful, it also uploads the raw file to the archive
handle_activity <- function(file_info, recording_type, remove_existing = FALSE) {
    log_trace(glue::glue("{file_info$filename} - begin handling."))

    clipped_activity_data <- load_activity(file_info, recording_type)

    if (is.null(clipped_activity_data)) {
        log_warn(glue::glue("{file_info$filename} will not be handled."))
        return(invisible())
    }

    clipped_data_list <- list(act_data = clipped_activity_data)
    names(clipped_data_list)[names(clipped_data_list) == "act_data"] <- recording_type$argname
    if (remove_existing) {
        # Removerows from the corresponding activity table in the database for this session_id, to avoid duplicates when we push the new data
        recording_table <- dplyr::tbl(con, dbplyr::in_schema("recordings", recording_type$table_name))
        tryCatch(
            {
                DBI::dbExecute(con, glue::glue("DELETE FROM recordings.{recording_type$table_name} WHERE session_id = '{file_info$session_id}'"))
                log_info(glue::glue("{file_info$filename} - existing data removed from database for session_id {file_info$session_id}."))
            },
            error = function(e) {
                log_error(glue::glue("{file_info$filename} - Error in removing existing data from database: {e$message}"))
            }
        )
    }

    # Check if data for this session_id already exists in the database. If it does, do not upload data.
    recording_table <- dplyr::tbl(con, dbplyr::in_schema("recordings", recording_type$table_name))
    existing_data <- dplyr::filter(recording_table, session_id == file_info$session_id) %>%
        head(1) %>%
        dplyr::collect()
    if (nrow(existing_data) > 0) {
        log_warn(glue::glue("{file_info$filename} - data for session_id {file_info$session_id} already exists in database. Skipping upload."))
        return(invisible())
    }

    # Push to database
    db_upload_success <- tryCatch(
        {
            do.call(seatrackR::writeRecordings, clipped_data_list)
            log_success(glue::glue("{file_info$filename} - uploaded to database"))
            TRUE
        },
        error = function(e) {
            log_error(glue::glue("{file_info$filename} - Error in pushing to database: {e$message}"))
            return(FALSE)
        }
    )

    if (db_upload_success) {
        # Save raw file to archive
        tryCatch(
            {
                seatrackR::uploadFiles(file_info$full_path, overwrite = remove_existing)
                log_success(glue::glue("{file_info$filename} - uploaded to archive"))
            },
            error = function(e) {
                log_error(glue::glue("{file_info$filename} - Error in uploading to archive: {e$message}"))
            }
        )
    }
    return(invisible())
}
