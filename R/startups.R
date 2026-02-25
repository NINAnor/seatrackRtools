#' Get paths to all startup Excel files
#'
#' This function retrieves paths to all Excel files in the "Starttime files and stored loggers" subdirectory of the sea track folder.
#'
#' @return A character vector containing paths to all Excel files in the specified subdirectory.
#' @examples
#' \dontrun{
#' all_xlsx_files <- get_startup_paths()
#' }
#' @export
#' @concept startups
get_startup_paths <- function() {
    if (is.null(the$sea_track_folder)) {
        stop("Sea track folder is not set. Please use set_sea_track_folder() to set it.")
    }
    start_time_path <- file.path(the$sea_track_folder, "Starttime files and stored loggers")
    subfolders <- rev(list.dirs(start_time_path, full.names = TRUE, recursive = FALSE))
    ignored_folders <- c("starttimes for other projects")
    for (ignored_folder in ignored_folders) {
        subfolders <- subfolders[!grepl(ignored_folder, subfolders, fixed = TRUE)]
    }
    all_xlsx_list <- lapply(subfolders, function(folder) {
        files <- list.files(folder, pattern = "^[^~].*\\.xls", full.names = TRUE)
        return(files)
    })
    all_xlsx_list <- unlist(all_xlsx_list)
    return(all_xlsx_list)
}

#' Attempt to add logger from startup sheets
#'
#' This function attempts to add a logger to the master startup data frame from the startup sheets.
#' Because the data quality of older startup sheets is variable, the function checks for column mismatches and skips these files.
#' Incorrectly formatted datetime columns can also lead to issues.
#'
#' @param master_import Loaded Master startup file.
#' @param partner_metadata Dataframe of loggers handled by partners
#'
#' @return A new version of the master startup data frame, with the logger added if succesful.
#' @examples
#' \dontrun{
#' updated_master_startup <- add_loggers_from_startup_sheets(master_startup)
#' }
#' @export
#' @concept startups
add_loggers_from_startup <- function(master_import, new_metadata) {
    # Should rename this as it is now any model that is not vital to have a correct start time
    gps_models <- c("W30A9-SEA", "PicoFix_GEO_mini3", "PicoFix_GEO_mini2", "PicoFix_GEO_mini2", "NanoFix_GEO_GPS", "picoFix_GEO_mini3", "OrniTrack15", "OrniTrack10", "Ornitrack-15", "Ornitrack10") # Should be in database


    db_models <- tryCatch(
        {
            models <- seatrackR::getLoggerModels()
            models$model[!c(models$producer %in% c("Lotek", "BAS", "Biotrack"))]
        },
        error = function(e) {
            log_warn("No database connection from which to get models")
            return(c())
        }
    )
    gps_models <- c(gps_models, db_models)

    partner_metadata <- new_metadata$data$`ENCOUNTER DATA`
    partner_restarts <- new_metadata$data$`RESTART TIMES`

    master_startup <- master_import$data$`STARTUP_SHUTDOWN`
    master_metadata <- master_import$data$METADATA
    partner_logger_data_retrieved <- partner_metadata[
        !is.na(partner_metadata$logger_id_retrieved),
        c("date", "logger_id_retrieved", "logger_model_retrieved")
    ]
    names(partner_logger_data_retrieved) <- c("date", "logger_id", "model")
    partner_logger_data_retrieved$deployed <- FALSE
    partner_logger_data_deployed <- partner_metadata[
        !is.na(partner_metadata$logger_id_deployed),
        c("date", "logger_id_deployed", "logger_model_deployed")
    ]
    names(partner_logger_data_deployed) <- c("date", "logger_id", "model")
    partner_logger_data_deployed$deployed <- TRUE
    partner_logger_data <- rbind(partner_logger_data_deployed, partner_logger_data_retrieved)

    partner_logger_ids <- unique(partner_logger_data$logger_id)



    # Force imported classes
    master_classes <- sapply(master_startup, function(variable) paste(class(variable), collapse = "/"))
    excel_classes <- master_classes
    excel_classes[master_classes == "POSIXct/POSIXt"] <- 3
    excel_classes[master_classes == "Date"] <- 2
    excel_classes[master_classes == "character"] <- 0
    excel_classes[master_classes == "numeric"] <- 1
    excel_classes[master_classes == "logical"] <- 4
    excel_classes_numeric <- as.numeric(excel_classes)
    names(excel_classes_numeric) <- names(excel_classes)

    log_trace("Checking for new loggers in startup files")
    master_logger_id_date <- paste(master_startup$logger_serial_no, as.character(master_startup$starttime_gmt))
    startup_paths <- get_startup_paths()
    all_startups <- tibble()
    for (startup_path in startup_paths) {
        log_trace("Processing startup file: ", startup_path)

        # Peek at excel file to determine column number
        startup_file <- tryCatch(
            suppressWarnings(openxlsx2::read_xlsx(startup_path, rows = c(1))),
            error = function(e) {
                log_trace(paste("Unable to import:", startup_path, e))
                return(NULL)
            }
        )
        if (is.null(startup_file)) {
            next
        }

        if (ncol(startup_file) != ncol(master_startup)) {
            log_trace(paste("Skipping startup file due to column number mismatch:", startup_path))
            next
        }

        # Peeking at the excel files can miss empty columns created further down.
        # Import is wrapped in a try catch to handle import failures.
        startup_file <- tryCatch(
            suppressWarnings(openxlsx2::read_xlsx(startup_path, types = excel_classes_numeric)),
            error = function(e) {
                log_trace(paste("Unable to import:", startup_path, e))
                return(NULL)
            }
        )
        if (is.null(startup_file)) {
            next
        }
        # log_warn(paste(names(warnings()), collapse = "\n"))

        # Skip files whose columns do not match master_startup
        if (!all(colnames(master_startup) %in% colnames(startup_file))) {
            log_trace(paste("Skipping startup file due to column mismatch:", startup_path))
            next
        }

        # As I now force the column types based on the master file, this snippet is not needed.
        # class_match = sapply(1:ncol(master_startup), function(i) {
        #     # Ignore cases where this column is all NA in the file - this means the excel import will not have guessed the class correctly
        #     if (all(is.na(startup_file[[i]])) | all(is.na(master_startup[[i]]))) {
        #         return(TRUE)
        #     }
        #     return(all(class(startup_file[[i]]) == class(master_startup[[i]])))
        # })

        # if (any(!class_match)) {
        #     mismatched_cols <- colnames(startup_file)[!class_match]
        #     master_class <- sapply(which(!class_match), function(i) paste(class(master_startup[[i]]), collapse = "/"))
        #     startup_class <- sapply(which(!class_match), function(i) paste(class(startup_file[[i]]), collapse = "/"))
        #     mismatch_summary <- tibble::tibble(Column = mismatched_cols, Master_Class = master_class, Startup_Class = startup_class)
        #     log_warn(paste("Skipping startup file:",startup_path," due to class mismatch in columns:\n", paste(capture.output(print(mismatch_summary, n = nrow(mismatch_summary)))[c(-1, -3)], collapse = "\n")))
        #     next
        # }

        startup_file <- tibble(startup_file)
        startup_file <- dplyr::mutate(startup_file, dplyr::across(dplyr::where(is.character), stringr::str_trim))

        # Filter to only include rows where the logger has been handled
        startup_file <- startup_file[
            !is.na(startup_file$logger_serial_no) &
                startup_file$logger_serial_no %in% partner_logger_ids,
        ]

        if (nrow(startup_file) == 0) {
            next
        }
        startup_logger_id_date <- paste(startup_file$logger_serial_no, as.character(startup_file$starttime_gmt))
        # get logger ID/date combinations that do not appear in master_startup
        new_logger_indices <- which(!startup_logger_id_date %in% master_logger_id_date)
        if (length(new_logger_indices) > 0) {
            new_loggers <- startup_file[new_logger_indices, ]

            for (date_index in which(master_classes == "Date")) {
                new_loggers[, date_index] <- sapply(new_loggers[, date_index], as.Date)
            }

            all_startups <- rbind(all_startups, new_loggers)
        }
    }

    # Generate startups from restarts?


    new_loggers <- tibble()
    for (logger_id in partner_logger_ids) {
        logger_partner_logger_data <- partner_logger_data[partner_logger_data$logger_id == logger_id, ]
        logger_partner_logger_data <- logger_partner_logger_data[1, ] # loggers deployed or retrieved multiple times should be handled by restart
        logger_model <- logger_partner_logger_data$model
        if (nrow(all_startups) > 0) {
            startup_rows <- all_startups[all_startups$logger_serial_no == logger_id, ]
        } else {
            startup_rows <- tibble()
        }

        # Find the required deployment date
        deployment_date <- logger_partner_logger_data$date[logger_partner_logger_data$deployed]
        if (length(deployment_date) == 0) {
            # check master metadata
            deployment_date <- master_metadata$date[master_metadata$logger_id_deployed == logger_id]
            deployment_date <- deployment_date[length(deployment_date)]
        }

        # Check for existing sessions first
        if (nrow(startup_rows) == 0||all(is.na(startup_rows$starttime_gmt))) {
            critical <- check_critical_missing(startup_rows, logger_partner_logger_data, master_startup, logger_id, partner_restarts)
            if (!critical) {
                next
            }
        }

        can_dummy <- tolower(gsub("[^a-zA-Z0-9]", "", logger_model)) %in% tolower(gsub("[^a-zA-Z0-9]", "", gps_models))

        if (can_dummy && ((nrow(startup_rows) == 0) || (all(is.na(startup_rows$starttime_gmt))))) {
            if (!length(deployment_date) == 0 && !is.na(deployment_date)) {
                log_trace("Create dummy start time")
                dummy_start <- as.POSIXct(deployment_date, tz = "GMT")
                if (nrow(startup_rows) > 0) {
                    startup_row <- startup_rows[1, ]
                    startup_row$starttime_gmt <- dummy_start
                    new_loggers <- rbind(new_loggers, startup_row)
                    log_warn(glue::glue("Created dummy start time for {logger_id}, using information in start up sheets."))
                    next
                } else {
                    # Check if a session exists already in the master startup

                    existing_startup_idx <- which(master_startup$logger_serial_no == logger_id & is.na(master_startup$starttime_gmt))
                    # try and get logger information from database

                    logger_info <- tryCatch(
                        {
                            logger_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
                            logger_info <- dplyr::filter(logger_info, logger_serial_no == !!logger_id)
                            if (nrow(logger_info) == 1) {
                                logger_info <- logger_info[, 1]
                                log_trace(paste("Got logger info from database for logger", logger_id, ":", logger_model))
                                logger_info
                            } else {
                                log_trace(paste("No logger information/ambiguous logger information found in database for logger", logger_id))
                                NULL
                            }
                        },
                        error = function(e) {
                            log_trace(paste("Unable to get logger information from database for logger", logger_id, ":", e))
                            return(NULL)
                        }
                    )
                    if (is.null(logger_info)) {
                        logger_info <- list(logger_model = logger_model, producer = NA, production_year = NA, project = NA)
                    }

                    if (length(existing_startup_idx) == 0) {
                        startup_row <- tibble(
                            logger_serial_no = logger_id,
                            logger_model = logger_info$logger_model,
                            producer = logger_info$producer,
                            production_year = logger_info$production_year,
                            project = logger_info$project,
                            starttime_gmt = dummy_start,
                            logging_mode = NA,
                            started_by = NA,
                            started_where = NA,
                            days_delayed = NA,
                            programmed_gmt_time = NA,
                            intended_species = NA,
                            intended_location = NA,
                            intended_deployer = NA,
                            shutdown_session = NA,
                            field_status = NA,
                            downloaded_by = NA,
                            download_type = NA,
                            download_date = NA,
                            decomissioned = NA,
                            shutdown_date = NA,
                            comment = "dummy start time"
                        )
                        log_warn(glue::glue("Created dummy start time for {logger_id}. Note that some mandatory values will still need to be filled in."))
                        new_loggers <- rbind(new_loggers, startup_row)
                        next
                    } else {
                        log_warn("Logger already existing master startup, but with no start time")
                    }
                }
            } else {
                log_warn("No deployment date, cannot create dummy start time")
            }
        }

        # If no start up times were found at all
        if (nrow(startup_rows) == 0) {
            check_critical_missing(startup_rows, logger_partner_logger_data, master_startup, logger_id, partner_restarts)
            next
        }

        # If a start up time that is greater than this already exists in the startup sheet, we should not add this - check db too?
        existing_startups <- master_startup$starttime_gmt[master_startup$logger_serial_no == logger_id]
        if (length(existing_startups) > 0) {
            if (all(is.na(existing_startups))) {
                # have to consider reuse of GPS loggers at some point - probably check if that session is closed.
                next
            }
            startup_rows <- startup_rows[!is.na(startup_rows$starttime_gmt) & startup_rows$starttime_gmt > max(existing_startups), ]
            if (nrow(startup_rows) == 0) {
                check_critical_missing(startup_rows, logger_partner_logger_data, master_startup, logger_id, partner_restarts)
                next
            }
        }



        # Check if the start time is sensible
        if (!(logger_model %in% gps_models) && any(!is.na(startup_rows$starttime_gmt)) && !length(deployment_date) == 0 && !is.na(deployment_date)) {
            if (any(!is.na(startup_rows$programmed_gmt_time))) {
                start_time <- startup_rows$programmed_gmt_time
            } else {
                start_time <- startup_rows$starttime_gmt
            }

            startup_rows <- startup_rows[as.Date(start_time) <= deployment_date & as.Date(start_time) >= (deployment_date - (6 * 30)) & !is.na(start_time), ]
            if (nrow(startup_rows) == 0) {
                critical <- check_critical_missing(startup_rows, logger_partner_logger_data, master_startup, logger_id, partner_restarts)
                if (critical) {
                    log_warn(paste("No suitable start up time found for logger ID:", logger_id, "deployed on", deployment_date))
                }
                next
            }
        }

        if (nrow(startup_rows) == 1) {
            new_loggers <- rbind(new_loggers, startup_rows)
            next
        }

        # filter by intended location
        if (any(partner_metadata$colony %in% startup_rows$intended_location)) {
            startup_rows <- startup_rows[startup_rows$intended_location %in% partner_metadata$colony, ]
        } else {
            log_warn(paste("Could not resolve multiple startups for logger ID:", logger_id, "using intended location"))
        }

        if (nrow(startup_rows) == 1) {
            new_loggers <- rbind(new_loggers, startup_rows)
            next
        }

        # # use recovery date as deployment date
        # if (length(deployment_date) == 0 || is.na(deployment_date)) {
        #     deployment_date <- partner_logger_data$date[!partner_logger_data$deployed & partner_logger_data$logger_id == logger_id]
        # }

        if (length(deployment_date) == 0 || is.na(deployment_date)) {
            # Could try and look at intended location?

            critical <- check_critical_missing(startup_rows, logger_partner_logger_data, master_startup, logger_id, partner_restarts)

            log_warn(paste("Could not resolve multiple startups for logger ID:", logger_id, "due to lack of a deployment date"))

            next
        }

        # make sure we only consider dates in the past
        startup_rows <- startup_rows[as.Date(startup_rows$starttime_gmt) <= deployment_date & !is.na(as.Date(startup_rows$starttime_gmt)), ]

        if (nrow(startup_rows) == 1) {
            new_loggers <- rbind(new_loggers, startup_rows)
            next
        } else if (nrow(startup_rows) == 0) {
            critical <- check_critical_missing(startup_rows, logger_partner_logger_data, master_startup, logger_id, partner_restarts)
            if (critical) {
                log_warn(paste("No time found for logger ID:", logger_id, "deployed on", deployment_date))
            }
            next
        }



        # For each startup row, calculate the difference in the deployment date and the startup time
        time_diffs <- difftime(deployment_date, startup_rows$starttime_gmt, units = "days")
        startup_row <- startup_rows[which(time_diffs == min(time_diffs))[1], ]
        new_loggers <- rbind(new_loggers, startup_row)
    }
    # For each logger ID,
    # If there are multiple instances of that logger being started,
    # Try to find the deployment date.
    # Try to find the startup time closest to that date

    n_loggers <- nrow(new_loggers)
    log_success("Adding ", n_loggers, " new loggers from startup files")
    if (n_loggers > 0) {
        logger_summary <- new_loggers[, c("logger_serial_no", "logger_model", "starttime_gmt", "intended_location")]
        log_success("New loggers:\n", paste(capture.output(print(logger_summary, n = n_loggers))[c(-1, -3)], collapse = "\n"))

        master_startup <- rbind(master_startup, new_loggers)
    }
    # master_logger_id_date <- paste(master_startup$logger_serial_no, as.character(master_startup$starttime_gmt))
    return(master_startup)
}

check_critical_missing <- function(startup_rows, logger_partner_logger_data, master_startup, logger_id, partner_restarts) {
    if (nrow(startup_rows) == 0) {
        sessions <- master_startup[master_startup$logger_serial_no == logger_id, ]
        event_type <- ifelse(any(logger_partner_logger_data$deployed), "deployed", "retrieved")
        if (nrow(sessions) > 0) {
            sessions$end_date <- sessions$shutdown_date
            sessions$end_date[is.na(sessions$end_date)] <- sessions$download_date[is.na(sessions$end_date)]

            valid_open_sessions <- sessions[!is.na(sessions$starttime_gmt) & !is.na(logger_partner_logger_data$date) & logger_partner_logger_data$date >= sessions$starttime_gmt & is.na(sessions$end_date), ]
            valid_closed_sessions <- sessions[!is.na(sessions$starttime_gmt) & !is.na(logger_partner_logger_data$date) & logger_partner_logger_data$date >= sessions$starttime_gmt & logger_partner_logger_data$date <= sessions$end_date & !is.na(sessions$end_date), ]
            if (any(logger_partner_logger_data$deployed)) {
                valid_open_sessions <- valid_open_sessions[logger_partner_logger_data$date < (as.Date(valid_open_sessions$starttime_gmt) + (6 * 30)), ]
            }

            if (nrow(valid_closed_sessions) > 0) {
                # If the event falls in an existing closed session, this is fine
                return(FALSE)
            } else if (nrow(valid_open_sessions > 0)) {
                # If the event falls in an open session...
                # Ideally only one
                if (nrow(valid_open_sessions == 1)) {
                    # If this is a deployment, this should be fine due to the time check
                    if (!any(logger_partner_logger_data$deployed)) {
                        # Otherwise, there is no guaruntee this is the correct session
                        log_info(paste(
                            "Logger ID", logger_id, "was retrieved on", logger_partner_logger_data$date, "but no startup was added. \n",
                            "This falls into a single open session started on", str(valid_open_sessions$starttime_gmt), ". The retrieval may belong to this open session"
                        ))
                        return(FALSE)
                    } else {
                        log_info(paste("Logger ID", logger_id, "was", event_type, "on", logger_partner_logger_data$date, "but no startup was added. This falls inside a suitable open sessions."))
                    }
                    return(FALSE)
                }
            } else {
                log_warn(paste("Logger ID", logger_id, "was", event_type, "on", logger_partner_logger_data$date, "but no startup was added and no suitable existing sessions were found."))
                return(TRUE)
            }
        } else {
            log_warn(paste("Logger ID", logger_id, "was", event_type, "on", logger_partner_logger_data$date, "but no startup was added and there are no existing sessions for this logger."))
            return(TRUE)
        }
    }
    return(FALSE)
}

#' Set a value in a specific cell of master startup
#'
#' This function updates the value of a specified cell in the `master_startup` data frame.
#'
#' @param master_startup Master starup tibble.
#' @param index Integer. The row index of the cell to update.
#' @param column Character or integer. The column name or index of the cell to update.
#' @param value The new value to assign to the specified cell.
#'
#' @return The updated `master_startup` data frame.
#' @examples
#' \dontrun{
#' set_master_startup_value(master_startup, 2, "download_type", "Succesfully downloaded")
#' }
#' @export
#' @concept startups
set_master_startup_value <- function(master_startup, index, column, value) {
    master_startup[index, column] <- value
    log_trace(paste0("Set value in master_startup: row ", index, ", column '", column, "' to '", value, "'"))
    return(master_startup)
}


#' Set or append comments in the master_startup data frame
#'
#' This function updates the 'comment' field of the specified row in the master_startup data frame.
#' If a non-empty logger comment is provided, it will be set as the comment if no existing comment is present.
#' If an existing comment is present, the logger comment will be appended to it, separated by " | ".
#'
#' @param master_startup A data frame containing a 'comment' column to be updated.
#' @param index Integer index specifying the row in master_startup to update.
#' @param logger_comments A character string containing the comment to add or append.
#'
#' @return The updated master_startup data frame with the modified comment.
#' @examples
#' \dontrun{
#' master_startup <- data.frame(comment = c("", "Existing comment"))
#' set_comments(master_startup, 1, "New logger comment")
#' set_comments(master_startup, 2, "Another logger comment")
#' }
#' @export
#' @concept startups
set_comments <- function(master_startup, index, logger_comments) {
    if (!is.null(logger_comments) && !is.na(logger_comments) && logger_comments != "") {
        # If there is a comment:
        if (is.na(master_startup$comment[index]) || master_startup$comment[index] == "") {
            # If there is no existing comment:
            master_startup$comment[index] <- logger_comments
        } else {
            # If there is an existing comment, append to it:
            master_startup$comment[index] <- paste(master_startup$comment[index], logger_comments, sep = " | ")
        }
    }
    return(master_startup)
}
