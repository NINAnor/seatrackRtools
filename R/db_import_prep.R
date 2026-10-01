#' Prepare master sheet for database import
#'
#' This function prepares the master import sheet for database import by processing the metadata and startup/shutdown information.
#' It checks the database for existing sessions and categorizes them into sessions to be closed, opened, or both opened and closed.
#' It then prepares session batches for database import. Metadata is linked to sessions within these batches.
#' @param master_sheets A LoadedWBCollection object containing the master import sheets.
#' @return A vector of DBImportCollection objects representing the prepared session batches for database import.
#' @export
#' @concept db_import_prep
prepare_master_sheet_for_db <- function(master_sheets) {
    seatrackR:::checkCon()
    log_info_all(paste("Prepare", master_sheets$path, "for database upload"))
    metadata <- master_sheets$data$METADATA
    metadata <- metadata[order(metadata$date), ]

    # Temporarily bring back scull until we fix the name
    if("skull" %in% names(metadata)){
        metadata <- dplyr::rename(metadata, scull = skull)
    }

    # Check colony
    metadata <- check_metadata_colony(metadata)

    metadata$ring_number <- as.character(metadata$ring_number)

    startup_shutdown <- master_sheets$data$STARTUP_SHUTDOWN
    startup_shutdown <- startup_shutdown[order(startup_shutdown$starttime_gmt), ]

    # Check shutdowns
    startup_shutdown <- check_shutdown(startup_shutdown)
    check_startup_rows(startup_shutdown)

    # Check euring code
    metadata <- check_euring_code(metadata)

    # Check people
    startup_shutdown <- check_people_startup_shutdown(startup_shutdown)
    metadata <- check_people_metadata(metadata)

    # Check sex
    metadata <- check_sex_metadata(metadata)

    # Check sexing method

    # fix hatching success/breeding success
    metadata$hatching_success <- as.logical(metadata$hatching_success)
    metadata$breeding_success <- as.logical(metadata$breeding_success)
    metadata$chicks <- fix_num_col(metadata$chicks)
    metadata$eggs <- fix_num_col(metadata$eggs)

    # Fix lat/lon
    metadata <- fix_metadata_latlon(metadata)

    # check breeding stage
    metadata <- check_metadata_breeding_stage(metadata)

    # Fix morph
    metadata$tarsus <- fix_num_col(metadata$tarsus)
    metadata$scull <- fix_num_col(metadata$scull)
    metadata$weight <- fix_num_col(metadata$weight)
    metadata$wing <- fix_num_col(metadata$weight)

    # fix back_on_nest
    metadata <- fix_metadata_back_on_nest(metadata)

    # fix eggs
    metadata$eggs <- fix_num_col(metadata$eggs)

    # check mounting types
    metadata <- check_metadata_mounting(metadata)

    # check species
    startup_shutdown <- check_startup_species(startup_shutdown)
    check_startup_rows(startup_shutdown)

    metadata <- check_metadata_species(metadata)

    # Check species mismatch
    metadata <- check_metadata_species_match(metadata)

    # Check logger model ID
    logger_fix_result <- fix_logger_models(metadata, startup_shutdown)
    metadata <- logger_fix_result$metadata
    startup_shutdown <- logger_fix_result$startup_shutdown

    startup_shutdown <- check_startup_prod_year(startup_shutdown)

    # check download status
    startup_shutdown <- check_startup_download(startup_shutdown)
    check_startup_rows(startup_shutdown)

    # check retrieval type

    # Order startups
    startup_shutdown <- startup_shutdown[order(startup_shutdown$starttime_gmt), ]

    # combine download/shutdown
    startup_shutdown$download_date[is.na(startup_shutdown$download_date)] <- startup_shutdown$shutdown_date[is.na(startup_shutdown$download_date)]

    startup_shutdown_id <- paste(startup_shutdown$logger_serial_no, startup_shutdown$starttime_gmt)

    # check db for which sessions are already in the correct state
    shutdown_new <- check_shutdown_db(startup_shutdown)

    startup_new <- check_startups_db(startup_shutdown)

    # As the above only checks startups and shutdowns, we need to check for sessions that might have missing deployments or retrievals
    log_info("Check deployments")
    deployments_new <- prepare_session_deployments(startup_shutdown, metadata, TRUE, TRUE)
    missing_deployment_ids <- paste(deployments_new$sessions$logger_serial_no[!is.na(deployments_new$sessions$logger_serial_no)], deployments_new$sessions$starttime_gmt[!is.na(deployments_new$sessions$logger_serial_no)])

    log_info("Check retrievals")
    retrievals_new <- prepare_session_retrievals(startup_shutdown, metadata, TRUE, TRUE)
    missing_retrieval_ids <- paste(retrievals_new$sessions$logger_serial_no[!is.na(retrievals_new$sessions$logger_serial_no)], retrievals_new$sessions$starttime_gmt[!is.na(retrievals_new$sessions$logger_serial_no)])

    # which of our closed sessions are open sessions in the database vs. not existing at all in the database?
    db_shutdown_starts_df <-
        data.frame(
            session_id = paste(shutdown_new$logger_serial_no, as.Date(shutdown_new$starttime_gmt), sep = "_"),
            starttime_gmt = shutdown_new$starttime_gmt
        )
    closed_sessions_bool <- check_db_metadata_import(db_shutdown_starts_df, "loggers.startup")

    # rows of sessions that are currently open in the database and will only be closed
    db_to_close_only <- shutdown_new[!closed_sessions_bool, ]
    log_info(nrow(db_to_close_only), " sessions to close")

    # rows of sessions that currently don't exist in the database and therefore need to be started first before being closed.
    db_to_open_and_close <- shutdown_new[closed_sessions_bool, ]
    log_info(nrow(db_to_open_and_close), " sessions to open and close")

    # row of sessions that will be opened in the database and not closed
    db_to_open_only <- startup_new[is.na(startup_new$download_date), ]
    log_info(nrow(db_to_open_only), " sessions to open")

    # Combine all db IDs from these three dataframes
    all_db_ids <- paste(
        c(
            db_to_close_only$logger_serial_no,
            db_to_open_only$logger_serial_no,
            db_to_open_and_close$logger_serial_no
        ),
        c(
            db_to_close_only$starttime_gmt,
            db_to_open_only$starttime_gmt,
            db_to_open_and_close$starttime_gmt
        )
    )

    # Check which of the missing deployment/retrieval only sessions are still missing after accounting for sessions being opened/closed here
    still_missing_retrieval_ids <- missing_retrieval_ids[!missing_retrieval_ids %in% all_db_ids]
    still_missing_deployment_ids <- missing_deployment_ids[!missing_deployment_ids %in% all_db_ids]

    # Sort these by type
    retrieval_only_ids <- startup_shutdown_id[startup_shutdown_id %in% still_missing_retrieval_ids & !startup_shutdown_id %in% still_missing_deployment_ids]
    deployment_only_ids <- startup_shutdown_id[!startup_shutdown_id %in% still_missing_retrieval_ids & startup_shutdown_id %in% still_missing_deployment_ids]
    deployment_retrieval_ids <- startup_shutdown_id[startup_shutdown_id %in% still_missing_retrieval_ids & startup_shutdown_id %in% still_missing_deployment_ids]

    # reinsert these
    if (length(retrieval_only_ids) > 0) {
        log_info(glue::glue("Reinserting {length(retrieval_only_ids)} sessions due to missing retrievals"))
        db_to_close_only <- rbind(db_to_close_only, startup_shutdown[startup_shutdown_id %in% retrieval_only_ids, ])
    }
    if (length(deployment_only_ids) > 0) {
        log_info(glue::glue("Reinserting {length(deployment_only_ids)} sessions due to missing deployments"))
        db_to_open_only <- rbind(db_to_open_only, startup_shutdown[startup_shutdown_id %in% deployment_only_ids, ])
    }
    if (length(deployment_retrieval_ids) > 0) {
        log_info(glue::glue("Reinserting {length(deployment_retrieval_ids)} sessions due to missing deployments and retrievals"))
        db_to_open_and_close <- rbind(db_to_open_and_close, startup_shutdown[startup_shutdown_id %in% deployment_retrieval_ids, ])
    }

    # batch open closed sessions
    db_to_open_and_close_list <- list()
    db_to_open_and_close_list <- c(db_to_open_and_close_list, list(db_to_open_and_close))

    while (any(duplicated(paste(db_to_open_and_close_list[[length(db_to_open_and_close_list)]]$logger_serial_no, db_to_open_and_close_list[[length(db_to_open_and_close_list)]]$logger_model)))) {
        current_index <- length(db_to_open_and_close_list)
        current_df <- db_to_open_and_close_list[[length(db_to_open_and_close_list)]]
        first_df <- current_df[!duplicated(paste(current_df$logger_serial_no, current_df$logger_model)), ]
        new_df <- current_df[duplicated(paste(current_df$logger_serial_no, current_df$logger_model)), ]
        db_to_open_and_close_list[[current_index]] <- first_df
        db_to_open_and_close_list <- c(db_to_open_and_close_list, list(new_df))
    }

    db_to_open_and_close_sessions <- sapply(db_to_open_and_close_list, SessionBatch$new, type = "open_and_close")
    log_info("Split sessions to open and close into ", length(db_to_open_and_close_sessions), " sessions.")

    session_batches <- c(SessionBatch$new(db_to_close_only, "close_only"), db_to_open_and_close_sessions, SessionBatch$new(db_to_open_only, "open_only"))
    session_retrieval_deployments <- prepare_session_batches(session_batches, metadata)
    return(session_retrieval_deployments)
}

#' Prepare session batches for database import
#'
#' This function prepares a list of session batches for database import by processing each batch and extracting relevant metadata.
#' It filters out any batches that do not contain sessions and returns a list of prepared session batches.
#' It adds deployments and retrievals associated with the sessions in each batch.
#' In cases where sessions have multiple deployments or retrievals, those sessions are removed from the batch and a warning is logged.
#' @param session_batches A vector of SessionBatch objects to be prepared for database import.
#' @param metadata A dataframe containing metadata information for the sessions.
#' @return A vector of DBImportCollection objects representing the prepared session batches for database import.
#' @export
#' @concept db_import_prep
prepare_session_batches <- function(session_batches, metadata) {
    log_info("Preparing ", length(session_batches), " session batches for database import.")
    session_metadata_batches <- sapply(session_batches, prepare_session_batch, metadata = metadata)
    session_metadata_batches <- session_metadata_batches[which(!sapply(session_metadata_batches, is.null))]
    return(session_metadata_batches)
}

#' Prepare session batch for database import
#'
#' This function prepares a session batch for database import by processing each batch and extracting relevant metadata.
#' It adds deployments and retrievals associated with the sessions in each batch.
#' In cases where sessions have multiple deployments or retrievals, those sessions are removed from the batch and a warning is logged.
#' @param session_batch A SessionBatch object to be prepared for database import.
#' @param metadata A dataframe containing metadata information for the sessions.
#' @return DBImportCollection object representing the prepared session batches for database import.
#' @export
#' @concept db_import_prep
prepare_session_batch <- function(session_batch, metadata) {
    if (nrow(session_batch$sessions) == 0) {
        log_info(glue::glue("No sessions available. Sessions would be {gsub('_',' ', session_batch$type)}"))
        return(NULL)
    }
    original_count <- nrow(session_batch$sessions)
    log_info(glue::glue("Prepare {original_count}. Sessions will be {gsub('_',' ', session_batch$type)}"))

    deployment_results <- prepare_session_deployments(session_batch$sessions, metadata, duplicate_warnings = FALSE)
    deployments <- deployment_results$deployments
    session_batch$sessions <- deployment_results$sessions

    retrieval_results <- prepare_session_retrievals(session_batch$sessions, metadata, duplicate_warnings = FALSE)
    retrievals <- retrieval_results$retrievals
    session_batch$sessions <- retrieval_results$sessions

    # as we may have lost sessions, double check.
    deployment_results <- prepare_session_deployments(session_batch$sessions, metadata, duplicate_warnings = FALSE)
    deployments <- deployment_results$deployments
    session_batch$sessions <- deployment_results$sessions


    new_count <- nrow(session_batch$sessions)
    log_success(glue::glue("Removed {original_count - new_count} sessions. {new_count} sessions prepared.."))


    return(DBImportCollection$new(
        session_batch,
        retrievals,
        deployments
    ))
}

#' Get open session dates
#' Convenience function to get the open date for a set of logger sessions.
#' If the shutdown date is available, use that. If not, use the download date. If both exist, use the earlier date.
#' If there is another session from the same logger, use the start of that.
#' If neither are available, use a date far in the future to indicate the session is still open.
#' @param sessions A tibble containing session information from master import startup_shutdown.
#' @return A vector of POSIXct dates representing the open dates for the sessions.
#' @concept utility
get_open_session_dates <- function(sessions) {
    session_batch_open_date <- sessions$shutdown_date
    session_batch_open_date[is.na(session_batch_open_date)] <- sessions$download_date[is.na(session_batch_open_date)]
    earlier_download_bool <- (!is.na(sessions$shutdown_date)) & (!is.na(sessions$download_date)) & (sessions$shutdown_date > sessions$download_date)
    session_batch_open_date[earlier_download_bool] <- sessions$download_date[earlier_download_bool]

    inferred_close <- sapply(which(is.na(session_batch_open_date)), function(missing_idx) {
        logger_id <- sessions$logger_serial_no[missing_idx]
        start_time <- sessions$starttime_gmt[missing_idx]
        logger_model <- sessions$logger_model[missing_idx]
        # Find other instances of this logger ID that are not this start time
        other_sessions <- sessions[sessions$logger_serial_no %in% logger_id & sessions$logger_model == logger_model & sessions$starttime_gmt > start_time, ]
        if (nrow(other_sessions) == 0) {
            return(NA)
        }
        # find which other session start time is closes to start time
        time_diff <- other_sessions$starttime_gmt - start_time
        return(as.Date(other_sessions$starttime_gmt[time_diff == min(time_diff)][1]) - 1)
    })
    session_batch_open_date[is.na(session_batch_open_date)] <- inferred_close

    session_batch_open_date[is.na(session_batch_open_date)] <- sessions$starttime_gmt[is.na(session_batch_open_date)] + (100 * 60 * 60 * 24 * 365)
    return(session_batch_open_date)
}

#' Get session deployments
#'
#' Function to get deployments associated with a set of logger sessions. The function will look for deployments that occurred between the session start time and the session end time (download/shutdown).
#' If the session is open (no download/shutdown date), the function will use a date far in the future to indicate the session is still open.
#' If there are ambiguities (multiple deployments for a single session), those sessions will be removed and a warning will be logged.
#' @param sessions A tibble containing session information from master import startup_shutdown.
#' @param metadata A dataframe containing metadata information for the sessions.
#' @param filter_sessions Logical indicating whether to filter out sessions with no deployments.
#' @param report_missing Logical indicating whether to report deployments that are not associated with any sessions.
#' @param duplicate_warnings Logical indicating whether to report duplicate deployments
#' @return A list containing a dataframe of deployments and a tibble of sessions.
#' @concept db_import_prep
#' @export
prepare_session_deployments <- function(sessions, metadata, filter_sessions = FALSE, report_missing = FALSE, duplicate_warnings = TRUE) {
    session_batch_open_date <- get_open_session_dates(sessions)
    session_batch_start_date <- as.Date(sessions$programmed_gmt_time)
    session_batch_start_date[is.na(session_batch_start_date)] <- as.Date(sessions$starttime_gmt[is.na(session_batch_start_date)])
    # foreach session batch
    # find the deployments and retrievals for those loggers that are associated with these sessions
    # deployments

    original_idx <- seq_len(nrow(sessions))

    deployed_match_idx <- lapply(original_idx, function(session_row_idx) {
        session_row <- sessions[session_row_idx, ]
        session_open_date <- session_batch_open_date[session_row_idx]
        session_start_date <- session_batch_start_date[session_row_idx]
        match_idx <- which(metadata$date >= session_start_date &
            metadata$date < session_open_date &
            metadata$logger_id_deployed == session_row$logger_serial_no & metadata$logger_model_deployed == session_row$logger_model & !is.na(metadata$logger_id_deployed))
        return(match_idx)
    })

    n_matches <- sapply(deployed_match_idx, length)
    session_has_matches <- n_matches > 0
    duplicate_deployments <- n_matches > 1
    all_deployed_match_idx <- deployed_match_idx

    if (any(duplicate_deployments)) {
        duplicate_idx <- which(duplicate_deployments)
        if (duplicate_warnings) {
            log_warn(sum(duplicate_deployments), " sessions have multiple deployments within them")
            log_warn("These sessions will not be handled.")
            for (i in duplicate_idx) {
                session_summary <- sessions[i, c("logger_serial_no", "starttime_gmt", "download_date")]
                duplicate_logger <- session_summary$logger_serial_no
                log_warn(duplicate_logger, ":\n", paste(capture.output(print(session_summary, n = 1))[c(-1, -3)], collapse = "\n"))

                deployment_summary <- metadata[deployed_match_idx[[i]], c("date", "ring_number", "logger_id_retrieved", "logger_id_deployed", "comment")]
                log_warn("Deployments:\n", paste(capture.output(print(deployment_summary, n = nrow(deployment_summary)))[c(-1, -3)], collapse = "\n"))
            }
        }

        deployed_match_idx <- deployed_match_idx[!original_idx %in% duplicate_idx]
        sessions <- sessions[!original_idx %in% duplicate_idx, ]
        session_has_matches <- session_has_matches[!original_idx %in% duplicate_idx]
        original_idx <- original_idx[!original_idx %in% duplicate_idx]
    }

    if (report_missing) {
        flat_all_deployed_match_idx <- unlist(all_deployed_match_idx)
        orphaned_deployments <- metadata[(!seq_len(nrow(metadata)) %in% flat_all_deployed_match_idx) & (!is.na(metadata$logger_id_deployed)), ]
        if (nrow(orphaned_deployments) > 0) {
            orphan_deployment_summary <- orphaned_deployments[, c("date", "ring_number", "logger_id_deployed", "comment")]
            log_warn(glue::glue("Found {nrow(orphaned_deployments)}/{nrow(metadata[!is.na(metadata$logger_id_deployed),])} deployments that are not associated with any sessions."))
            log_warn("Orphaned deployments:\n", paste(capture.output(print(orphan_deployment_summary, n = nrow(orphan_deployment_summary)))[c(-1, -3)], collapse = "\n"))
            log_warn("This could be due to incorrect dates on the encounter or sessions, incorrect logger IDs or incorrect models.")
        }
    }

    flat_deployed_match_idx <- unlist(deployed_match_idx)
    deployments <- metadata[flat_deployed_match_idx, ]
    log_info(glue::glue("Found {nrow(deployments)} deployments associated with these sessions"))

    new_deployments <- check_deployment_db(deployments)
    # As we are handling retrievals and deployments seperately, remove deployment information
    new_deployments$logger_model_retrieved <- NA
    new_deployments$logger_id_retrieved <- NA
    log_info_all(glue::glue("Found {nrow(new_deployments)} deployments associated with these sessions and not present in database."))

    if (filter_sessions) {
        # Get original deployment IDs
        deployment_id <- paste(deployments$logger_id_deployed, deployments$date)
        # Get filtered retrieval IDs
        new_deployments_id <- paste(new_deployments$logger_id_deployed, new_deployments$date)

        sessions <- sessions[session_has_matches, ]
        sessions <- sessions[deployment_id %in% new_deployments_id, ]
    }

    return(list(deployments = new_deployments, sessions = sessions))
}


#' Get session retrievals
#'
#' Function to get retrievals associated with a set of logger sessions. The function will look for retrievals that occurred between the session start time and the session end time (download/shutdown).
#' If the session is open (no download/shutdown date), the function will use a date far in the future to indicate the session is still open.
#' If there are ambiguities (multiple retrievals for a single session), those sessions will be removed and a warning will be logged.
#' @param sessions A tibble containing session information from master import startup_shutdown.
#' @param metadata A dataframe containing metadata information for the sessions.
#' @param filter_sessions Logical indicating whether to filter out sessions with no retrievals.
#' @param report_missing Logical indicating whether to report retrievals that are not associated with any sessions.
#' @param duplicate_warnings Logical indicating whether to report duplicate retrievals
#' @return A list containing a dataframe of retrievals and a tibble of sessions.
#' @concept db_import_prep
#' @export
prepare_session_retrievals <- function(sessions, metadata, filter_sessions = FALSE, report_missing = FALSE, duplicate_warnings = TRUE) {
    session_batch_open_date <- get_open_session_dates(sessions)

    original_idx <- seq_len(nrow(sessions))
    retrieved_match_idx <- lapply(original_idx, function(session_row_idx) {
        session_row <- sessions[session_row_idx, ]
        session_open_date <- session_batch_open_date[session_row_idx]
        match_idx <- which(metadata$date > as.Date(session_row$starttime_gmt) &
            metadata$date <= session_open_date &
            metadata$logger_id_retrieved == session_row$logger_serial_no & metadata$logger_model_retrieved == session_row$logger_model & !is.na(metadata$logger_id_retrieved))
        return(match_idx)
    })

    n_matches <- sapply(retrieved_match_idx, length)
    session_has_matches <- n_matches > 0
    duplicate_retrievals <- n_matches > 1

    all_retrieved_match_idx <- retrieved_match_idx

    if (any(duplicate_retrievals)) {
        duplicate_idx <- which(duplicate_retrievals)
        if (duplicate_warnings) {
            log_warn(sum(duplicate_retrievals), " sessions have multiple retrievals within them")
            for (i in duplicate_idx) {
                session_summary <- sessions[i, c("logger_serial_no", "starttime_gmt", "download_date")]
                duplicate_logger <- session_summary$logger_serial_no
                log_warn(duplicate_logger, ":\n", paste(capture.output(print(session_summary, n = 1))[c(-1, -3)], collapse = "\n"))

                retrieval_summary <- metadata[retrieved_match_idx[[i]], c("date", "ring_number", "logger_id_retrieved", "logger_id_deployed", "comment")]
                log_warn("Retrievals:\n", paste(capture.output(print(retrieval_summary, n = nrow(retrieval_summary)))[c(-1, -3)], collapse = "\n"))
                log_warn("This session will not be handled.")
            }
        }
        retrieved_match_idx <- retrieved_match_idx[!original_idx %in% duplicate_idx]
        sessions <- sessions[!original_idx %in% duplicate_idx, ]
        session_has_matches <- session_has_matches[!original_idx %in% duplicate_idx]
        original_idx <- original_idx[!original_idx %in% duplicate_idx]
    }

    flat_retrieved_match_idx <- unlist(retrieved_match_idx)
    retrievals <- metadata[flat_retrieved_match_idx, ]

    if (report_missing) {
        flat_all_retrieved_match_idx <- unlist(all_retrieved_match_idx)
        orphaned_retrievals <- metadata[(!seq_len(nrow(metadata)) %in% flat_all_retrieved_match_idx) & (!is.na(metadata$logger_id_retrieved)), ]
        if (nrow(orphaned_retrievals) > 0) {
            orphan_retrieval_summary <- orphaned_retrievals[, c("date", "ring_number", "logger_id_retrieved", "comment")]
            log_warn(glue::glue("Found {nrow(orphaned_retrievals)}/{nrow(metadata[!is.na(metadata$logger_id_retrieved),])} retrievals that are not associated with any sessions."))
            log_warn(paste("Orphaned retrievals:\n", paste(capture.output(print(orphan_retrieval_summary, n = nrow(orphan_retrieval_summary)))[c(-1, -3)], collapse = "\n")))
            log_warn("This could be due to incorrect dates on the encounter or sessions, incorrect logger IDs or incorrect models.")
        }
    }

    log_info(glue::glue("Found {nrow(retrievals)} retrievals associated with these sessions"))

    new_retrievals <- check_retrieval_db(retrievals)

    # As we are handling retrievals and deployments seperately, remove deployment information
    new_retrievals$logger_model_deployed <- NA
    new_retrievals$logger_id_deployed <- NA
    log_info_all(glue::glue("Found {nrow(new_retrievals)} retrievals associated with these sessions and not present in database."))

    if (filter_sessions) {
        # Get original retrieval IDs
        retrieval_id <- paste(retrievals$logger_id_retrieved, retrievals$date)
        # Get filtered retrieval IDs
        new_retrievals_id <- paste(new_retrievals$logger_id_retrieved, new_retrievals$date)

        sessions <- sessions[session_has_matches, ]
        sessions <- sessions[retrieval_id %in% new_retrievals_id, ]
    }

    return(list(retrievals = new_retrievals, sessions = sessions))
}
