#' Check existing rows in database given certain columns from a metadata dataframe
#'
#' This function checks for existing rows in a database table based on specified columns from a metadata dataframe.
#' It constructs a SQL query to compare the values in the dataframe with those in the database table. IF a row from the dataframe
#' does not exist in the database table based on the specified columns, it is marked as missing.
#'
#' @param metadata_df A dataframe containing metadata to be checked against the database.
#' @param table The name of the database table to check against.
#' @param col_names A vector of column names from the metadata dataframe to be used for comparison. If NULL, all columns are used.
#' @param db_col_names A vector of column names in the database table corresponding to `col_names`. If NULL, `col_names` are used.
#' @return A logical vector indicating which rows in the metadata dataframe do not exist in the database table.
#' @export
#' @concept db_import_utility
check_db_metadata_import <- function(metadata_df, table, col_names = NULL, db_col_names = NULL) {
    if (nrow(metadata_df) == 0) {
        return(logical(0))
    }
    existing_rows <- get_db_metadata_import(metadata_df, table, col_names, db_col_names)
    log_trace("Check existing rows in database using columns: ", paste(col_names, collapse = ", "), "... ", nrow(existing_rows), " existing rows found")

    existing_rows_combined <- sapply(seq_len(nrow(existing_rows)), function(row_idx) {
        paste(existing_rows[row_idx, ], collapse = " ")
    })
    metadata_df_combined <- sapply(seq_len(nrow(metadata_df)), function(row_idx) {
        paste(metadata_df[row_idx, ], collapse = " ")
    })
    missing_row_bool <- !metadata_df_combined %in% existing_rows_combined
    log_trace(sum(missing_row_bool), " rows are not in the database")

    return(missing_row_bool)
}

get_db_metadata_import <- function(metadata_df, table, col_names = NULL, db_col_names = NULL, additional_db_col_names = c()) {
    if (nrow(metadata_df) == 0) {
        stop("No data to check!")
    }
    metadata_df <- tibble(metadata_df)
    if (is.null(col_names)) {
        col_names <- names(metadata_df)
    }
    if (is.null(db_col_names)) {
        db_col_names <- col_names
    }
    if (length(db_col_names) != length(col_names)) {
        stop("`db_col_names`` must be equal in length to `col_names`")
    }
    # given a table
    res <- DBI::dbSendQuery(con, glue::glue("SELECT {paste(db_col_names, collapse = ',')} FROM {table} LIMIT 1"))
    column_info <- DBI::dbColumnInfo(res)
    DBI::dbClearResult(res)

    # check certain columns
    db_cols <- metadata_df[, col_names]

    temp_values <- seatrackR::R_df_to_db_values(db_cols, column_info$type)

    t_db_names <- paste0("t.", db_col_names)
    if (length(additional_db_col_names) > 0) {
        t_additional_db_names <- paste0("t.", additional_db_col_names)
    } else {
        t_additional_db_names <- c()
    }

    t_db_names_combined <- paste(c(t_db_names, t_additional_db_names), collapse = ",\n")
    mv_db_names <- paste0("mv.", db_col_names)
    mv_db_names_combined <- paste(mv_db_names, collapse = ",\n")

    match_names <- lapply(seq_along(t_db_names), function(name_idx) {
        glue::glue("({t_db_names[name_idx]} IS NOT DISTINCT FROM {mv_db_names[name_idx]})")
    })
    combined_match_names <- paste(match_names, collapse = " AND \n")

    combined_db_names <- paste(db_col_names, collapse = ", ")


    query <- glue::glue("
    WITH match_values ({combined_db_names}) AS (
        VALUES
            { temp_values }
    )
    SELECT
        {t_db_names_combined}
    FROM
        {table} t
        JOIN match_values mv ON
        {combined_match_names}
        ;
    ")

    existing_rows <- DBI::dbGetQuery(con, query)
    return(existing_rows)
}

#' Check existing active sessions in database
#'
#' Function to check which startups from the master import sheet correspond to active sessions in the database.
#' It constructs a dataframe of session IDs and uses the `check_db_metadata_import` function
#' to determine which startups are associated with active sessions.
#' @param startup_shutdown A tibble containing session information from master import startup_shutdown.
#' @param filter_startups A logical indicating whether to filter out startups associated with active sessions. Default is TRUE.
#' @return If filter_startups is TRUE, a tibble of startups that are associated with active sessions in the database. Otherwise
#' @concept db_import_utility
#' @export
check_startups_active_db <- function(startup_shutdown, filter_startups = FALSE) {
    if (nrow(startup_shutdown) == 0) {
        return(startup_shutdown)
    }

    # Get database logger IDs
    all_ids <- startup_shutdown$logger_serial_no

    db_logger_ids <- get_db_metadata_import(data.frame(
        logger_serial_no = all_ids
    ), table = "loggers.logger_info", additional_db_col_names = "logger_id")

    all_ids_db <- db_logger_ids$logger_id[match(all_ids, db_logger_ids$logger_serial_no)]

    # Check for an active session
    db_startup_df_active <- tibble(logger_id = all_ids_db[!is.na(all_ids_db)], active = TRUE)
    active_session_bool <- check_db_metadata_import(db_startup_df_active, "loggers.logging_session")

    # Log the removal of active sessions

    active_session_bool_all_ids <- active_session_bool[match(all_ids_db, all_ids_db[!is.na(all_ids_db)])]
    active_session_bool_all_ids[is.na(active_session_bool_all_ids)] <- TRUE

    active_summary <- startup_shutdown[!active_session_bool_all_ids, c("logger_serial_no", "starttime_gmt")]
    if (nrow(active_summary) > 0) {
        log_warn(nrow(active_summary), " startups for loggers already in active sessions in database.")
        if (filter_startups) {
            log_warn("Sessions being removed:")
        }
        log_warn("Sesssions :\n", paste(capture.output(print(active_summary, n = nrow(active_summary)))[c(-1, -3)], collapse = "\n"))
    }
    if (filter_startups) {
        startup_shutdown <- startup_shutdown[active_session_bool_all_ids, ]
    } 
            
    return(startup_shutdown)

}

#' Check existing startups in database
#'
#' Function to check which startups from the master import sheet are not already present in the database.
#' It constructs a dataframe of session IDs and start times, and uses the `check_db_metadata_import` function
#' to determine which startups are new and need to be added to the database.
#' @param startup_shutdown A tibble containing session information from master import startup_shutdown.
#' @return A tibble of startups that are not already present in the database.
#' @concept db_import_utility
#' @export
check_startups_db <- function(startup_shutdown) {
    if (nrow(startup_shutdown) == 0) {
        return(startup_shutdown)
    }

    db_startup_df <- data.frame(
        session_id = paste(startup_shutdown$logger_serial_no, as.Date(startup_shutdown$starttime_gmt), sep = "_"),
        starttime_gmt = startup_shutdown$starttime_gmt
    )

    sessions_to_startup_bool <- check_db_metadata_import(db_startup_df, "loggers.startup")
    startup_new <- startup_shutdown[sessions_to_startup_bool, ]
    return(startup_new)
}

#' Check existing shutdowns in database
#'
#' Function to check which shutdowns from the master import sheet are not already present in the database.
#' It constructs a dataframe of session IDs and download dates, and uses the `check_db_metadata_import` function
#' to determine which shutdowns are new and need to be added to the database.
#' @param startup_shutdown A tibble containing session information from master import startup_shutdown.
#' @return A tibble of shutdowns that are not already present in the database.
#' @concept db_import_utility
#' @export
check_shutdown_db <- function(startup_shutdown) {
    if (nrow(startup_shutdown) == 0) {
        return(startup_shutdown)
    }
    shutdown <- startup_shutdown[!is.na(startup_shutdown$download_date) | !is.na(startup_shutdown$shutdown_date), ]
    if (nrow(shutdown) == 0) {
        return(shutdown)
    }
# # First check for an active session
# db_shutdown_df <-
#     data.frame(
#         session_id = paste(shutdown$logger_serial_no, as.Date(shutdown$starttime_gmt), sep = "_"), active = TRUE
#     )
# closed_session_bool <- check_db_metadata_import(db_shutdown_df, "loggers.logging_session")

# get_db_metadata_import(db_shutdown_df, "loggers.logging_session")

    # shutdown_new <- shutdown[!closed_session_bool, ]
    shutdown_new <- shutdown
    db_shutdown_df <-
        data.frame(
            session_id = paste(shutdown_new$logger_serial_no, as.Date(shutdown_new$starttime_gmt), sep = "_"),
            download_date = shutdown_new$download_date
        )

    db_shutdown_df$download_date[is.na(db_shutdown_df$download_date)] <- shutdown_new$shutdown_date[is.na(db_shutdown_df$download_date)]

    sessions_to_shutdown_bool <- check_db_metadata_import(db_shutdown_df, "loggers.shutdown")
    shutdown_new <- shutdown_new[sessions_to_shutdown_bool, ]
    # Check also the shutdown date, as the download date can differ from the database entry
    db_shutdown_df <-
        data.frame(
            session_id = paste(shutdown_new$logger_serial_no, as.Date(shutdown_new$starttime_gmt), sep = "_"),
            shutdown_date = shutdown_new$shutdown_date
        )
    db_shutdown_df <- db_shutdown_df[!is.na(db_shutdown_df$shutdown_date), ]
    sessions_to_shutdown_bool <- rep(TRUE, nrow(shutdown_new))

    check_result <- check_db_metadata_import(db_shutdown_df, "loggers.shutdown")
    sessions_to_shutdown_bool[!is.na(shutdown_new$shutdown_date)] <- check_result
    shutdown_new <- shutdown_new[sessions_to_shutdown_bool, ]

    return(shutdown_new)
}

#' Check existing retrievals in database
#'
#' Function to check which retrievals from the master import sheet are not already present in the database.
#' It constructs a dataframe of retrieval dates and individ IDs, and uses the `check_db_metadata_import` function
#' to determine which retrievals are new and need to be added to the database.
#' @param retrievals A tibble containing retrieval information from master import metadata.
#' @return A tibble of retrievals that are not already present in the database.
#' @concept db_import_utility
#' @export
check_retrieval_db <- function(retrievals) {
    if (nrow(retrievals) == 0) {
        return(retrievals)
    }
    db_retrievals_df <- tibble(
        retrieval_date = retrievals$date,
        individ_id = paste(retrievals$euring_code, retrievals$ring_number, sep = "_")
    )
    new_rows_bool <- check_db_metadata_import(db_retrievals_df, "loggers.retrieval")
    new_retrievals <- retrievals[new_rows_bool, ]
    return(new_retrievals)
}

#' Check existing deployments in database
#'
#' Function to check which deployments from the master import sheet are not already present in the database.
#' It constructs a dataframe of deployment dates and individ IDs, and uses the `check_db_metadata_import` function
#' to determine which deployments are new and need to be added to the database.
#' @param deployments A tibble containing deployment information from master import metadata.
#' @return A tibble of deployments that are not already present in the database.
#' @concept db_import_utility
#' @export
check_deployment_db <- function(deployments) {
    if (nrow(deployments) == 0) {
        return(deployments)
    }
    db_deployments_df <- tibble(
        deployment_date = deployments$date,
        individ_id = paste(deployments$euring_code, deployments$ring_number, sep = "_")
    )
    new_rows_bool <- check_db_metadata_import(db_deployments_df, "loggers.deployment")
    new_deployments <- deployments[new_rows_bool, ]
    return(new_deployments)
}
