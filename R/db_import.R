push_master_import_file_to_db <- function(location) {
    master_sheets <- load_master_import(location)
    push_master_sheet_to_db(master_sheets)
}

#push_master_sheet_to_db <- function(master_sheets) {

check_startups_db <- function(startup_shutdown){
    db_startup_df <-    data.frame(
        session_id = paste(startup_shutdown$logger_serial_no,as.Date(startup_shutdown$starttime_gmt), sep ="_"),
        starttime_gmt = startup_shutdown$starttime_gmt)

    sessions_to_startup_bool <- check_db_metadata_import(db_startup_df, "loggers.startup")
    startup_new <- startup_shutdown[sessions_to_startup_bool,]
    return(startup_new)
}

check_shutdown_db <- function(startup_shutdown){
    shutdown <- startup_shutdown[!is.na(startup_shutdown$download_date)|!is.na(startup_shutdown$shutdown_date), ]
    db_shutdown_df <- 
    data.frame(
        session_id = paste(shutdown$logger_serial_no,as.Date(shutdown$starttime_gmt), sep ="_"),
        download_date = shutdown$download_date)
    
    db_shutdown_df$download_date[is.na(db_shutdown_df$download_date)] <- shutdown$shutdown_date[is.na(db_shutdown_df$download_date)]

    sessions_to_shutdown_bool <- check_db_metadata_import(db_shutdown_df, "loggers.shutdown")
    shutdown_new <- shutdown[sessions_to_shutdown_bool,]
    return(shutdown_new)
}


prepare_master_sheet_for_db <- function(master_sheets) {
    import_fields <- DBI::dbListFields(con, DBI::Id(schema = "imports", table = "metadata_import"))

    metadata <- master_sheets$data$METADATA
    startup_shutdown <- master_sheets$data$STARTUP_SHUTDOWN

    # check db for which sessions are already in the corretct state
    shutdown_new <- check_shutdown_db(startup_shutdown)

    startup_new <- check_startups_db(startup_shutdown)

    # which of our closeed sessions are open sessions in the database vs. not existing at all in the database?
    db_shutdown_starts_df <- 
    data.frame(
        session_id = paste(shutdown_new$logger_serial_no,as.Date(shutdown_new$starttime_gmt), sep ="_"),
        starttime_gmt = shutdown_new$starttime_gmt)
    closed_sessions_bool <- check_db_metadata_import(db_shutdown_starts_df, "loggers.startup")
    
    # rows of sessions that are currently open in the database and will only be closed
    db_to_close_only <- shutdown_new[!closed_sessions_bool,]

    # rows of sessions that currently don't exist in the database and therefore need to be started first before being closed.
    db_to_open_and_close <- shutdown_new[closed_sessions_bool,]

    # row of sessions that will be opened in the database and not closed
    db_to_open_only <- startup_new[is.na(startup_new$download_date)&is.na(startup_new$shutdown_date), ]

    #neither db_to_close or db_to_open_only should have any duplicates. Remove these and warn the user

    # THIS SHOULDN'T REALLY HAPPEN
    duplicate_to_close_loggers<-unique(db_to_close_only$logger_serial_no[duplicated(db_to_close_only$logger_serial_no)])
    if(length(duplicate_to_close_loggers) > 0){
        log_warn("The following logger serial numbers have multiple close entries in the import sheet that cannot be added to the database: ", 
        paste(duplicate_to_close_loggers, collapse = ", "))
        db_to_close_only <- db_to_close_only[!db_to_close_only$logger_serial_no %in% duplicate_to_close_loggers, ]
        log_warn("These sessions will not be added to the database. Check if these sessions can be closed in the master import sheet.")
    }
    # WOULD SUGGEST MULTIPLE OPEN SESSIONS IN THE DB

    duplicate_to_open_loggers<-unique(db_to_open_only$logger_serial_no[duplicated(db_to_open_only$logger_serial_no)])
    if(length(duplicate_to_open_loggers) > 0){
        log_warn("The following logger serial numbers have multiple open startup entries in the import sheet that cannot be added to the database: ", 
        paste(duplicate_to_open_loggers, collapse = ", "))
        db_to_open_only <- db_to_open_only[!db_to_open_only$logger_serial_no %in% duplicate_to_open_loggers, ]
        log_warn("These sessions will not be added to the database. Check if these sessions can be closed in the master import sheet.")
    }

    # batch open closed sessions
    db_to_open_and_close_list <- list()
    db_to_open_and_close_list <- c(db_to_open_and_close_list, list(db_to_open_and_close))

    while(any(duplicated(db_to_open_and_close_list[[length(db_to_open_and_close_list)]]$logger_serial_no))){
        current_index <- length(db_to_open_and_close_list)
        current_df <- db_to_open_and_close_list[[length(db_to_open_and_close_list)]]
        first_df <- current_df[!duplicated(current_df$logger_serial_no), ]
        new_df <- current_df[duplicated(current_df$logger_serial_no), ]
        db_to_open_and_close_list[[current_index]] <- first_df
        db_to_open_and_close_list <- c(db_to_open_and_close_list, list(new_df))
    }

    db_to_open_and_close_sessions <- sapply(db_to_open_and_close_list, SessionBatch$new, type = "open_and_close")

    session_batches <- c(SessionBatch$new(db_to_close_only, "close_only"), db_to_open_and_close_sessions, SessionBatch$new(db_to_open_only, "open_only"))
    session_retrieval_deployments <- prepare_session_batches(session_batches, metadata)

}

prepare_session_batches <- function(session_batches, metadata){
    session_metadata_batches <- sapply(session_batches, prepare_session_batch, metadata = metadata)
    session_metadata_batches <- session_metadata_batches[which(!sapply(session_metadata_batches , is.null))]
    return(session_metadata_batches)    
}

prepare_session_batch <- function(session_batch, metadata){
    if(nrow(session_batch$sessions) == 0){
        log_info(glue::glue("No sessions available. Sessions would be {gsub('_',' ', session_batch$type)}"))
        return(NULL)
    }
    log_info(glue::glue("Prepare {nrow(session_batch$sessions)}. Sessions will be {gsub('_',' ', session_batch$type)}"))
    session_batch_open_date <- session_batch$sessions$download_date
    session_batch_open_date[is.na(session_batch_open_date)] <- session_batch$sessions$shutdown_date
    session_batch_open_date[is.na(session_batch_open_date)] <- session_batch$sessions$starttime_gmt + (100*60*60*24*365)

    #foreach session batch
    # find the deployments and retrievals for those loggers that are associated with these sessions
    #deployments
    deployed_match_idx <- sapply(seq_len(nrow(metadata)), function(metadata_row_idx) {
        metadata_row <- metadata[metadata_row_idx,]
        match_idx = which(metadata_row$date >= session_batch$sessions$starttime_gmt & 
            metadata_row$date <= session_batch_open_date &
            metadata_row$logger_id_deployed == session_batch$sessions$logger_serial_no & 
            !is.na(metadata_row$logger_id_deployed))
        return(match_idx)
    })
    deployed_bool<- sapply(deployed_match_idx, function(idx) length(idx) > 0)

    deployments <- metadata[deployed_bool,]
    log_trace(glue::glue("Found {nrow(deployments)} associated with these sessions"))

    db_deployments_df <- tibble(
        deployment_date = deployments$date,
        individ_id = paste(deployments$euring_code, deployments$ring_number, sep = "_")
    )
    new_rows_bool <- check_db_metadata_import(db_deployments_df, "loggers.deployment")
    new_deployments <- deployments[new_rows_bool, ]
    # As we are handling retrievals and deployments seperately, remove deployment information
    new_deployments$logger_model_retrieved <- NA
    new_deployments$logger_id_retrieved <- NA
    log_trace(glue::glue("Found {nrow(new_deployments)} associated with these sessions and not present in database."))

    retrieved_match_idx <- sapply(seq_len(nrow(metadata)), function(metadata_row_idx) {
        metadata_row <- metadata[metadata_row_idx,]
        match_idx = which(metadata_row$date >= session_batch$sessions$starttime_gmt & 
            metadata_row$date <= session_batch_open_date &
            metadata_row$logger_id_retrieved == session_batch$sessions$logger_serial_no & 
            !is.na(metadata_row$logger_id_retrieved))
        return(match_idx)
    })

    retrieved_bool<- sapply(retrieved_match_idx, function(idx) length(idx) > 0)

    retrievals <- metadata[deployed_bool,]
    log_trace(glue::glue("Found {nrow(retrievals)} associated with these sessions"))

    db_retrievals_df <- tibble(
        retrieval_date = retrievals$date,
        individ_id = paste(retrievals$euring_code, retrievals$ring_number, sep = "_")
    )
    new_rows_bool <- check_db_metadata_import(db_retrievals_df, "loggers.retrieval")
    new_retrievals <- retrievals[new_rows_bool, ]
    # As we are handling retrievals and deployments seperately, remove deployment information
    new_retrievals$logger_model_deployed <- NA
    new_retrievals$logger_id_deployed <- NA
    log_trace(glue::glue("Found {nrow(new_retrievals)} associated with these sessions and not present in database."))

    return(DBImportCollection$new(
        session_batch,
        retrievals,
        deployments
    ))

}

push_db_import_collection <- function(db_import_collection){
    if(db_import_collection$type %in% c("open_only", "open_and_close")){
        # startup - check if startup already in database
        startup <- check_startups_db(db_import_collection$sessions$sessions)
        if(nrow(startup) > 0){
            seatrackR::writeLoggerImport(startup)
        }
        
    }
    
    if(db_import_collection$type %in% c("open_only", "open_and_close")){
        if(nrow(db_import_collection$deployments) > 0){
            # deployment - already checked if in db
            writeMetadata(db_import_collection$deployments)
        }
    }

    if(db_import_collection$type %in% c("close_only", "open_and_close")){
        if(nrow(db_import_collection$retrievals) > 0){
            # retrieval - already checked if in db
            writeMetadata(db_import_collection$retrievals)
        }
    }

    # shutdown - check if startup already in database
    if(db_import_collection$type %in% c("close_only", "open_and_close")){
        # startup - check if startup already in database
        shutdown <- check_shutdown_db(db_import_collection$sessions$sessions)
        if(nrow(shutdown) > 0){
            seatrackR::writeLoggerImport(shutdown)
        }
    }
}





#' Convert an R value to its corresponding database representation
#' 
#' This function takes an R value and converts it to its corresponding database representation.
#' It handles different data types such as Date, NA, character, and others.
#' @param r_value An R value to be converted.
#' @return A string representing the database value.
#' @export 
#' @concept utility
R_value_to_db_value <- function(r_value) {
    if ("Date" %in% class(r_value)) {
        db_value <- glue::glue("DATE '{format(r_value)}'")
    } else if (any(class(r_value) %in% c("POSIXct", "POSIXt"))) {
        db_value <- glue::glue("TIMESTAMP '{format(r_value, '%Y-%m-%d %H:%M:%S')}'")
    } else if (is.na(r_value)) {
        db_value <- glue::glue("NULL")
    } else if ("character" %in% class(r_value)) {
        db_value <- glue::glue("'{r_value}'")
    } else {
        db_value <- r_value
    }
    return(db_value)
}

#' Convert a dataframe of R values to a string of database values for SQL insertion
#'
#' This function takes a dataframe of R values and converts each value to its corresponding database representation.
#' It then constructs a string of database values formatted for SQL insertion.
#' @param db_cols A dataframe containing the columns to be converted.
#' @return A string of database values formatted for SQL insertion.
#' @export 
#' @concept utility
R_df_to_db_values <- function(db_cols) {
    db_values <- lapply(seq_len(nrow(db_cols)), function(row_idx) {
        curr_row <- db_cols[row_idx, ]
        db_strings <- sapply(names(db_cols), function(col_name) R_value_to_db_value(curr_row[[col_name]]))
        db_row <- paste(db_strings, collapse = ", ")
        db_row <- paste0("(", db_row, ")")
    })
    combined_db_values <- paste(db_values, collapse = ", \n")
    return(combined_db_values)
}

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
#' @concept db_import
check_db_metadata_import <- function(metadata_df, table, col_names = NULL, db_col_names = NULL) {
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
    # check certain columns
    db_cols <- metadata_df[, col_names]

    temp_values <- R_df_to_db_values(db_cols)

    t_db_names <- paste0("t.", db_col_names)
    t_db_names_combined <- paste(t_db_names, collapse = ",\n")
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
    log_trace("Check existing rows in database using columns: ", paste(col_names, collapse = ", "), "... ")

    existing_rows <- DBI::dbGetQuery(con, query)
    log_trace("Check existing rows in database using columns: ", paste(col_names, collapse = ", "), "... ", nrow(existing_rows), " existing rows found")

    existing_rows_combined <- sapply(seq_len(nrow(existing_rows)), function(row_idx) {
        paste(existing_rows[row_idx, ], collapse = " ")
    })
    metadata_df_combined <- sapply(seq_len(nrow(metadata_df)), function(row_idx) {
        paste(metadata_df[row_idx, ], collapse = " ")
    })
    missing_row_bool <- !metadata_df_combined %in% existing_rows_combined
    log_trace(nrow(existing_rows), " rows are not in the database")

    return(missing_row_bool)
}
