#' Push master import file to database
#'
#' Function to push a master import file to the database. The function loads the master import file, prepares the data for database import, and then pushes the data to the database using the `push_db_import_collections` function.
#' @param colony Optional colony name to filter the master import file.
#' @param file_path Optional file path to a master import Excel file.
#' @concept db_import
#' @export
push_master_import_file_to_db <- function(colony = NULL, file_path = NULL) {
    master_sheets <- load_master_import(colony, file_path)
    db_import_collections <- prepare_master_sheet_for_db(master_sheets)
    push_db_import_collections(db_import_collections)
}



#' Push multiple database import collections to database
#' 
#' Function to push multiple DBImportCollection objects to the database. Each collection is processed in turn using the `push_db_import_collection` function.
#' @param db_import_collections A list of DBImportCollection objects.
#' @concept db_import
#' @export
push_db_import_collections <- function(db_import_collections){
    for(collection_idx in seq_along(db_import_collections)){
        log_info(glue::glue("Pushing session batch {collection_idx}/{length(db_import_collections)}"))
        push_db_import_collection(db_import_collections[[collection_idx]])
    }
}

#' Push database import collection to database
#' 
#' Function to push a DBImportCollection object to the database. The function handles the import of startups, deployments, retrievals, and shutdowns associated with the sessions in the collection.
#' It checks the database for existing entries and only imports new entries to avoid duplicates. 
#' It performs checks to make sure sessions are started before a logger is deployed, deployed before it is retrieved and (if it has a deployment) is retrieved before being shutdown.
#' A database error leads to the whole function stopping and no partial imports of the table the database fails on.
#' @param db_import_collection A DBImportCollection object containing session, deployment, and retrieval information.
#' @concept db_import
#' @export
push_db_import_collection <- function(db_import_collection){

    push_startup(db_import_collection$sessions$sessions)

    push_deployments(db_import_collection$deployments)

    push_retrievals(db_import_collection$retrievals)

    push_shutdowns(db_import_collection$sessions$sessions)
    
}

#' Push startups to database
#' 
#' Function to push startups from a master import sheet to the database. It checks the database for existing startups and only imports new startups to avoid duplicates.
#' @param startup A tibble containing session information from a DBImportCollection object.
#' @concept db_import
#' @export
push_startup <- function(startup){
        # startup - check if startup already in database
    startup <- check_startups_db(startup)
    
    if(nrow(startup) > 0){
        log_info(glue::glue("Pushing {nrow(startup)} startups to database"))
        startup$shutdown_session <- FALSE
        seatrackR::writeLoggerImport(startup[, 1:15])
    }
}

#' Push deployments to database
#'
#' Function to push deployments from a master import sheet to the database. It checks the database for existing deployments and only imports new deployments to avoid duplicates.
#' It also checks that the loggers being deployed have an associated session in the database.
#' @param deployments A tibble containing deployment information from a DBImportCollection object.
#' @concept db_import
#' @export
push_deployments <-function(deployments){
    deployments <- check_deployment_db(deployments)
    if(nrow(deployments) > 0){

        # check if logger is registered as starting
        logger_db <- data.frame(logger_serial_no = deployments$logger_id_deployed)

        existing_loggers <- get_db_metadata_import(logger_db, "loggers.logger_info", additional_db_col_names = c("logger_id"))
        existing_loggers <- existing_loggers[match(logger_db$logger_serial_no, existing_loggers$logger_serial_no),]

        deploy_startup_df <- data.frame(
            logger_id = existing_loggers$logger_id,
            deployment_id = NA,
            retrieval_id = NA
        )

        missing_startups <- check_db_metadata_import(deploy_startup_df, "loggers.logging_session")
        deployment_missing_startups <- deployments[missing_startups, ]

        if(nrow(deployment_missing_startups) > 0){
            log_warn("The following loggers cannot be registered as being deployed as they have no session in the database.")
            log_warn("Attempting to push them to the database would result in an error. These loggers will not be registered as deployed")

            for(i in seq_len(nrow(deployment_missing_startups))){
                deployment_summary <- deployment_missing_startups[i, c("date", "logger_id_deployed", "comment")]
                log_warn(deployment_summary$logger_id_deployed,
                ":\n", paste(capture.output(print(deployment_summary, n = nrow(deployment_summary)))[c(-1, -3)], collapse = "\n"))
            }

            log_warn("- Check which session these deployments should occur in. Is there an error in the metadata sheet?")
        }            

        deployments <- deployments[!missing_startups, ]
        log_info(glue::glue("Pushing {nrow(deployments)} deployments to database"))
        # deployment - check if in db
        seatrackR::writeMetadata(deployments)
    }
}

#' Push retrievals to database
#'  
#' Function to push retrievals from a master import sheet to the database. It checks the database for existing retrievals and only imports new retrievals to avoid duplicates.
#' It also checks that the loggers being retrieved have an associated deployment in the database.
#' @param retrievals A tibble containing retrieval information from a DBImportCollection object.
#' @concept db_import
#' @export
push_retrievals <- function(retrievals){
    retrievals <- check_retrieval_db(retrievals)
    if(nrow(retrievals) > 0){

        logger_db <- data.frame(logger_serial_no = retrievals$logger_id_retrieved)

        existing_loggers <- get_db_metadata_import(logger_db, "loggers.logger_info", additional_db_col_names = c("logger_id"))
        existing_loggers <- existing_loggers[match(logger_db$logger_serial_no, existing_loggers$logger_serial_no),]
                    
        retrieval_deploy_df <- data.frame(
            individ_id = paste(retrievals$euring_code, retrievals$ring_number, sep = "_"),
            logger_id = existing_loggers$logger_id,
            retrieval_id = NA
        )


        missing_deployments <- check_db_metadata_import(retrieval_deploy_df, "loggers.logging_session")
        retrievals_missing_deployments <- retrievals[missing_deployments, ]
        if(nrow(retrievals_missing_deployments) > 0){
            log_warn("The following loggers cannot be registered as being retrieved as they have no deployment events in the database.")
            log_warn("Attempting to push them to the database would result in an error. These loggers will not be registered as retrieved")

            for(i in seq_len(nrow(retrievals_missing_deployments))){
                retrieval_summary <- retrievals_missing_deployments[i, c("date", "logger_id_retrieved", "comment")]
                log_warn(retrieval_summary$logger_id_retrieved,
                ":\n", paste(capture.output(print(retrieval_summary, n = nrow(retrieval_summary)))[c(-1, -3)], collapse = "\n"))
            }

            log_warn("- Check METADATA sheet for an appropriate deployment event.")
            log_warn("- Check starttime_gmt of a session would contain the deployment event.") 
        }

        retrievals <- retrievals[!missing_deployments, ]

        log_info(glue::glue("Pushing {nrow(retrievals)} retrievals to database"))
        seatrackR::writeMetadata(retrievals)
    }
}

#' Push shutdowns to database
#'
#' Function to push shutdowns from a master import sheet to the database. It checks the database for existing shutdowns and only imports new shutdowns to avoid duplicates.
#' It also checks that the loggers being shutdown have an associated retrieval in the database if they have a deployment. 
#' It also checks that loggers without deployments are not being shutdown if they are marked as 'Successfully downloaded' or 'Nonresponsive'.
#' @param shutdown A tibble containing session information from a DBImportCollection object.
#' @concept db_import
#' @export
push_shutdowns <- function(shutdown){
    # shutdown - check if shutdown already in database
    shutdown <- check_shutdown_db(shutdown)
    shutdown$temp_idx <- seq_len(nrow(shutdown))
    # if a session is to be shut down and has a deployment, check for a retrieval
    shutdown_deployment_df <- data.frame(
        session_id = paste0(shutdown$logger_serial_no,"_",as.Date(shutdown$starttime_gmt))
    )
    existing_deployments <- !check_db_metadata_import(shutdown_deployment_df, "loggers.deployment")
    shutdowns_with_deployments <- shutdown[existing_deployments, ]
    shutdown_retrieval_df <- data.frame(
        session_id = paste0(shutdowns_with_deployments$logger_serial_no,"_",as.Date(shutdowns_with_deployments$starttime_gmt))
    )
    missing_retrievals <- check_db_metadata_import(shutdown_retrieval_df, "loggers.retrieval")
    shutdowns_missing_retrievals <- shutdowns_with_deployments[missing_retrievals, ]

    if(nrow(shutdowns_missing_retrievals) >0 ){
        log_warn("The following loggers cannot be registered as shutdown as they have deployment events but no retrieval event in the database.")
        log_warn("Attempting to push them to the database would result in an error. These sessions will not be shutdown.")

        for(i in seq_len(nrow(shutdowns_missing_retrievals))){
            session_summary <- shutdowns_missing_retrievals[i, c("logger_serial_no","starttime_gmt","download_date", "shutdown_date")]
            log_warn(paste0(session_summary$logger_serial_no,"_",as.Date(session_summary$starttime_gmt)),
                ":\n", paste(capture.output(print(session_summary, n = nrow(session_summary)))[c(-1, -3)], collapse = "\n"))
        }

        log_warn("- Check METADATA sheet for an appropriate retrieval event that falls inside the session.")
        log_warn("- Check download date of session would contain the retrieval event.")
        log_warn("- Check if the session has a later shutdown date as well as a download date. Check this date is formatted to be imported safely.")
    }


    shutdown <- shutdown[!shutdown$temp_idx %in% shutdowns_missing_retrievals$temp_idx, ]
    
    shutdown_deployment_df <- data.frame(
        session_id = paste0(shutdown$logger_serial_no,"_",as.Date(shutdown$starttime_gmt))
    )
    no_deployments <- check_db_metadata_import(shutdown_deployment_df, "loggers.deployment")
    shutdown_without_deployments <- shutdown[no_deployments, ]
    downloaded_missing_deployments <- shutdown_without_deployments[shutdown_without_deployments$download_type %in% c("Successfully downloaded", "Nonresponsive", "Reconstructed"),]
    if(nrow(downloaded_missing_deployments) >0 ){
        log_warn("The following loggers cannot be registered as shutdown as they have no deployment events but are flagged as 'Succesfully downloaded', 'Nonresponsive' or 'Reconstructed' .")
        log_warn("Attempting to push them to the database would result in an error. These sessions will not be shutdown.")

        for(i in seq_len(nrow(downloaded_missing_deployments))){
            session_summary <- downloaded_missing_deployments[i, c("logger_serial_no","starttime_gmt","download_date", "shutdown_date")]
            log_warn(paste0(session_summary$logger_serial_no,"_",as.Date(session_summary$starttime_gmt)),
                ":\n", paste(capture.output(print(session_summary, n = nrow(session_summary)))[c(-1, -3)], collapse = "\n"))
        }
    }
    shutdown <- shutdown[!shutdown$temp_idx %in% downloaded_missing_deployments$temp_idx, ]    

    if(nrow(shutdown) > 0){
        log_info(glue::glue("Pushing {nrow(shutdown)} shutdowns to database"))
        # Tell the DB the session is being shutdown
        shutdown$intended_species=NA
        shutdown$shutdown_session <- TRUE
        seatrackR::writeLoggerImport(shutdown[,1:22])
    }    
}


