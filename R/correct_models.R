#' Correct logger models in metadata based on database values
#'
#' This function checks the logger models in the metadata against the database values and corrects any mismatches.
#' It also uses logger startup information to fill in missing logger models if available. Database connection is required
#' @param metadata Metadata dataframe to correct
#' @param startup Optional dataframe of logger startups to use for correction if database values are missing
#' @return Corrected metadata dataframe
#' @export
#' @concept metadata
correct_models <- function(metadata, startups = NULL) {
    db_logger_model <-
        data.frame(
            logger_serial_no = c(metadata$logger_id_deployed, metadata$logger_id_retrieved),
            logger_model = c(metadata$logger_model_deployed, metadata$logger_model_retrieved)
        )
    db_logger_model <- db_logger_model[!is.na(db_logger_model$logger_serial_no), ]
    db_logger_model <- dplyr::distinct(db_logger_model)

    logger_model_bool <- check_db_metadata_import(db_logger_model, "loggers.logger_info")
    missing_logger_models <- db_logger_model[logger_model_bool, ]
    # Ignore cases where the logger model is not in the database at all
    db_logger <- data.frame(logger_serial_no = missing_logger_models$logger_serial_no)
    logger_exists_bool <- check_db_metadata_import(db_logger, "loggers.logger_info")
    mismatch_logger_models <- missing_logger_models[!logger_exists_bool, ]
    if (nrow(mismatch_logger_models) > 0) {
        # Correct these
        db_true_logger_model <- get_db_metadata_import(data.frame(logger_serial_no = mismatch_logger_models$logger_serial_no), "loggers.logger_info", additional_db_col_names = c("logger_model", "production_year"))
        n_mismatch <- nrow(db_true_logger_model)
        if (n_mismatch > 0) {
            model_summary <- tibble::as_tibble(db_true_logger_model)
            model_summary$md_logger_model <- missing_logger_models$logger_model[match(model_summary$logger_serial_no, missing_logger_models$logger_serial_no)]
            model_summary <- model_summary[model_summary$md_logger_model != model_summary$logger_model | is.na(model_summary$md_logger_model), ]
            log_warn(glue::glue("{n_mismatch} logger deployments/retrievals had a mismatch between the master sheet and the database."), "\n", paste(capture.output(print(model_summary, n = nrow(model_summary)))[c(-1, -3)], collapse = "\n"))
            log_warn("Database value will be used. Consider updating master metadata")
            metadata$logger_model_deployed[metadata$logger_id_deployed %in% db_true_logger_model$logger_serial_no] <- db_true_logger_model$logger_model[match(metadata$logger_id_deployed[metadata$logger_id_deployed %in% db_true_logger_model$logger_serial_no], db_true_logger_model$logger_serial_no)]
            metadata$logger_model_retrieved[metadata$logger_id_retrieved %in% db_true_logger_model$logger_serial_no] <- db_true_logger_model$logger_model[match(metadata$logger_id_retrieved[metadata$logger_id_retrieved %in% db_true_logger_model$logger_serial_no], db_true_logger_model$logger_serial_no)]
        }
    }

    if (!is.null(startups)) {
        missing_deployment_ids <- metadata$logger_id_deployed[is.na(metadata$logger_model_deployed) & !is.na(metadata$logger_id_deployed)]
        metadata$logger_model_deployed[is.na(metadata$logger_model_deployed) & !is.na(metadata$logger_id_deployed)] <- startups$logger_model[match(missing_deployment_ids, startups$logger_serial_no)]

        missing_retrieval_ids <- metadata$logger_id_retrieved[is.na(metadata$logger_model_retrieved) & !is.na(metadata$logger_id_retrieved)]
        metadata$logger_model_retrieved[is.na(metadata$logger_model_retrieved) & !is.na(metadata$logger_id_retrieved)] <- startups$logger_model[match(missing_retrieval_ids, startups$logger_serial_no)]
    }

    return(metadata)
}
