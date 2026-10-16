#' Clean the field plan data
#'
#' This function processes the field plan data by selecting relevant columns, renaming them, calculating deployment and retrieval success rates based on the best available sources.
#' A connection to the SEATRACK database is required.
#' @param field_plan_sheet A data frame containing the raw field plan data
#' @param target_species Vector of species names to use in correcting the different ways partners might write species
#' @param use_master_sheets A boolean indicating whether to use master sheets to calculate success.
#' @param all_locations An optional data frame containing all location metadata from the master import, if not provided this will be imported.
#' @param use_db A boolean indicating whether to use the database to calculate success.
#' @param field_year The year to filter the metadata for when using master sheets (default is the current year)
#' @return A cleaned data frame with deployment and retrieval statistics
#' @examples
#' \dontrun{
#' field_plan_sheet <- get_field_plan("path/to/fieldplan.xlsx")
#' seatrackR::connectSeatrack()
#' field_plan_clean <- get_clean_field_plan(field_plan_sheet)
#' }
#' @export
#' @concept field_planning
get_clean_field_plan <- function(field_plan_sheet, target_species = NULL, use_master_sheets = FALSE, all_locations = NULL, use_db = TRUE, field_year = c(as.numeric(format(Sys.Date(), "%Y")))) {
    all_models <- seatrackR::getLoggerModels()
    # classify models
    gps_models <- c(
        "NanoFix_GEO_mini",
        "PicoFix_GEO_mini2",
        "NanoFix_GEO_mini",
        "PicoFix_GEO_mini3",
        "050522_NanoFixAccM_20",
        "NanoFix_GEO_GPS",
        "NanoFix_GEO_mini_5.5g",
        "picofix",
        all_models$model[all_models$logger_type == "GPS"]
    )

    gsm_models <- c("OrniTrack15", "OrniTrack10", all_models$model[all_models$logger_type == "GPS-GSM"])

    # Subset columns
    field_plan_clean <- field_plan_sheet[, c(
        "Ocean area", "Colony", "Species",
        "GLS assigned", "GLS deployments", "GLS retrievals",
        "GPS assigned", "GPS deployments", "GPS retrievals",
        "GPS-GSM assigned", "GPS-GSM deployments",
        "Age"
    )]
    # Rename
    names(field_plan_clean) <- c(
        "LME", "Location", "Species",
        "planned", "deployed", "retrieved",
        "gps_planned", "gps_deployed", "gps_retrieved",
        "gps_gsm_planned", "gps_gsm_deployed",
        "age"
    )

    # split the different logger types into seperate dataframes, rename
    field_plan_clean_gps_only <- field_plan_clean[, c(
        "LME", "Location", "Species",
        "gps_planned", "gps_deployed", "gps_retrieved",
        "age"
    )]
    names(field_plan_clean_gps_only) <- c(
        "LME", "Location", "Species",
        "planned", "deployed", "retrieved",
        "age"
    )
    field_plan_clean_gps_only$logger_type <- "GPS"

    field_plan_clean_gsm_only <- field_plan_clean[, c(
        "LME", "Location", "Species",
        "gps_gsm_planned", "gps_gsm_deployed",
        "age"
    )]
    names(field_plan_clean_gsm_only) <- c(
        "LME", "Location", "Species",
        "planned", "deployed",
        "age"
    )
    # GSM doesn't get retrieved
    field_plan_clean_gsm_only$retrieved <- NA
    field_plan_clean_gsm_only$logger_type <- "GPS-GSM"
    field_plan_clean_gsm_only <- field_plan_clean_gsm_only[, names(field_plan_clean_gps_only)]

    # remove these columns
    field_plan_clean[, c("gps_planned", "gps_deployed", "gps_retrieved", "gps_gsm_planned", "gps_gsm_deployed", "gps_gsm_retrieved")] <- NULL
    field_plan_clean$logger_type <- "GLS"
    # combine the dataframe
    field_plan_clean <- rbind(field_plan_clean, field_plan_clean_gps_only, field_plan_clean_gsm_only)
    field_plan_clean$source <- "reported"

    field_plan_clean <- field_plan_clean[!is.na(field_plan_clean$planned), ]

    if (use_master_sheets) {
        log_info("Using master sheets to calculate success...")
        # Shift to function
        if (is.null(all_locations)) {
            log_info("Loading all metadata from master import...")
            all_locations <- load_all_master_import(skip = c(c("Blomstrand", "Keysite Vestland", "Lowestoft", "Iceland_processed_metadata", "not_processed", "no_location_not_processed")))
        }

        startups <- all_locations$STARTUP_SHUTDOWN
        all_metadata <- all_locations$METADATA
        # Harmonise metadata colony names
        original_colony <- all_metadata$colony
        all_metadata$colony <- fix_colony(all_metadata$colony, TRUE)
        if (any(is.na(all_metadata$colony))) {
            problem_colony <- paste(unique(original_colony[is.na(all_metadata$colony)]), collapse = ", ")
            stop(glue::glue("The following colonies are not in the database: {problem_colony}"))
        }

        all_metadata$year <- as.numeric(format(as.Date(all_metadata$date), "%Y"))
        all_metadata <- all_metadata[order(as.Date(all_metadata$date)), ]
        metadata <- all_metadata[all_metadata$year %in% field_year, ]
        metadata <- correct_models(metadata, startups)
        metadata$deployment <- !is.na(metadata$logger_id_deployed)
        metadata$retrieval <- !is.na(metadata$logger_id_retrieved)

        metadata$deployment_retrieval <- metadata$deployment & metadata$retrieval
        metadata_deployment <- metadata[metadata$deployment & !metadata$deployment_retrieval, ]
        metadata_retrieval <- metadata[metadata$retrieval & !metadata$deployment_retrieval, ]
        metadata_both <- metadata[metadata$deployment_retrieval, ]
        metadata_both_dup <- rbind(metadata_both, metadata_both)
        metadata_both_dup$deployment[1:nrow(metadata_both)] <- TRUE
        metadata_both_dup$retrieval[1:nrow(metadata_both)] <- FALSE
        metadata_both_dup$retrieval[(nrow(metadata_both) + 1):nrow(metadata_both_dup)] <- TRUE
        metadata_both_dup$deployment[(nrow(metadata_both) + 1):nrow(metadata_both_dup)] <- FALSE
        metadata <- rbind(metadata_deployment, metadata_retrieval, metadata_both_dup)

        metadata$type <- NA
        metadata$type[metadata$retrieval] <- "retrieval"
        metadata$type[metadata$deployment] <- "deployment"
        metadata <- metadata[!is.na(metadata$type), ]

        all_models <- unique(c(metadata$logger_model_deployed, metadata$logger_model_retrieved))

        missing_models <- metadata[(metadata$type == "deployment" & is.na(metadata$logger_model_deployed)) | (metadata$type == "retrieval" & is.na(metadata$logger_model_retrieved)), ]
        other_models <- metadata[(metadata$type == "deployment" & !is.na(metadata$logger_model_deployed) & metadata$logger_model_deployed == "other") | (metadata$type == "retrieval" & !is.na(metadata$logger_model_retrieved) & metadata$logger_model_retrieved == "other"), ]

        # drop other models
        metadata <- metadata[!((metadata$type == "deployment" & !is.na(metadata$logger_model_deployed) & metadata$logger_model_deployed == "other") | (metadata$type == "retrieval" & !is.na(metadata$logger_model_retrieved) & metadata$logger_model_retrieved == "other")), ]

        gls_models <- all_models[!all_models %in% c(gps_models, gsm_models)]

        metadata$model_type <- "GLS"
        metadata$model_type[metadata$type == "deployment" & metadata$logger_model_deployed %in% gps_models | metadata$type == "retrieval" & metadata$logger_model_retrieved %in% gps_models] <- "GPS"
        metadata$model_type[metadata$type == "deployment" & metadata$logger_model_deployed %in% gsm_models | metadata$type == "retrieval" & metadata$logger_model_retrieved %in% gsm_models] <- "GPS-GSM"

        log_info("Calculating age at deployment for retrievals...")
        metadata$age_at_deployment <- NA
        metadata$age_at_deployment[metadata$type == "deployment"] <- metadata$age[metadata$type == "deployment"]
        # try to get age at deployment from the database

        # Copy metadata to the database as a temporary table
        metadata_db <- dplyr::copy_to(
            con,
            metadata[metadata$type == "retrieval", ],
            name = "tmp_metadata",
            temporary = TRUE,
            overwrite = TRUE
        )

        # Reference the remote table
        db_status <- dplyr::tbl(con, dbplyr::in_schema("individuals", "individ_status"))
        db_deployment <- dplyr::tbl(con, dbplyr::in_schema("loggers", "deployment"))
        db_retrieval <- dplyr::tbl(con, dbplyr::in_schema("loggers", "retrieval"))
        db_dep_ret <- dplyr::union_all(dplyr::select(db_deployment, status_id, logger_id), dplyr::select(db_retrieval, status_id, logger_id))

        db_loggers <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
        db_status_filtered <- dplyr::left_join(db_status, db_dep_ret, by = "status_id") %>%
            dplyr::group_by(info_id, logger_id) %>%
            dplyr::slice_min(status_date, n = 1, with_ties = FALSE) %>%
            dplyr::ungroup()

        db_status_filtered <- dplyr::left_join(db_status_filtered, db_loggers, by = "logger_id")


        # Perform the join in the database
        result <- dplyr::left_join(
            metadata_db,
            db_status_filtered,
            by = join_by("ring_number", logger_id_retrieved == logger_serial_no),
            suffix = c("_metadata", "_db")
        ) %>%
            dplyr::select(date, colony, logger_id_retrieved, logger_model, ring_number, age_db) %>%
            dplyr::collect()

        metadata$age_at_deployment[metadata$type == "retrieval"] <- dplyr::left_join(metadata[metadata$type == "retrieval", ], result, by = c("ring_number", "logger_id_retrieved"), multiple = "first") %>% dplyr::pull(age_db)

        # In cases where age is still NA, try to find matching deployment age within the metadata
        na_bool <- is.na(metadata$age_at_deployment) & metadata$type == "retrieval"
        na_ids <- dplyr::distinct(metadata[na_bool, c("ring_number", "logger_id_retrieved")])
        metadata_filtered <- all_metadata %>%
            dplyr::group_by("ring_number", "logger_id_deployed") %>%
            dplyr::slice_min(date, n = 1, with_ties = FALSE)

        first_occurences <- dplyr::left_join(na_ids, metadata_filtered, by = dplyr::join_by(ring_number, logger_id_retrieved == logger_id_deployed))

        metadata$age_at_deployment[na_bool] <- dplyr::left_join(metadata[na_bool, ], first_occurences, by = c("ring_number", "logger_id_retrieved"), suffix = c("", "_first"), multiple = "first")$age_first

        # classify ages
        metadata$age_class <- "A"
        metadata$age_class[tolower(metadata$age_at_deployment) %in% c("pullus", "chick", "pull", "juvenile") & !is.na(metadata$age_at_deployment)] <- "C"

        metadata_summary <- metadata %>%
            group_by(colony, species, type, model_type, age_class) %>%
            summarise(n = n(), .groups = "drop")

        metadata_summary <- metadata_summary %>%
            tidyr::pivot_wider(
                names_from = type,
                values_from = n,
                values_fill = 0
            ) %>%
            dplyr::rename(
                Species = species,
                deployed = deployment,
                retrieved = retrieval,
                Location = colony,
                age = age_class,
                logger_type = model_type
            )

        metadata_summary$retrieved[metadata_summary$logger_type == "GPS-GSM"] <- NA

        field_plan_clean <- dplyr::left_join(field_plan_clean, metadata_summary, by = c("Species", "Location", "logger_type", "age"), suffix = c("_reported", "_metadata"))

        field_plan_clean$deployed <- field_plan_clean$deployed_reported
        field_plan_clean$retrieved <- field_plan_clean$retrieved_reported

        colonies_db <- colonies <- dplyr::tbl(con, dbplyr::in_schema("metadata", "colony_details")) %>% collect()
        missing_rows <- dplyr::anti_join(metadata_summary, field_plan_clean, metadata_summary, by = c("Species", "Location", "logger_type", "age"))


        missing_rows$planned <- 0
        missing_rows$source <- "metadata"
        missing_rows$LME <- left_join(missing_rows, colonies_db, by = dplyr::join_by(Location == colony_int_name)) %>% pull(lme_name)
        missing_rows <- dplyr::mutate(missing_rows, deployed_metadata = deployed, retrieved_metadata = retrieved)
        missing_rows$retrieved_reported <- missing_rows$deployed_reported <- NA

        missing_rows <- missing_rows[, names(field_plan_clean)]
        field_plan_clean <- rbind(field_plan_clean, missing_rows)

        field_plan_clean$deployed[!is.na(field_plan_clean$deployed_metadata) | !is.na(field_plan_clean$retrieved_metadata)] <- field_plan_clean$deployed_metadata[!is.na(field_plan_clean$deployed_metadata) | !is.na(field_plan_clean$retrieved_metadata)]
        field_plan_clean$retrieved[!is.na(field_plan_clean$deployed_metadata) | !is.na(field_plan_clean$retrieved_metadata)] <- field_plan_clean$retrieved_metadata[!is.na(field_plan_clean$retrieved_metadata) | !is.na(field_plan_clean$deployed_metadata)]

        field_plan_clean$source[!is.na(field_plan_clean$deployed_metadata) | !is.na(field_plan_clean$retrieved_metadata)] <- "metadata"

        field_plan_clean <- field_plan_clean %>%
            select(-starts_with("deployed_"), -starts_with("retrieved_"), everything(), starts_with("deployed_"), starts_with("retrieved_"))

        # could also add the database numbers here for our own benefit
    }

    if (use_db) {
        log_info("Using database to calculate success...")
        db_deployments <- dplyr::tbl(con, dbplyr::in_schema("loggers", "deployment"))
        db_retrievals <- dplyr::tbl(con, dbplyr::in_schema("loggers", "retrieval"))
        db_logger_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
        db_status <- dplyr::tbl(con, dbplyr::in_schema("individuals", "individ_status"))

        db_deployments <- dplyr::filter(db_deployments, lubridate::year(deployment_date) %in% field_year)
        db_retrievals <- dplyr::filter(db_retrievals, lubridate::year(retrieval_date) %in% field_year)
        db_dep_ret <- dplyr::union_all(dplyr::select(db_deployments, status_id, logger_id, session_id), dplyr::select(db_retrievals, status_id, logger_id, session_id))
        db_status <- dplyr::left_join(db_status, db_dep_ret, by = "status_id")
        db_status <- dplyr::left_join(db_status, db_loggers, by = "logger_id")


        log_info("Filtering database status to first record for each logger and ring number...")
        db_status_filtered <- db_status %>%
            dplyr::group_by(session_id, logger_id) %>%
            dplyr::slice_min(status_date, n = 1, with_ties = FALSE) %>%
            dplyr::ungroup()

        db_deployments <- dplyr::left_join(db_deployments, db_status_filtered, by = "session_id")
        db_retrievals <- dplyr::left_join(db_retrievals, db_status_filtered, by = "session_id")

        log_info("Classifying logger types for database entries...")
        db_deployments <- dplyr::mutate(db_deployments,
            age_class = ifelse(tolower(age) %in% c("pullus", "chick", "pull") & !is.na(age), "C", "A"),
            logger_type = dplyr::case_when(logger_model %in% gps_models ~ "GPS", logger_model %in% gsm_models ~ "GPS-GSM", .default = "GLS")
        )

        db_retrievals <- dplyr::mutate(db_retrievals,
            age_class = ifelse(tolower(age) %in% c("pullus", "chick", "pull") & !is.na(age), "C", "A"),
            logger_type = dplyr::case_when(logger_model %in% gps_models ~ "GPS", logger_model %in% gsm_models ~ "GPS-GSM", .default = "GLS")
        )

        log_info("Summarising database deployments and retrievals...")
        db_deployments_summary <- dplyr::group_by(db_deployments, location, species, logger_type, age_class) %>%
            dplyr::summarise(deployed_db = n(), .groups = "drop") %>%
            dplyr::rename(age = "age_class", Location = "location", Species = "species") %>%
            dplyr::collect()


        db_retrievals_summary <- dplyr::group_by(db_retrievals, location, species, logger_type, age_class) %>%
            dplyr::summarise(retrieved_db = n(), .groups = "drop") %>%
            dplyr::rename(age = "age_class", Location = "location", Species = "species") %>%
            dplyr::collect()

        log_info("Merging database summaries with field plan...")

        field_plan_clean$deployed_db <- dplyr::left_join(field_plan_clean, db_deployments_summary, by = join_by("Location", "age", "Species", "logger_type")) %>% pull("deployed_db")
        field_plan_clean$retrieved_db <- dplyr::left_join(field_plan_clean, db_retrievals_summary, by = join_by("Location", "age", "Species", "logger_type")) %>% pull("retrieved_db")

        valid_db_deployed_bool <- !is.na(field_plan_clean$deployed_db) & !is.na(field_plan_clean$deployed_metadata) & field_plan_clean$deployed_db >= field_plan_clean$deployed_metadata
        valid_db_retrieved_bool <- !is.na(field_plan_clean$retrieved_db) & !is.na(field_plan_clean$retrieved_metadata) & field_plan_clean$retrieved_db >= field_plan_clean$retrieved_metadata

        valid_db_bool <- valid_db_deployed_bool & valid_db_retrieved_bool

        field_plan_clean$deployed[valid_db_bool] <- field_plan_clean$deployed_db[valid_db_bool]
        field_plan_clean$retrieved[valid_db_bool] <- field_plan_clean$retrieved_db[valid_db_bool]

        field_plan_clean$source[valid_db_bool] <- "database"
    }

    field_plan_clean <- dplyr::mutate(field_plan_clean, Species = target_species[match(tolower(field_plan_clean$Species), tolower(c(target_species)))])

    best_source <- function(variable) {
        if (any(variable == "database")) {
            return("database")
        } else if (any(variable == "metadata")) {
            return("metadata")
        }
        return("reported")
    }

    # collapse to avoid duplicates driven by variations in species names

    get_lme <- function(location) {
        location_lme <- field_plan_clean$LME[field_plan_clean$Location == location]
        location_lme_valid <- location_lme[!is.na(location_lme)]

        if (length(location_lme_valid) == 0) {
            return(NA)
        }

        return(location_lme_valid[1])
    }

    sum_if_no_na <- function(variable, second_variable = NULL) {
        if (any(!is.na(variable))) {
            return(sum(variable, na.rm = TRUE))
        } else if (!is.null(second_variable) && any(!is.na(second_variable))) {
            return(0)
        } else {
            return(NA)
        }
    }

    max_if_no_na <- function(variable, second_variable = NULL) {
        if (any(!is.na(variable))) {
            return(max(variable, na.rm = TRUE))
        } else if (!is.null(second_variable) && any(!is.na(second_variable))) {
            return(0)
        } else {
            return(NA)
        }
    }

    field_plan_clean <- dplyr::rowwise(field_plan_clean) %>% dplyr::mutate(LME = get_lme(Location))

    field_plan_clean <- dplyr::group_by(field_plan_clean, LME, Location, Species, age, logger_type) %>%
        dplyr::summarise(
            source = best_source(source),
            planned = max_if_no_na(planned),
            deployed = max_if_no_na(deployed, retrieved),
            retrieved = max_if_no_na(retrieved, deployed),
            deployed_reported = sum_if_no_na(deployed_reported, retrieved_reported),
            retrieved_reported = sum_if_no_na(retrieved_reported, deployed_reported),
            deployed_metadata = sum_if_no_na(deployed_metadata, retrieved_metadata),
            retrieved_metadata = sum_if_no_na(retrieved_metadata, deployed_metadata),
            deployed_db = sum_if_no_na(deployed_db, retrieved_db),
            retrieved_db = sum_if_no_na(retrieved_db, deployed_db),
        ) %>%
        dplyr::ungroup() %>%
        dplyr::relocate(names(field_plan_clean))


    # Deployment success
    field_plan_clean$dep_success <- field_plan_clean$deployed / field_plan_clean$planned

    field_plan_clean$dep_success[is.infinite(field_plan_clean$dep_success)] <- NA

    # Retrieval success
    retrieval_target_year <- min(field_year) - 1
    history_table_summary <- get_history_table(retrieval_target_year, "retrieval")

    field_plan_clean <- dplyr::left_join(field_plan_clean, history_table_summary, by = join_by(
        Location == colony,
        Species == species,
        age == age_deployment_class,
        logger_type == logger_type
    ))

    field_plan_clean$ret_success <- field_plan_clean$retrieved / field_plan_clean$prev_retrieved
    field_plan_clean$ret_success[is.infinite(field_plan_clean$ret_success)] <- NA

    # reorder to match other tables
    reorder_vector <- c("LME", "Location", "Species", "age", "logger_type", "planned", "retrieved", "deployed", "prev_retrieved", "ret_success", "dep_success")
    reorder_vector <- c(reorder_vector, colnames(field_plan_clean)[!colnames(field_plan_clean) %in% reorder_vector])
    field_plan_clean <- field_plan_clean[, reorder_vector]

    field_plan_clean <- field_plan_clean[(!is.na(field_plan_clean$planned) & field_plan_clean$planned > 0) | (!is.na(field_plan_clean$retrieved) & field_plan_clean$retrieved > 0) | (!is.na(field_plan_clean$deployed) & field_plan_clean$deployed > 0), ]
    field_plan_clean$source[field_plan_clean$source == "reported" & is.na(field_plan_clean$deployed_reported) & is.na(field_plan_clean$retrieved_reported)] <- NA


    return(field_plan_clean)
}


#' Get historical deployment and retrieval data
#'
#' This function retrieves historical deployment and retrieval data from the SEATRACK database for a specified year and event type (Deployment or Retrieval).
#' It filters the data based on the event type, age class, and logger type, and returns a data frame with relevant information.
#' A connection to the SEATRACK database is required. This should probably be moved to seatrackRdb at some point.
#' @param history_year The year for which to retrieve historical data (e.g., 2023)
#' @param event_type A vector specifying the event type(s) to filter by (defaults to c("deployment", "retrieval"))
#' @return A data frame containing historical deployment and retrieval data
#' @examples
#' \dontrun{
#' seatrackR::connectSeatrack()
#' history_data <- get_history_table(2023, c("Deployment", "Retrieval"))
#' }
#' @keywords internal
get_history_table <- function(history_year, event_type = c("deployment", "retrieval")) {
    seatrackR:::checkCon()
    # Rewrite to do this in the db.
    # Get all individuals (seems fast, but a tad wasteful in terms of memory)
    if (event_type == "deployment") {
        log_info(paste("Retrieving historical deployment data for year", history_year, "..."))
        individual_info <- seatrackR::getIndividInfo(event_type = event_type, deployment_year = history_year, age_at_deployment = NULL, as_tibble = FALSE)
    } else if (event_type == "retrieval") {
        log_info(paste("Retrieving historical retrieval data for year", history_year, "..."))
        individual_info <- seatrackR::getIndividInfo(event_type = event_type, retrieval_year = history_year, age_at_deployment = NULL, as_tibble = FALSE)
    }

    history_table <- dplyr::select(individual_info, colony, species, age_deployment_class, logger_type) %>%
        group_by(colony, species, age_deployment_class, logger_type)
    if (event_type == "deployment") {
        history_table_summary <- history_table %>%
            summarise(prev_deployed = n(), .groups = "drop")
    } else if (event_type == "retrieval") {
        history_table_summary <- history_table %>%
            summarise(prev_retrieved = n(), .groups = "drop")
    }
    history_table_summary <- history_table_summary %>% collect()

    history_table_summary$year <- history_year
    history_table_summary$logger_type <- as.character(history_table_summary$logger_type)

    return(history_table_summary)
}
