#' Read and clean the field plan data
#'
#' This function processes the field plan data by selecting relevant columns, renaming them, calculating deployment and retrieval success rates,
#' and merging with historical data to compute previous deployments. A connection to the SEATRACK database is required.
#' @param field_plan_sheet A data frame containing the raw field plan data
#' @param use_master_sheets A boolean indicating whether to use master sheets to calculate success.
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
get_clean_field_plan <- function(field_plan_sheet, use_master_sheets = FALSE, all_locations = NULL, use_db = TRUE, field_year = c(as.numeric(format(Sys.Date(), "%Y")))) {
    # classify models
    gps_models <- c(
        "NanoFix_GEO_mini",
        "PicoFix_GEO_mini2",
        "NanoFix_GEO_mini",
        "PicoFix_GEO_mini3",
        "050522_NanoFixAccM_20",
        "NanoFix_GEO_GPS",
        "NanoFix_GEO_mini_5.5g",
        "picofix"
    )

    gsm_models <- c("OrniTrack15", "OrniTrack10")

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
        # Shift to function
        if (is.null(all_locations)) {
            all_locations <- load_all_master_import(skip = c(c("Blomstrand", "Keysite Vestland", "Lowestoft", "Iceland_processed_metadata", "not_processed", "no_location_not_processed")))
        }

        startups <- all_locations$STARTUP_SHUTDOWN
        all_metadata <- all_locations$METADATA
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
        db_loggers <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
        db_status_filtered <- db_status %>%
            dplyr::group_by(ring_number, logger_id) %>%
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

        missing_rows <- dplyr::anti_join(metadata_summary, field_plan_clean, metadata_summary, by = c("Species", "Location", "logger_type", "age"))
        missing_rows$planned <- 0
        missing_rows$source <- "metadata"
        missing_rows$LME <- NA
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
        db_deployments <- dplyr::tbl(con, dbplyr::in_schema("loggers", "deployment"))
        db_retrievals <- dplyr::tbl(con, dbplyr::in_schema("loggers", "retrieval"))
        db_logger_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
        db_deployments <- dplyr::left_join(db_deployments, db_logger_info, by = "logger_id")
        db_retrievals <- dplyr::left_join(db_retrievals, db_logger_info, by = "logger_id")
        db_status <- dplyr::tbl(con, dbplyr::in_schema("individuals", "individ_status"))

        db_deployments <- dplyr::filter(db_deployments, lubridate::year(deployment_date) %in% field_year)
        db_retrievals <- dplyr::filter(db_retrievals, lubridate::year(retrieval_date) %in% field_year)

        db_status_filtered <- db_status %>%
            dplyr::group_by(session_id, logger_id) %>%
            dplyr::slice_min(status_date, n = 1, with_ties = FALSE) %>%
            dplyr::ungroup()

        db_deployments <- dplyr::left_join(db_deployments, db_status_filtered, by = "session_id")
        db_retrievals <- dplyr::left_join(db_retrievals, db_status_filtered, by = "session_id")

        db_deployments <- dplyr::mutate(db_deployments,
            age_class = ifelse(tolower(age) %in% c("pullus", "chick", "pull") & !is.na(age), "C", "A"),
            logger_type = dplyr::case_when(logger_model %in% gps_models ~ "GPS", logger_model %in% gsm_models ~ "GPS-GSM", .default = "GLS")
        )

        db_retrievals <- dplyr::mutate(db_retrievals,
            age_class = ifelse(tolower(age) %in% c("pullus", "chick", "pull") & !is.na(age), "C", "A"),
            logger_type = dplyr::case_when(logger_model %in% gps_models ~ "GPS", logger_model %in% gsm_models ~ "GPS-GSM", .default = "GLS")
        )

        db_deployments_summary <- dplyr::group_by(db_deployments, location, species, logger_type, age_class) %>%
            dplyr::summarise(deployed_db = n(), .groups = "drop") %>%
            dplyr::rename(age = "age_class", Location = "location", Species = "species") %>%
            dplyr::collect()

        db_retrievals_summary <- dplyr::group_by(db_retrievals, location, species, logger_type, age_class) %>%
            dplyr::summarise(retrieved_db = n(), .groups = "drop") %>%
            dplyr::rename(age = "age_class", Location = "location", Species = "species") %>%
            dplyr::collect()

        field_plan_clean$deployed_db <- dplyr::left_join(field_plan_clean, db_deployments_summary, by = join_by("Location", "age", "Species", "logger_type")) %>% pull("deployed_db")
        field_plan_clean$retrieved_db <- dplyr::left_join(field_plan_clean, db_retrievals_summary, by = join_by("Location", "age", "Species", "logger_type")) %>% pull("retrieved_db")

        valid_db_deployed_bool <- !is.na(field_plan_clean$deployed_db) & !is.na(field_plan_clean$deployed_metadata) & field_plan_clean$deployed_db >= field_plan_clean$deployed_metadata
        valid_db_retrieved_bool <- !is.na(field_plan_clean$retrieved_db) & !is.na(field_plan_clean$retrieved_metadata) & field_plan_clean$retrieved_db >= field_plan_clean$retrieved_metadata

        valid_db_bool <- valid_db_deployed_bool & valid_db_retrieved_bool

        field_plan_clean$deployed[valid_db_bool] <- field_plan_clean$deployed_db[valid_db_bool]
        field_plan_clean$retrieved[valid_db_bool] <- field_plan_clean$retrieved_db[valid_db_bool]

        field_plan_clean$source[valid_db_bool] <- "database"
    }

    # Deployment success
    field_plan_clean$dep_success <- field_plan_clean$deployed / field_plan_clean$planned

    field_plan_clean$dep_success[is.infinite(field_plan_clean$dep_success)] <- NA

    # Retrieval success
    retrieval_target_year <- min(field_year) - 1
    history_table <- get_history_table(retrieval_target_year, gps_models, gsm_models, "Retrieval")


    history_table_groups <- group_by(history_table, colony, deployment_species, age, logger_type)
    history_table_summary <- summarise(history_table_groups, prev_deployed = n(), .groups = "drop")

    field_plan_clean <- left_join(field_plan_clean, history_table_summary, by = join_by(
        Location == colony,
        Species == deployment_species,
        age == age,
        logger_type == logger_type
    ))

    field_plan_clean$ret_success <- field_plan_clean$retrieved / field_plan_clean$prev_deployed
    field_plan_clean$ret_success[is.infinite(field_plan_clean$ret_success)] <- NA

    # reorder to match other tables
    reorder_vector <- c("LME", "Location", "Species", "age", "logger_type", "planned", "retrieved", "deployed", "prev_deployed", "ret_success", "dep_success")
    reorder_vector <- c(reorder_vector, colnames(field_plan_clean)[!colnames(field_plan_clean) %in% reorder_vector])
    field_plan_clean <- field_plan_clean[, reorder_vector]

    field_plan_clean <- field_plan_clean[(!is.na(field_plan_clean$planned) & field_plan_clean$planned > 0) | (!is.na(field_plan_clean$retrieved) & field_plan_clean$retrieved > 0) | (!is.na(field_plan_clean$deployed) & field_plan_clean$deployed > 0), ]

    return(field_plan_clean)
}


#' Get historical deployment and retrieval data
#'
#' This function retrieves historical deployment and retrieval data from the SEATRACK database for a specified year and event type (Deployment or Retrieval).
#' It filters the data based on the event type, age class, and logger type, and returns a data frame with relevant information.
#' A connection to the SEATRACK database is required. This should probably be moved to seatrackRdb at some point.
#' @param history_year The year for which to retrieve historical data (e.g., 2023)
#' @param event_type A vector specifying the event type(s) to filter by (defaults to c("Deployment", "Retrieval"))
#' @return A data frame containing historical deployment and retrieval data
#' @examples
#' \dontrun{
#' seatrackR::connectSeatrack()
#' history_data <- get_history_table(2023, c("Deployment", "Retrieval"))
#' }
#' @keywords internal
get_history_table <- function(history_year, gps_models, gsm_models, event_type = c("Deployment", "Retrieval")) {
    seatrackR:::checkCon()

    # Rewrite to do this in the db.

    # Get all individuals (seems fast, but a tad wasteful in terms of memory)
    individual_info <- seatrackR::getIndividInfo()
    individual_info <- individual_info[!is.na(individual_info$eventType), ]

    # Filter to desired event type and age class.
    individual_events <- individual_info[individual_info$eventType %in% event_type, ]


    logger_info <- seatrackR::getLoggerInfo()
    logger_history <- logger_info[logger_info$session_id %in% individual_events$session_id, ]
    logger_history <- logger_history[!is.na(logger_history$deployment_date), ]

    logger_history$year <- format(logger_history$deployment_date, "%Y")
    logger_history_year <- logger_history[logger_history$year == history_year, ]

    # Try to translate between the age classes
    logger_history_year <- left_join(logger_history_year, individual_info[, c("session_id", "status_age")], by = "session_id")
    logger_history_year$age <- "A"
    logger_history_year$age[!is.na(logger_history_year$status_age) & logger_history_year$status_age %in% c("pullus", "juvenile", "chick")] <- "C"

    # Add type of logger
    logger_history_year$logger_type <- "GLS"

    logger_history_year$logger_type[logger_history_year$logger_model %in% gps_models] <- "GPS"
    logger_history_year$logger_type[logger_history_year$logger_model %in% gsm_models] <- "GPS-GSM"

    return(logger_history_year)
}

#' Read and process the field plan Excel sheet
#'
#' This function reads the specified Excel sheet, cleans the data by removing rows with missing values in key columns,
#' converts certain columns to numeric, removes bracketed text from colony names, and combines duplicate locations/species.
#'
#' @param field_plan_path The file path to the field plan Excel sheet
#' @return A data frame containing the processed field plan data
#' @examples
#' \dontrun{
#' get_field_plan("path/to/fieldplan.xlsx")
#' }
#' @export
#' @concept field_planning
get_field_plan <- function(field_plan_path) {
    field_plan_sheet <- read_excel(field_plan_path, sheet = "Main", na = c("", "?"))
    field_plan_sheet$`Ocean area` <- as.character(field_plan_sheet$`Ocean area`)
    field_plan_sheet <- field_plan_sheet[!is.na(field_plan_sheet$Colony) & !is.na(field_plan_sheet$`Ocean area`), ]

    # Force some columns to be numeric
    suppressWarnings({
        field_plan_sheet$`GLS deployments` <- as.numeric(field_plan_sheet$`GLS deployments`)
        field_plan_sheet$`GLS retrievals` <- as.numeric(field_plan_sheet$`GLS retrievals`)
    })

    # remove any strings in brackets from the colony name e.g "\u00C5gotnes (instead of Lyngøy)"" becomes "\u00C5gotnes"
    field_plan_sheet$Colony <- gsub(" \\(.*\\)", "", field_plan_sheet$Colony)

    # Combine duplicate locations/species e.g. Herring gulls instrumented at two sites in Nord-Troms, and assigned two rows in the planning sheet get combined into one.
    field_plan_sheet <- field_plan_combine_locations(field_plan_sheet)

    return(field_plan_sheet)
}

#' Combine duplicate locations/species in the field plan sheet
#'
#' This function aggregates locations with the same colony name, species and age together by summing all numeric variables.
#'
#' @param field_plan_sheet A data frame containing the field plan data
#' @return A data frame with combined locations/species
#' @examples
#' \dontrun{
#' field_plan_combine_locations(field_plan_sheet)
#' }
#' @export
#' @concept field_planning
field_plan_combine_locations <- function(field_plan_sheet) {
    # Aggregate locations with the same colony name, species and age together (sum all numeric variables)
    colony_species <- group_by(field_plan_sheet, Colony, Species, Age)
    colony_species_summary <- summarise(colony_species, across(
        where(is.numeric),
        function(x) {
            sum(x, na.rm = TRUE)
        }
    ), .groups = "drop")
    new_field_plan_sheet <- left_join(colony_species_summary, field_plan_sheet, suffix = c("", "._x_"), by = c("Colony", "Species", "Age"), multiple = "first")
    new_field_plan_sheet <- select(new_field_plan_sheet, -ends_with("._x_"))
    new_field_plan_sheet <- new_field_plan_sheet[, names(field_plan_sheet)]
    return(new_field_plan_sheet)
}

#' Check and update locations in the field plan sheet
#'
#' This function checks the locations in the field plan sheet against the SEATRACK database table of colonies, updates the sheet with new locations if provided,
#' calculates the distance of each ocean area from Trondheim, and orders the ocean areas by latitude and distance from Trondheim.
#' @param field_plan_sheet A data frame containing the field plan data
#' @param new_locations An optional data frame containing new locations with columns 'colony' and 'lat'
#' @return A data frame with updated and ordered locations
#' @examples
#' \dontrun{
#' seatrackR::connectSeatrack()
#' field_plan_sheet <- get_field_plan("path/to/fieldplan.xlsx")
#' new_locations <- data.frame(colony = c("Nord-Troms", "\u00C5gotnes"), lat = c(70, 60.40))
#' field_plan_check_locations(field_plan_sheet, new_locations)
#' }
#' @export
#' @concept field_planning
field_plan_check_locations <- function(field_plan_sheet, new_locations = NULL) {
    # Check if there is a seatrack DB data connection
    seatrackR:::checkCon()

    # Get all the colonies, load the geometries
    # Should declare con as a global variable to avoid R CMD check note
    all_colonies <- DBI::dbReadTable(con, DBI::Id(
        schema = "metadata",
        table = "colony"
    ))


    all_colonies$geom <- NULL

    field_plan_sheet <- left_join(field_plan_sheet, all_colonies, by = join_by("Colony" == "colony_int_name"))

    # Update the sheet with new locations
    if (!is.null(new_locations) && nrow(new_locations) > 0) {
        for (i in seq_len(nrow(new_locations))) {
            new_data <- new_locations[i, ]
            field_plan_sheet$lat[field_plan_sheet$Colony == new_data$colony & is.na(field_plan_sheet$lat)] <- new_data$lat
            if ("lon" %in% colnames(new_locations)) {
                field_plan_sheet$lat[field_plan_sheet$Colony == new_data$colony & is.na(field_plan_sheet$lon)] <- new_data$lon
            }
        }
    }

    if (any(is.na(field_plan_sheet$`Ocean area`))) {
        # First double check this colony doesn't appear elsewhere
        with_lme <- dplyr::filter(field_plan_sheet, !is.na(field_plan_sheet$`Ocean area`)) %>%
            dplyr::select(Colony, "Ocean area") %>%
            dplyr::distinct()
        field_plan_sheet$`Ocean area`[is.na(field_plan_sheet$`Ocean area`)] <- dplyr::left_join(dplyr::filter(field_plan_sheet, is.na(field_plan_sheet$`Ocean area`)), with_lme, by = "Colony", multiple = "first")$`Ocean area.y`

        polygons <- dplyr::tbl(con, dbplyr::in_schema("areas", "polygons"))

        missing_lme <- dplyr::filter(field_plan_sheet, is.na(field_plan_sheet$`Ocean area`)) %>%
            dplyr::select(Colony, lat, lon) %>%
            dplyr::distinct()

        dplyr::copy_to(con, missing_lme, "temp_points", temporary = TRUE, overwrite = TRUE)

        points_db <- tbl(con, "temp_points")

        result <- dplyr::tbl(con, sql("
            SELECT p.*, a.name
            FROM temp_points p
            JOIN areas.polygons a
                ON ST_Contains(a.geom,
                            ST_SetSRID(ST_MakePoint(p.lon, p.lat), 4326))
                AND a.group = 'LME66'
            ")) %>% dplyr::collect()

        field_plan_sheet$`Ocean area`[is.na(field_plan_sheet$`Ocean area`)] <- dplyr::left_join(dplyr::filter(field_plan_sheet, is.na(field_plan_sheet$`Ocean area`)), result, by = "Colony", multiple = "first")$name
    }

    field_plan_sheet <- field_plan_sheet[!is.na(field_plan_sheet$lat), ]

    # calculate distance of each ocean area from Trondheim (the center of the world)
    field_plan_sheet_ocean <- sf::st_as_sf(field_plan_sheet[!is.na(field_plan_sheet$lon), ], coords = c("lat", "lon"), remove = FALSE, crs = 4326)

    trondheim_coord <- sf::st_sfc(sf::st_point(c(10.3951, 63.4305)), crs = 4326)
    field_plan_sheet_ocean$distance_from_trondheim <- sapply(seq_len(nrow(field_plan_sheet_ocean)), function(i) {
        sf::st_distance(trondheim_coord, field_plan_sheet_ocean[i, ], by_element = TRUE)
    })

    # Get average latitude and longitude of each ocean area
    ocean_area_coords <- group_by(field_plan_sheet_ocean, `Ocean area`)
    ocean_area_coords <- slice_min(ocean_area_coords, order_by = distance_from_trondheim)
    ocean_area_coords <- summarise(ocean_area_coords,
        lat = mean(lat),
        lon = mean(lon),
        distance_from_trondheim = mean(distance_from_trondheim),
        .groups = "drop"
    )

    # Order the ocean areas by distance latitude and distance from Trondheim
    ocean_area_coords <- arrange(ocean_area_coords, desc(lat), distance_from_trondheim)

    field_plan_sheet$`Ocean area` <- factor(field_plan_sheet$`Ocean area`, levels = ocean_area_coords$`Ocean area`)

    field_plan_sheet <- arrange(field_plan_sheet, `Ocean area`, desc(lat))

    return(field_plan_sheet)
}
