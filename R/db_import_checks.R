fix_num_col <- function(num_col) {
    if (!is.numeric(num_col)) {
        num_col <- gsub(",", ".", num_col, fixed = TRUE)
        num_col <- gsub("[^0-9.]", "", num_col)
        num_col <- as.numeric(num_col)
    }
    return(num_col)
}

check_startup_rows <- function(startup_shutdown) {
    if (nrow(startup_shutdown) == 0) {
        stop("No valid sessions! Cannot proceed with database import.")
    }
}

check_shutdown <- function(startup_shutdown) {
    # remove cases with no startup date. For some loggers, this should be inferred from the deployment date. This can happen here.
    original_count <- nrow(startup_shutdown)

    # Check shutdowns
    problem_shutdown <- startup_shutdown[is.na(startup_shutdown$starttime_gmt), ]
    if (nrow(problem_shutdown) > 0) {
        startup_shutdown <- startup_shutdown[!is.na(startup_shutdown$starttime_gmt), ]
        log_warn(paste("Removed ", original_count - nrow(startup_shutdown), " rows with no startup date."))

        row_summary <- problem_shutdown[, c("logger_serial_no", "starttime_gmt", "download_date", "shutdown_date")]
        log_warn("The following rows have no startup date and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
    }
    return(startup_shutdown)
}

check_euring_code <- function(metadata) {
    # check euring code
    problem_ring_bool <- is.na(metadata$ring_number) | is.na(metadata$euring_code)
    if (sum(problem_ring_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_ring_bool), " rows with missing ring number or euring code."))
        row_summary <- metadata[problem_ring_bool, c("date", "ring_number", "euring_code")]
        log_warn("The following rows have missing ring number or euring code and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!problem_ring_bool, ]
    }
    return(metadata)
}

fix_people_strings <- function(people_strings) {
    people_strings <- trimws(people_strings)
    people_strings <- gsub("/", "_", people_strings, fixed = TRUE)
    people_strings <- gsub(" & ", "_", people_strings, fixed = TRUE)
    people_strings <- gsub(", ", "_", people_strings, fixed = TRUE)
    people_strings <- gsub(" _ ", "_", people_strings, fixed = TRUE)
    return(people_strings)
}

check_people_startup_shutdown <- function(startup_shutdown) {
    startup_shutdown <- check_people_startup_col(startup_shutdown, "intended_deployer")
    startup_shutdown <- check_people_startup_col(startup_shutdown, "started_by")
    return(startup_shutdown)
}

check_people_startup_col <- function(startup_shutdown, col_name) {
    startup_shutdown[[col_name]] <- fix_people_strings(startup_shutdown[[col_name]])

    unique_people <- unique(startup_shutdown[[col_name]])

    unique_problem_names_bool <- sapply(unique_people, seatrackR::checkPeople)
    problem_names_bool <- !unique_problem_names_bool[match(startup_shutdown[[col_name]], unique_people)]


    if (sum(problem_names_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_names_bool), " rows with invalid ", col_name, " names."))
        row_summary <- startup_shutdown[problem_names_bool, c("logger_serial_no", "starttime_gmt", col_name)]
        log_warn(glue::glue("The following rows have invalid {col_name} value and will not be handled"), ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        startup_shutdown <- startup_shutdown[!problem_names_bool, ]
    }
    return(startup_shutdown)
}

check_people_metadata <- function(metadata) {
    metadata$data_responsible <- fix_people_strings(metadata$data_responsible)

    unique_people <- unique(metadata$data_responsible)
    unique_problem_names_bool <- sapply(unique_people, seatrackR::checkPeople)
    problem_names_bool <- !unique_problem_names_bool[match(metadata$data_responsible, unique_people)] | is.na(metadata$data_responsible)


    if (sum(problem_names_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_names_bool), " rows with invalid data_responsible names."))
        row_summary <- metadata[problem_names_bool, c("date", "ring_number", "data_responsible", "logger_id_deployed", "logger_id_retrieved")]
        log_warn("The following rows have invalid data_responsible value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!problem_names_bool, ]
    }
    return(metadata)
}

check_sex_metadata <- function(metadata) {
    valid_sex <- c("male", "female", "unknown", NA)
    sex_alias <- data.frame(db_name = c("male", "female", "unknown"), alias = c("m", "f", "u"))
    metadata$sex <- tolower(metadata$sex)
    invalid_sex_bool <- !metadata$sex %in% valid_sex
    if (sum(invalid_sex_bool) > 0) {
        metadata$sex[invalid_sex_bool] <- sex_alias$db_name[match(metadata$sex[invalid_sex_bool], sex_alias$alias)]
        invalid_sex_bool <- !metadata$sex %in% valid_sex
        problem_sex <- metadata[invalid_sex_bool, ]
        if (nrow(problem_sex) > 0) {
            metadata <- metadata[!invalid_sex_bool, ]
            log_warn(paste("Removed ", nrow(problem_sex), " rows with invalid sex."))
            row_summary <- problem_sex[, c("date", "ring_number", "sex", "logger_id_deployed", "logger_id_retrieved")]
            log_warn("The following rows have invalid sex value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        }
        metadata <- metadata[!invalid_sex_bool, ]
    }
    return(metadata)
}

fix_metadata_latlon <- function(metadata) {
    lat_lon_cols <- c("colony_latitude", "colony_longitude", "nest_latitude", "nest_longitude")
    for (col in lat_lon_cols) {
        # If column exists
        if (col %in% names(metadata)) {
            # Remove non-numeric characters and convert to numeric
            metadata[[col]] <- as.numeric(gsub("[^0-9.-]", "", metadata[[col]]))
        }
    }
    return(metadata)
}

check_metadata_breeding_stage <- function(metadata) {
    db_breeding_table <- dplyr::tbl(con, dbplyr::in_schema("metadata", "breeding_stages"))
    valid_breeding_stages <- dplyr::pull(db_breeding_table, "breeding_stage")
    invalid_breeding_bool <- !is.na(metadata$breeding_stage) & !metadata$breeding_stage %in% valid_breeding_stages
    if (sum(invalid_breeding_bool) > 0) {
        problem_breed <- metadata[invalid_breeding_bool, ]
        log_warn(paste("Removed ", nrow(problem_breed), " rows with invalid breeding stages."))
        row_summary <- problem_breed[, c("date", "ring_number", "breeding_stage", "logger_id_deployed", "logger_id_retrieved")]
        log_warn("The following rows have invalid breeding_stage value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!invalid_breeding_bool, ]
    }
    return(metadata)
}

fix_metadata_back_on_nest <- function(metadata) {
    back_on_nest <- metadata$back_on_nest
    back_on_nest[back_on_nest == "yes"] <- TRUE
    back_on_nest[back_on_nest == "no"] <- FALSE
    metadata$back_on_nest <- as.logical(back_on_nest)
    return(metadata)
}

check_metadata_colony <- function(metadata) {
    db_locs <- seatrackR::getColonies(allLocations = TRUE)$location_name
    problem_colony_bool <- !metadata$colony %in% db_locs
    if (sum(problem_colony_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_colony_bool), " rows with invalid location names."))
        row_summary <- metadata[problem_colony_bool, c("date", "ring_number", "colony", "logger_id_deployed", "logger_id_retrieved")]
        log_warn("The following rows have invalid location/colony value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!problem_colony_bool, ]
    }
    return(metadata)
}

check_metadata_mounting <- function(metadata) {
    metadata$logger_mount_method[!is.na(metadata$logger_mount_method)] <- tolower(metadata$logger_mount_method[!is.na(metadata$logger_mount_method)])

    db_mounting_table <- dplyr::tbl(con, dbplyr::in_schema("metadata", "mounting_types"))
    valid_mountings <- dplyr::pull(db_mounting_table, "logger_mount_method")
    invalid_mounting_bool <- !is.na(metadata$logger_mount_method) & !metadata$logger_mount_method %in% valid_mountings
    if (sum(invalid_mounting_bool) > 0) {
        problem_mount <- metadata[invalid_mounting_bool, ]
        log_warn(paste("Removed ", nrow(problem_mount), " rows with invalid mounting types."))
        row_summary <- problem_mount[, c("date", "ring_number", "logger_mount_method", "logger_id_deployed", "logger_id_retrieved")]
        log_warn("The following rows have invalid mounting value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!invalid_mounting_bool, ]
    }
    return(metadata)
}

check_startup_species <- function(startup_shutdown) {
    db_species <- seatrackR::getSpecies()$species_name_eng
    startup_shutdown$intended_species <- gsub(" adults", "", startup_shutdown$intended_species)
    startup_shutdown$intended_species <- gsub(" chicks", "", startup_shutdown$intended_species)

    problem_species_bool <- !startup_shutdown$intended_species %in% c(db_species, NA)
    if (sum(problem_species_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_species_bool), " rows with invalid species."))
        row_summary <- startup_shutdown[problem_species_bool, c("starttime_gmt", "logger_serial_no", "intended_species")]
        log_warn("The following rows have invalid species value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        startup_shutdown <- startup_shutdown[!problem_species_bool, ]
    }

    return(startup_shutdown)
}

check_metadata_species <- function(metadata) {
    db_species <- seatrackR::getSpecies()$species_name_eng
    problem_species_bool <- !metadata$species %in% db_species

    if (sum(problem_species_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_species_bool), " rows with invalid species."))
        row_summary <- metadata[problem_species_bool, c("date", "ring_number", "species", "logger_id_deployed", "logger_id_retrieved")]
        log_warn("The following rows have invalid species value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!problem_species_bool, ]
    }

    return(metadata)
}

check_metadata_species_match <- function(metadata) {
    species_ring <- dplyr::distinct(metadata, species, ring_number)
    problem_rings <- species_ring$ring_number[duplicated(species_ring$ring_number)]
    if (length(problem_rings) > 0) {
        problem_rings_bool <- metadata$ring_number %in% problem_rings
        log_warn(paste("Removed ", sum(problem_rings_bool), " rows with species/ring number mismatch."))
        row_summary <- metadata[problem_rings_bool, c("date", "ring_number", "species")]
        log_warn("The following rows have species/ring number mismatch and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!problem_rings_bool, ]
    }
    return(metadata)
}

fix_logger_models <- function(metadata, startup_shutdown) {
    db_logger_model <-
        data.frame(
            logger_serial_no = c(metadata$logger_id_deployed, metadata$logger_id_retrieved, startup_shutdown$logger_serial_no),
            logger_model = c(metadata$logger_model_deployed, metadata$logger_model_retrieved, startup_shutdown$logger_model)
        )

    db_true_logger_model <- get_db_models(db_logger_model)

    if (!is.null(db_logger_model)) {
        metadata$logger_model_retrieved[metadata$logger_id_retrieved %in% db_true_logger_model$logger_serial_no] <- db_true_logger_model$logger_model[match(metadata$logger_id_retrieved[metadata$logger_id_retrieved %in% db_true_logger_model$logger_serial_no], db_true_logger_model$logger_serial_no)]
        metadata$logger_model_deployed[metadata$logger_id_deployed %in% db_true_logger_model$logger_serial_no] <- db_true_logger_model$logger_model[match(metadata$logger_id_deployed[metadata$logger_id_deployed %in% db_true_logger_model$logger_serial_no], db_true_logger_model$logger_serial_no)]
        startup_shutdown$logger_model[startup_shutdown$logger_serial_no %in% db_true_logger_model$logger_serial_no] <- db_true_logger_model$logger_model[match(startup_shutdown$logger_serial_no[startup_shutdown$logger_serial_no %in% db_true_logger_model$logger_serial_no], db_true_logger_model$logger_serial_no)]
        startup_shutdown$production_year[startup_shutdown$logger_serial_no %in% db_true_logger_model$logger_serial_no] <- db_true_logger_model$production_year[match(startup_shutdown$logger_serial_no[startup_shutdown$logger_serial_no %in% db_true_logger_model$logger_serial_no], db_true_logger_model$logger_serial_no)]
    }

    # Check for invalid logger models and producers
    # Check for logger models that don't exist in the database
    logger_fix_result <- check_model_producer(metadata, startup_shutdown)

    check_model_mismatch(logger_fix_result$metadata, logger_fix_result$startup_shutdown)

    return(list(metadata = metadata, startup_shutdown = startup_shutdown))
}

check_model_producer <- function(metadata, startup_shutdown) {
    db_model_producer <- seatrackR::getLoggerModels()
    problem_logger_models <- unique(startup_shutdown$logger_model[!startup_shutdown$logger_model %in% db_model_producer$model])
    n_problem_logger_models <- length(problem_logger_models)
    if (n_problem_logger_models > 0) {
        # Try a case insensitive match of these models
        correct_case <- db_model_producer$model[match(tolower(problem_logger_models), tolower(db_model_producer$model))]
        fixable <- data.frame(md_model = problem_logger_models[!is.na(correct_case)], db_model = correct_case[!is.na(correct_case)])

        metadata$logger_model_retrieved[metadata$logger_model_retrieved %in% fixable$md_model] <- fixable$db_model[match(metadata$logger_model_retrieved[metadata$logger_model_retrieved %in% fixable$md_model], fixable$md_model)]
        metadata$logger_model_deployed[metadata$logger_model_deployed %in% fixable$md_model] <- fixable$db_model[match(metadata$logger_model_deployed[metadata$logger_model_deployed %in% fixable$md_model], fixable$md_model)]
        startup_shutdown$logger_model[startup_shutdown$logger_model %in% fixable$md_model] <- fixable$db_model[match(startup_shutdown$logger_model[startup_shutdown$logger_model %in% fixable$md_model], fixable$md_model)]

        problem_logger_models <- problem_logger_models[!problem_logger_models %in% fixable$md_model]
    }

    n_problem_logger_models <- length(problem_logger_models)
    if (n_problem_logger_models > 0) {
        id_problem_startups <- startup_shutdown$logger_model %in% problem_logger_models
        missing_model_summary <- startup_shutdown[id_problem_startups, c("logger_serial_no", "logger_model", "starttime_gmt")]
        log_warn(glue::glue("{nrow(missing_model_summary)} logging sessions have logger models not present in database."), "\n", paste(capture.output(print(missing_model_summary, n = nrow(missing_model_summary)))[c(-1, -3)], collapse = "\n"))
        startup_shutdown <- startup_shutdown[!id_problem_startups, ]
    }

    startup_shutdown$producer <- db_model_producer$producer[match(startup_shutdown$logger_model, db_model_producer$model)]

    return(list(metadata = metadata, startup_shutdown = startup_shutdown))
}

check_model_mismatch <- function(metadata, startup_shutdown) {
    # Check for remaining model mismatch
    db_logger_model <-
        data.frame(
            logger_serial_no = c(metadata$logger_id_deployed, metadata$logger_id_retrieved, startup_shutdown$logger_serial_no),
            logger_model = c(metadata$logger_model_deployed, metadata$logger_model_retrieved, startup_shutdown$logger_model)
        )
    db_logger_model <- db_logger_model[!is.na(db_logger_model$logger_serial_no), ]

    problem_logger_models_bool <- sapply(unique(db_logger_model$logger_serial_no), function(logger_serial_no) {
        length(unique(db_logger_model$logger_model[db_logger_model$logger_serial_no == logger_serial_no])) > 1
    })

    problem_logger_ids <- unique(db_logger_model$logger_serial_no)[problem_logger_models_bool]
    problem_logger_model_string <- sapply(problem_logger_ids, function(logger_serial_no) {
        logger_model_string <- unique(db_logger_model$logger_model[db_logger_model$logger_serial_no == logger_serial_no])
        logger_model_strings <- sapply(logger_model_string, function(x) {
            glue::glue("'{x}'")
        })
        paste0(logger_serial_no, ": ", paste(logger_model_strings, collapse = ", "))
    })
    if (length(problem_logger_ids) > 0) {
        log_warn(paste0("The following logger serial numbers have multiple different logger models in the master sheet. This could cause a database error.:\n", paste(problem_logger_model_string, collapse = "\n")))
    }
}

get_db_models <- function(db_logger_model) {
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
        db_true_logger_model <- get_db_metadata_import(data.frame(logger_serial_no = unique(mismatch_logger_models$logger_serial_no)), "loggers.logger_info", additional_db_col_names = c("logger_model", "production_year"))
        n_mismatch <- nrow(db_true_logger_model)
        if (n_mismatch > 0) {
            model_summary <- tibble::as_tibble(db_true_logger_model)
            ambiguity_loggers <- unique(model_summary$logger_serial_no[duplicated(model_summary$logger_serial_no)])
            n_ambiguity <- length(ambiguity_loggers)
            if (n_ambiguity > 0) {
                ambiguity_loggers_summary <- model_summary[model_summary$logger_serial_no %in% ambiguity_loggers, ]
                log_warn(
                    glue::glue("{n_ambiguity} logger serial numbers have a mismatch between the master sheet. \n However, there are multiple different logger models for these serial numbers in the database."), "\n",
                    paste(capture.output(print(ambiguity_loggers_summary, n = nrow(ambiguity_loggers_summary)))[c(-1, -3)], collapse = "\n")
                )
                log_warn("These cannot be corrected from the database. Master metadata must be updated to resolve this ambiguity.")
            }
            model_summary <- model_summary[!model_summary$logger_serial_no %in% ambiguity_loggers, ]

            model_summary$md_logger_model <- missing_logger_models$logger_model[match(model_summary$logger_serial_no, missing_logger_models$logger_serial_no)]
            model_summary <- model_summary[model_summary$md_logger_model != model_summary$logger_model | is.na(model_summary$md_logger_model), ]
            log_warn(glue::glue("{n_mismatch} logger deployments/retrievals had a mismatch between the master sheet and the database."), "\n", paste(capture.output(print(model_summary, n = nrow(model_summary)))[c(-1, -3)], collapse = "\n"))
            log_warn("Database value will be used. Consider updating master metadata")
            return(db_true_logger_model)
        }
    }
    return(NULL)
}

check_startup_prod_year <- function(startup_shutdown) {
    # Check production_year
    problem_production_year_bool <- is.na(startup_shutdown$production_year) | startup_shutdown$production_year < 1900 | startup_shutdown$production_year > as.numeric(format(Sys.Date(), "%Y"))
    if (sum(problem_production_year_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_production_year_bool), " rows with invalid production year."))
        row_summary <- startup_shutdown[problem_production_year_bool, c("logger_serial_no", "logger_model", "production_year", "starttime_gmt")]
        log_warn("The following rows have invalid production year and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        startup_shutdown <- startup_shutdown[!problem_production_year_bool, ]
    }
    return(startup_shutdown)
}

check_startup_download <- function(startup_shutdown) {
    startup_shutdown$download_type <- translate_logger_status(startup_shutdown$download_type)
    # Theoretically no other download statuses should be allowed, but perhaps check?
    db_download_status <- translate_logger_status(list_valid_only = TRUE)
    problem_download_bool <- !startup_shutdown$download_type %in% c(db_download_status) & (!is.na(startup_shutdown$download_date) | !is.na(startup_shutdown$shutdown_date))
    if (sum(problem_download_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_download_bool), " rows with invalid download status."))
        row_summary <- startup_shutdown[problem_download_bool, c("starttime_gmt", "logger_serial_no", "download_type")]
        log_warn("The following rows have invalid download_type value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        startup_shutdown <- startup_shutdown[!problem_download_bool, ]
    }
    return(startup_shutdown)
}
