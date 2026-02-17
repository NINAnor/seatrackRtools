get_col_from_list <- function(data_list, col_name) {
    for (curr_data in data_list) {
        if (col_name %in% names(curr_data)) {
            return(curr_data[[col_name]])
        }
    }
    return(NULL)
}

popmap_latin_to_english <- function(latin) {
    # For now. Better to have this in the database.
    species_names <- data.frame(
        latin = c(
            "Alle alle",
            "Fratercula arctica",
            "Fulmarus glacialis",
            "Rissa tridactyla",
            "Uria aalge",
            "Uria lomvia"
        ),
        common = c(
            "Little auk",
            "Atlantic puffin",
            "Northern fulmar",
            "Black-legged kittiwake",
            "Common guillemot",
            "Br\u00FCnnich's guillemot"
        )
    )
    return(species_names$common[species_names$latin == latin])
}

popmap_colonies_to_db <- function(popmap_colony, colony_list = getColonies()) {
    if (popmap_colony %in% colony_list$colony_int_name) {
        return(popmap_colony)
    }
    # Fuzzy match popmap colony names to SEATRACK colony names
    matched_colonies <- colony_list$colony_int_name[agrep(popmap_colony, colony_list$colony_int_name, max.distance = 4)]
    if (length(matched_colonies) == 0) {
        warning(paste0("No matching colony found for popmap colony: ", popmap_colony))
        return(popmap_colony)
    } else if (length(matched_colonies) > 1) {
        warning(paste0("Multiple matching colonies found for popmap colony: ", popmap_colony, ". Using first match: ", matched_colonies[1]))
    }
    return(matched_colonies[1])
}

#' Export a data request package
#'
#' This function will create a zip file of seatrack data. This includes:
#' - Writing each data type as a compressed parquet file
#' - Creating a README file with metadata about the data request
#' @param data_request_result List containing the result of a data request, which will contain all the arguments needed for the export. If this is provided, other arguments are ignored.
#' @param all_data A named list of lists containing `data`: a data.frame to be exported and `description`: A string description. Each name corresponds to a data type.
#' @param request_name A string representing the name of the data request.
#' @param output_dir An optional string specifying the directory where the zip file will be saved. Defaults to `requested_data_packages/<current_year>`.
#' @param additional_notes An optional string containing additional notes to be included in the README file.
#' @param species A character vector of species included in the data request. If NULL, it will be inferred from the data.
#' @param times A vector of two dates representing the start and end of the data request. If NULL, it will be inferred from the data.
#' @param colony A character vector of colonies included in the data request. If NULL, it will be inferred from the data.
#' @param additional_data_files An optional list of additional data files to include in the data directory of the exported zip file. Each element of the list should contain the file path to the file to be included and a description.
#' @param additional_files An optional list of additional files to include in the base directory of the exported zip file. Each element of the list should contain the file path to the file to be included and a description.
#' If NULL, it will be saved in a default location based on the current year.
#' @return None. The function creates a zip file in the specified output directory.
#' @concept data_requests
#' @export
export_data_package <- function(data_request_result = NULL, all_data = NULL, request_name = NULL, output_dir = NULL, species = NULL, colony = NULL, times = NULL, additional_notes = "", additional_data_files = list(), additional_files = list()) {
    creation_date <- Sys.Date()

    if (!is.null(data_request_result)) {
        all_data <- data_request_result$all_data
        request_name <- data_request_result$request_name
        output_dir <- data_request_result$output_dir
        species <- data_request_result$species
        colony <- data_request_result$colony
        times <- data_request_result$times
        additional_notes <- data_request_result$additional_notes
        additional_data_files <- data_request_result$additional_data_files
        additional_files <- data_request_result$additional_files
    }

    if (is.null(all_data)) {
        stop("Either all_data or data_request_result must be provided.")
    }

    if (is.null(request_name)) {
        stop("request_name must be provided.")
    }


    tmp_dir <- tempfile(pattern = paste0(request_name, "_"))
    dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(tmp_dir, "data"), recursive = TRUE)
    print(paste0("Creating data package in temporary directory: ", tmp_dir))
    file_list <- list()

    for (type in names(all_data)) {
        print(paste0("Writing data type: ", type))
        file_name <- paste0(request_name, "_", type, "_", creation_date, ".gz.parquet")
        file_list[[type]] <- list(path = file_name, description = all_data[[type]]$description)
        arrow::write_parquet(all_data[[type]]$data, file.path(tmp_dir, "data", file_name), compression = "gzip")
    }
    all_data_data_only <- lapply(all_data, function(x) {
        x$data
    })

    if (is.null(species)) {
        species <- get_col_from_list(all_data_data_only, "species")
        if (!is.null(species)) {
            species <- unique(species)
        }
    }

    if (is.null(times)) {
        times <- get_col_from_list(all_data_data_only, "date_time")
        if (!is.null(times)) {
            times <- c(min(as.Date(times)), max(as.Date(times)))
        }
    }

    if (is.null(colony)) {
        colony <- get_col_from_list(all_data_data_only, "colony")
        if (!is.null(colony)) {
            colony <- unique(colony)
        }
    }

    sessions <- unique(get_col_from_list(all_data_data_only, "session_id"))
    if (!is.null(sessions)) {
        data_responsible <- seatrackR::get_responsible(session = sessions)
    } else if (!is.null(species) && !is.null(colony)) {
        data_responsible <- seatrackR::get_responsible(species = species, colony = colony)
    } else {
        data_responsible <- NULL
    }

    if (length(additional_data_files) > 0) {
        db_colonies <- seatrackR::getColonies()
        for (additional_file in additional_data_files) {
            if (!file.exists(additional_file$path)) {
                warning(paste0("Additional file not found, skipping: ", additional_file$path))
                next
            }
            print(paste0("Adding additional data file to data package: ", additional_file$path))
            current_path <- additional_file$path
            current_description <- additional_file$description
            new_path <- file.path(tmp_dir, "data", basename(current_path))
            file.copy(current_path, new_path)
            # Try to extract metadata from additional_data_file
            # Check if this is a pop map
            if (grepl("Abundance_Model", additional_file$path)) {
                if (is.null(colony)) {
                    colony <- c()
                }
                if (is.null(species)) {
                    species <- c()
                }
                if (is.null(data_responsible)) {
                    data_responsible <- data.frame()
                }

                file_name <- basename(current_path)
                split_file_name <- strsplit(file_name, "_")[[1]]
                latin <- paste(split_file_name[c(4, 5)], collapse = " ")
                version <- paste(split_file_name[c(7, 8)], collapse = ".")
                common <- popmap_latin_to_english(latin)

                species <- c(species, common)
                loaded_netcdf <- ncdf4::nc_open(new_path)
                colony_names <- ncdf4::ncvar_get(loaded_netcdf, "SmcolName")
                ncdf4::nc_close(loaded_netcdf)
                colony_names <- gsub(" ", "_", colony_names)
                colony_names <- sapply(colony_names, popmap_colonies_to_db, colony_list = db_colonies)

                colony <- c(colony, colony_names)

                current_description <- glue::glue("SEATRACK population map {version} - {common}")

                popmap_responsible <- seatrackR::get_responsible(colony = colony_names, species = common)
                data_responsible <- rbind(data_responsible, popmap_responsible)
            }
            file_list[[paste("Abundance_model", version, common, sep = "_")]] <- list(path = basename(current_path), description = current_description)
        }
    }

    if (length(additional_files) > 0) {
        for (additional_file in additional_files) {
            if (!file.exists(additional_file$path)) {
                warning(paste0("Additional file not found, skipping: ", additional_file$path))
                next
            }
            print(paste0("Adding additional file to data package: ", additional_file$path))
            current_path <- additional_file$path
            new_path <- file.path(tmp_dir, basename(current_path))
            file.copy(current_path, new_path)
        }
    }


    # render markdown template to the temp dir
    create_readme(
        request_name = request_name,
        file_list = file_list,
        species = species,
        colonies = colony,
        times = times,
        data_responsible = data_responsible,
        data_dir = tmp_dir,
        additional_notes = additional_notes,
        additional_files = additional_files,
        output_file = file.path(tmp_dir, paste0("README_", request_name, "_", creation_date, ".html"))
    )

    if (is.null(output_dir)) {
        output_dir <- file.path("requested_data_packages", format(creation_date, "%Y"))
    }
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    print(paste0("Creating zip file in output directory: ", output_dir))
    zipfile <- file.path(output_dir, paste0(request_name, "_", creation_date, ".zip"))
    zip::zipr(
        zipfile = zipfile,
        files = tmp_dir,
        include_directories = TRUE,
        recurse = TRUE
    )
    print(paste0("Data package created: ", zipfile))
    unlink(tmp_dir)
    print("Temporary files cleaned up.")
}

#' Create SEATRACK documentation README
#'
#' This function generates a README file for a SEATRACK data request package using R Markdown.
#' The README includes metadata about the data request, such as species, colonies, time span, and additional notes.
#' @param request_name A string representing the name of the data request.
#' @param file_list A character vector of file names included in the data request.
#' @param species A character vector of species included in the data request.
#' @param colonies A character vector of colonies included in the data request.
#' @param times A vector of two dates representing the start and end of the data request
#' @param data_responsible Table of people responsible for the data
#' @param data_dir A string representing the directory where the data files are located.
#' @param additional_notes An optional string containing additional notes to be included in the README file
#' @param additional_files An optional list of additional files to include in the README file. Each element of the list should contain the file path to the file to be included and a description.
#' @param output_file A string representing the name and path under which the README file will be saved.
#' @return None. The function creates a README file in the specified data directory.
#' @concept data_requests
#' @export
create_readme <- function(request_name, file_list, species, colonies, times, data_responsible, data_dir, additional_notes = "", additional_files = list(), output_file = "README.html") {
    print("Creating README file...")
    print(file_list)
    rmarkdown::render(
        system.file("rmd", "README_template.Rmd", package = "seatrackRtools"),
        params = list(
            request_name = request_name,
            file_list = file_list,
            species = species,
            colonies = colonies,
            times = times,
            data_responsible = data_responsible,
            data_dir = data_dir,
            notes = additional_notes,
            additional_files = additional_files
        ),
        output_file = output_file,
        envir = new.env(),
        encoding = "UTF-8"
    )
}


#' Get SEATRACK data request
#'
#' This function retrieves SEATRACK data for a specified request, including position data, morphological and breeding data, and recordings of light, temperature, and activity.
#' If `export` is TRUE the `export_data_package` function is used to create a zip file containing the data. This includes writing each data type as a compressed parquet file and creating a README file with metadata about the data request.
#' If `export` is FALSE the function returns a named list of data.frames containing the requested data. This can be edited and passed to `export_data_package` later if desired.
#' Requires an active connection to the SEATRACK database.
#' Currently netCDF files are not included in the data request package, so these have to be injected using `additional_data_files` if needed.
#' @param request_name A string representing the name of the data request.
#' @param data_types A character vector specifying the types of data to include in the request. Possible values are "GLS_positional_data", "IRMA_positional_data", "individual_data", "light", "temperature", "activity", "population_maps" and "logger_info". Defaults to all types.
#' @param start_year An integer representing the start year for the data request.
#' @param end_year An integer representing the end year for the data request. Defaults to the current year.
#' @param species An optional string specifying the species to filter the data. If NULL, data for all species will be retrieved.
#' @param colony An optional string specifying the colony to filter the data. If NULL, data for all colonies will be retrieved.
#' @param export A boolean indicating whether to export the data package as a zip file. If FALSE, the function will return the data as a list instead.
#' @param output_dir An optional string specifying the directory where the exported zip file will be saved.
#' @param additional_notes An optional string containing additional notes to be included in the README file in the export.
#' @param additional_data_files An optional list of additional data files to include in the data directory of the exported zip file. Each element of the list should contain the file path to the file to be included and a description.
#' @param additional_files An optional list of additional files to include in the exported zip file. Each element of the list should contain the file path to the file to be included and a description.
#' If NULL, it will be saved in a default location based on the current year.
#' @return If `export` is `TRUE`: None. The function creates a zip file in the specified output directory.
#' If `export` is `FALSE`: A named list of data.frames containing the requested data.
#' @examples
#' \dontrun{
#' data_request("Mosbech_120925", 2023, 2025, "Common eider", "Christiansø")
#' }
#' @export
#' @concept data_requests
data_request <- function(
    request_name,
    data_types = c("GLS_positional_data", "IRMA_positional_data", "individual_data", "light", "temperature", "activity", "population_maps", "logger_info"),
    start_year = "2000", end_year = format(Sys.Date(), "%Y"), species = NULL, colony = NULL, export = TRUE, output_dir = NULL,
    additional_notes = "", additional_data_files = list(), additional_files = list()) {
    start_date <- as.Date(paste0(start_year, "-01-01"))
    end_date <- as.Date(paste0(end_year, "-12-31")) + 1
    all_data <- list()
    if (!is.vector(data_types)) {
        data_types <- c(data_types)
    }
    data_types <- match.arg(data_types, several.ok = TRUE)

    # check colonies
    if (!is.null(colony)) {
        available_colonies <- seatrackR::getColonies()$colony_int_name
        missing_colony_bool <- !colony %in% available_colonies
        if (any(missing_colony_bool)) {
            missing_colonies <- paste(colony[missing_colony_bool], collapse = ", ")
            stop(paste(c("Colonies", missing_colonies, "not in database."), collapse = " "))
        }
    }

    print(paste("Starting data request, datatypes are", paste(data_types, collapse = " ")))

    if ("GLS_positional_data" %in% data_types) {
        print("Fetching GLS position data...")
        all_pos <- seatrackR::getPositions(species = species, colony = colony)
        all_data$GLS_positional_data <- list(
            data = all_pos[all_pos$date_time >= start_date & all_pos$date_time < end_date, ],
            description = "GLS Positional data"
        )
    }

    if ("IRMA_positional_data" %in% data_types) {
        print("Fetching IRMA position data...")
        irma_pos <- seatrackR::getPositions(datatype = "IRMA", species = species, colony = colony)
        all_data$IRMA_positional_data <- list(
            data = irma_pos[irma_pos$date_time >= start_date & irma_pos$date_time < end_date, ],
            description = "IRMA Positional data"
        )
    }


    if (any(c("GLS_positional_data", "IRMA_positional_data", "individual_data", "light", "temperature", "activity") %in% data_types)) {
        print("Fetching individual data...")
        individuals <- seatrackR::getIndividInfo(colony = colony, year = NULL)
        if (!is.null(species)) {
            individuals <- individuals[individuals$species %in% species, ]
        }

        if ("individual_data" %in% data_types) {
            if ("GLS_positional_data" %in% names(all_data)) {
                indiv_ids <- unique(all_data$GLS_positional_data$data$individ_id)
                individuals <- individuals[individuals$individ_id %in% indiv_ids, ]
            } else if ("IRMA_positional_data" %in% names(all_data)) {
                indiv_ids <- unique(all_data$IRMA_positional_data$data$individ_id)
                individuals <- individuals[individuals$individ_id %in% indiv_ids, ]
            }
            all_data$individual_data <- list(data = individuals, description = "Individual information data")
        }
    }
    if ("logger_info" %in% data_types) {
        print("Fetching logger data...")
        logger_info <- seatrackR::getLoggerInfo()
        logger_info <- logger_info[logger_info$deployment_date >= start_date & logger_info$retrieval_date < end_date, ]
        if (!is.null(colony)) {
            logger_info <- logger_info[logger_info$colony %in% colony, ]
        }
        if (!is.null(species)) {
            logger_info <- logger_info[logger_info$deployment_species %in% species, ]
        }
        all_data$logger_info <- list(data = individuals, description = "Logger info")
    }

    descriptions <- c(
        light = "Light data", temperature = "Temperature data", activity = "Standardized immersion data"
    )
    types <- data_types[data_types %in% c("light", "temperature", "activity")]
    if (length(types) > 0) {
        activity_light_temp <- lapply(types, function(type) {
            print(paste0("Fetching ", type, " data..."))
            all_indivs <- lapply(individuals$individ_id, function(indiv_id) {
                recordings <- seatrackR::getRecordings(type = type, individId = indiv_id)
                recordings <- recordings[recordings$date_time >= start_date & recordings$date_time < end_date, ]
            })
            return(list(data = do.call(rbind, all_indivs), description = descriptions[type]))
        })
        names(activity_light_temp) <- types
        names(activity_light_temp)[names(activity_light_temp) == "activity"] <- "immersion"
        names(activity_light_temp) <- paste0(names(activity_light_temp), "_data")
        all_data <- c(all_data, activity_light_temp)
    }

    data_exists_bool <- sapply(all_data, function(x) {
        if (!is.null(x$data)) {
            if ("data.frame" %in% class(x$data)) {
                if (nrow(x$data) == 0) {
                    return(FALSE)
                }
            }
            return(TRUE)
        }
        return(FALSE)
    })
    # should log the removal of the data
    all_data <- all_data[data_exists_bool]

    if (any(c("GLS_positional_data", "light_data", "temperature_data", "immersion_data") %in% names(all_data))) {
        times <- NULL
    } else {
        times <- c(start_date, end_date)
    }

    if (any(c("GLS_positional_data", "individual_data") %in% names(all_data))) {
        species <- NULL
        colony <- NULL
    }

    data_request_result <- list(
        all_data = all_data,
        request_name = request_name,
        output_dir = output_dir,
        species = species,
        colony = colony,
        times = times,
        additional_notes = additional_notes,
        additional_data_files = additional_data_files,
        additional_files = additional_files
    )

    if (export) {
        print("Exporting data package...")
        export_data_package(
            data_request_result <- data_request_result
        )
    } else {
        return(
            data_request_result
        )
    }
}
