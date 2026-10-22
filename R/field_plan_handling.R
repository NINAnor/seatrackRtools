#' Get the path to the field plan Excel sheet
#'
#' This function retrieves the full path to the field plan Excel sheet for a specified year. It searches for the file in the given directory and returns the first match found. If no matching file is found, it raises an error.
#'
#' @param field_plan_path The directory path where the field plan Excel sheet is located (default is "Admin/Fieldplanning" within the SEATRACK folder)
#' @param field_year The year for which to retrieve the field plan (default is the current year)
#' @return The full path to the field plan Excel sheet
#' @concept field_planning
#' @export
get_field_plan_path <- function(field_plan_path = file.path(the$sea_track_folder, "Admin/Fieldplanning"), field_year = c(as.numeric(format(Sys.Date(), "%Y")))) {
    full_field_plan_path <- list.files(field_plan_path, recursive = TRUE, full.names = TRUE, pattern = paste0(".*", field_year, "_fieldsuksess.xlsx$"))[1]
    if (is.null(full_field_plan_path)) {
        stop(paste("Cannot find field plan for year", field_year))
    }
    return(full_field_plan_path)
}

#' Load a field plan sheet
#'
#' This function loads a field plan sheet from the specified path and year. If `return_df` is set to TRUE, it returns a data frame containing the processed field plan data; otherwise, it returns the workbook object.
#'
#' @param field_plan_path The file path to the field plan Excel sheet. Default is "Admin/Fieldplanning" within the SEATRACK folder.
#' @param field_year The year for which to load the field plan (default is the current year)
#' @param return_df A logical value indicating whether to return a data frame (TRUE) or the workbook object (FALSE). Default is TRUE.
#' @return A data frame containing the processed field plan data
#' @concept field_planning
#' @export
load_field_plan_sheet <- function(field_plan_path = file.path(the$sea_track_folder, "Admin/Fieldplanning"), field_year = c(as.numeric(format(Sys.Date(), "%Y"))), return_df = TRUE) {
    full_field_plan_path <- get_field_plan_path(field_plan_path, field_year)

    field_plan_wb <- openxlsx2::wb_load(full_field_plan_path)
    if (return_df) {
        return(get_field_plan_df(field_plan_wb))
    } else {
        return(field_plan_wb)
    }
}

#' Load and clean field plan data
#'
#' This function loads a field plan data dataframe from the specified path using `load_field_plan_sheet`, and then cleans the data using `get_clean_field_plan`.
#' It returns a cleaned data frame containing the field plan information for the specified year and target species.
#'
#' @param field_plan_path The file path to the field plan Excel sheet
#' @param field_year The year for which to load the field plan (default is the current year)
#' @param all_metadata Optional metadata to be used in cleaning the field plan. If not provided, the function will load master sheets from the SEATRACK folder.
#' @param target_species A vector of target species to filter the field plan data. Defaults to SEATRACK species list.
#' @return A cleaned data frame containing the field plan information
#'
#' @concept field_planning
#' @export
load_clean_field_plan <- function(field_plan_path = file.path(the$sea_track_folder, "Admin/Fieldplanning"),
                                  field_year = c(as.numeric(format(Sys.Date(), "%Y"))),
                                  all_metadata = NULL,
                                  target_species = c(
                                      "Atlantic puffin", "Brünnich's guillemot", "Common guillemot",
                                      "Little auk", "Razorbill", "Arctic tern", "Black-legged kittiwake", "Great skua",
                                      "Leach's Storm Petrel", "Northern fulmar", "Northern gannet", "Glaucous gull", "Herring gull",
                                      "Lesser black-backed gull", "European shag", "Common eider"
                                  )) {
    field_plan_sheet <- load_field_plan_sheet(field_plan_path, field_year)
    field_plan_clean <- get_clean_field_plan(field_plan_sheet, target_species, TRUE, all_metadata, TRUE, field_year)
    return(field_plan_clean)
}

#' Read and process a field plan Excel sheet
#'
#' This function reads a field plan Excel sheet and processes the data to ensure it is in the correct format. It performs a few basic checks and transformations, including converting certain columns to numeric, removing strings in brackets from colony names, combining duplicate locations/species, and checking locations against the SEATRACK database.
#' A database connection is required to check the locations against the SEATRACK database.
#' @param field_plan_wb The workbook object of the field plan Excel sheet
#' @return A data frame containing the processed field plan data
#' @examples
#' \dontrun{
#' get_field_plan_df("path/to/fieldplan.xlsx")
#' }
#' @export
#' @concept field_planning
get_field_plan_df <- function(field_plan_wb = NULL) {
    field_plan_sheet <- openxlsx2::wb_to_df(field_plan_wb, sheet = "Main", na.strings = c("", "?"))
    field_plan_sheet <- field_plan_sheet[, !is.na(colnames(field_plan_sheet))]

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

    log_info("Checking field plan locations...")
    field_plan_sheet <- field_plan_check_locations(field_plan_sheet)


    if (!"GPS assigned" %in% names(field_plan_sheet)) {
        field_plan_sheet$`GPS assigned` <- 0
    }

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
            if (any(!is.na(x))) {
                return(sum(x, na.rm = TRUE))
            } else {
                return(NA)
            }
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
field_plan_check_locations <- function(field_plan_sheet) {
    # Check if there is a seatrack DB data connection
    seatrackR:::checkCon()
    colonies <- dplyr::tbl(con, dbplyr::in_schema("metadata", "colony_details")) %>% collect()
    new_colony <- fix_colony(field_plan_sheet$Colony, TRUE)
    if (any(is.na(new_colony))) {
        problem_colony <- paste(unique(field_plan_sheet$Colony[is.na(new_colony)]), collapse = ", ")
        stop(glue::glue("The following colonies are not in the database: {problem_colony}"))
    }
    field_plan_sheet$Colony <- new_colony

    if (any(is.na(field_plan_sheet$`Ocean area`))) {
        # First double check this colony doesn't appear elsewhere
        with_lme <- dplyr::filter(field_plan_sheet, !is.na(`Ocean area`)) %>%
            dplyr::select(Colony, "Ocean area") %>%
            dplyr::distinct()
        field_plan_sheet$`Ocean area`[is.na(field_plan_sheet$`Ocean area`)] <- dplyr::left_join(dplyr::filter(field_plan_sheet, is.na(`Ocean area`)), with_lme, by = "Colony", multiple = "first")$`Ocean area.y`

        # Otherwise take lme from the database

        field_plan_sheet$`Ocean area`[is.na(field_plan_sheet$`Ocean area`)] <- dplyr::left_join(dplyr::filter(field_plan_sheet, is.na(`Ocean area`)), colonies, by = dplyr::join_by(Colony == colony_int_name))$`lme_name`
    }

    field_plan_sheet <- dplyr::left_join(field_plan_sheet, select(colonies, colony_int_name, lat, lon), by = dplyr::join_by(Colony == colony_int_name))


    if (any(is.na(field_plan_sheet$lat) | is.na(field_plan_sheet$lon))) {
        problem_colony <- paste(unique(field_plan_sheet$Colony[is.na(field_plan_sheet$lat)]), collapse = ", ")
        stop(glue::glue("The following colonies are missing latitutdes: {problem_colony}"))
    }

    # calculate distance of each ocean area from Trondheim (the center of the world)
    field_plan_sheet_ocean <- sf::st_as_sf(field_plan_sheet, coords = c("lat", "lon"), remove = FALSE, crs = 4326)

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
