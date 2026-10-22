filter_species_intro_year <- function(target_year, target_species = c(
                                          "Atlantic puffin", "Brünnich's guillemot", "Common guillemot",
                                          "Little auk", "Razorbill", "Arctic tern", "Black-legged kittiwake", "Great skua",
                                          "Leach's Storm Petrel", "Northern fulmar", "Northern gannet", "Glaucous gull", "Herring gull",
                                          "Lesser black-backed gull", "European shag", "Common eider"
                                      )) {
    species_introduction_year <- lapply(target_species, function(x) {
        phase_3_species <- c("Razorbill", "Arctic tern", "Great skua", "Leach's Storm Petrel", "Northern gannet")
        if (x %in% phase_3_species) {
            return(2023)
        } else {
            return(2014)
        }
    })
    names(species_introduction_year) <- target_species

    available_species <- unlist(sapply(seq_along(species_introduction_year), function(i) {
        if (species_introduction_year[[i]] <= target_year) {
            return(names(species_introduction_year)[i])
        }
    }))
    return(available_species)
}

#' Export all tables for annual report
#'
#' This function generates tables for the annual report based on the field plan, metadata and database.
#' @param field_plan_path Path to field plan Excel file
#' @param field_year Year to generate tables for, default is current year
#' @param body_cell_width Width of body cells in the exported Excel file, default is 9
#' @param event_table_header_heights Heights of header rows in the event tables, default is c(100, 30)
#' @param base_export_dir Directory to export tables to. Default is "Admin/01_Annual reports/Status_Report_for_funders_" in the sea track folder. The field year will be appended to the directory name
#' @param all_metadata Optional data frame of all metadata, if not provided will be loaded from master import
#' @export
#' @concept annual_report
make_annual_report_tables <- function(
    field_plan_path = file.path(the$sea_track_folder, "Admin/Fieldplanning"),
    field_year = c(as.numeric(format(Sys.Date(), "%Y"))),
    body_cell_width = 9,
    event_table_header_heights = c(100, 30),
    base_export_dir = file.path(the$sea_track_folder, "Admin/01_Annual reports/Status_Report_for_funders_"),
    export_clean_plan = TRUE,
    all_metadata = NULL) {
    seatrackR:::checkCon()

    flag_missing_data <- function(variable) {
        if (any(is.na(variable))) {
            return(TRUE)
        }
        return(FALSE)
    }

    target_species <- c(
        "Atlantic puffin", "Brünnich's guillemot", "Common guillemot",
        "Little auk", "Razorbill", "Arctic tern", "Black-legged kittiwake", "Great skua",
        "Leach's Storm Petrel", "Northern fulmar", "Northern gannet", "Glaucous gull", "Herring gull",
        "Lesser black-backed gull", "European shag", "Common eider"
    )

    juv_species <- c("Atlantic puffin", "Brünnich's guillemot", "Common guillemot", "Little auk", "Black-legged kittiwake", "Northern gannet", "Glaucous gull")
    gps_species <- c("Brünnich's guillemot", "Common guillemot", "Great skua", "Northern fulmar", "Northern gannet", "Glaucous gull", "Herring gull", "European shag", "Common eider")

    export_dir <- file.path(paste0(base_export_dir, field_year), "tables")
    if (!dir.exists(export_dir)) {
        dir.create(export_dir, recursive = TRUE)
    }

    field_plan_sheet <- load_field_plan_sheet(field_plan_path, field_year)

    if (is.null(all_metadata)) {
        log_info("Loading all metadata from master import...")
        all_metadata <- load_all_master_import(skip = c(c("Blomstrand", "Keysite Vestland", "Lowestoft", "Iceland_processed_metadata", "not_processed", "no_location_not_processed")))
    }

    log_info("Cleaning field plan for all years...")
    all_years <- c(2014:field_year)

    all_clean_field_plan <- lapply(all_years, function(target_year) {
        log_info(paste("Cleaning field plan for year", target_year, "..."))
        field_plan_clean <- get_clean_field_plan(field_plan_sheet, target_species, TRUE, all_metadata, TRUE, target_year)
        if (target_year != field_year) {
            field_plan_clean <- field_plan_clean[field_plan_clean$source != "reported", ]
        }
        field_plan_clean$year <- target_year

        return(field_plan_clean)
    })

    names(all_clean_field_plan) <- all_years
    current_field_plan <- all_clean_field_plan[[which(names(all_clean_field_plan) == field_year)]]

    device_type_totals <- dplyr::group_by(current_field_plan, logger_type, age) %>%
        summarise(
            planned = sum(planned, na.rm = TRUE),
            retrieved = sum(retrieved, na.rm = TRUE),
            deployed = sum(deployed, na.rm = TRUE),
            missing_data = flag_missing_data(source)
        )

    age_totals <- dplyr::group_by(current_field_plan, age) %>%
        summarise(
            planned = sum(planned, na.rm = TRUE),
            retrieved = sum(retrieved, na.rm = TRUE),
            deployed = sum(deployed, na.rm = TRUE),
            missing_data = flag_missing_data(source)
        )

    location_sumamry <- data.frame(n_locations = length(unique(current_field_plan$Location)))

    if (export_clean_plan) {
        write.csv(current_field_plan, file.path(paste0(base_export_dir, field_year), glue::glue("clean_field_plan_{field_year}.csv")))
        write.csv(device_type_totals, file.path(paste0(base_export_dir, field_year), glue::glue("device_type_totals_{field_year}.csv")))
        write.csv(age_totals, file.path(paste0(base_export_dir, field_year), glue::glue("age_totals_{field_year}.csv")))
        write.csv(location_sumamry, file.path(paste0(base_export_dir, field_year), glue::glue("n_locations_{field_year}.csv")))
    }

    log_info("Generating tables for annual report...")



    # Table 1
    log_info("Generating table 1...")
    phase_summary_table(all_clean_field_plan, export_dir, "table_1", body_cell_width = body_cell_width)


    #########
    # Table 2
    #########

    phase_summary_table(all_clean_field_plan, export_dir, "table_2", phases = list(phase_2 = c(2019:2022), phase_3 = c(2023:2026)), target_age = "C", target_species = juv_species, body_cell_width = body_cell_width)

    # Table 3, 4, 5, 6

    tables_3_6 <- list(
        table_3 = list(
            name = paste("Light-level geolocators\nDeployments", field_year),
            event_type = "deployed",
            logger_type = "GLS",
            target_age = "A", target_species = target_species
        ),
        table_4 = list(
            name = paste("Light-level geolocators\nRetrievals", field_year),
            event_type = "retrieved",
            logger_type = "GLS",
            target_age = "A", target_species = target_species
        ),
        table_5 = list(
            name = paste("Leg-mounted GPS logger\nDeployments", field_year),
            event_type = "deployed",
            logger_type = "GPS",
            target_age = "A",
            target_species = gps_species
        ),
        table_6 = list(
            name = paste("Leg-mounted GPS logger\nRetrievals", field_year),
            event_type = "retrieved",
            logger_type = "GPS",
            target_age = "A",
            target_species = gps_species
        )
    )

    for (i in seq_along(tables_3_6)) {
        current_table_name <- names(tables_3_6)[i]
        current_table <- tables_3_6[[i]]
        event_table_i <- do.call(get_event_table_ft, c(list(current_field_plan = current_field_plan), current_table))

        # Excel Format
        export_event_table(event_table_i = event_table_i, field_year = field_year, current_table_name = current_table_name, body_cell_width = body_cell_width * 0.75, header_heights = event_table_header_heights, export_dir = export_dir)
    }



    # Tables 7, 8, A1, A2
    appendix_tables <- list(
        list(table = paste("appendix_1_", field_year), type = "GLS", age = "A"),
        list(table = paste("table_8_", field_year), type = "GLS", age = "C"),
        list(table = paste("table_7_", field_year), type = "GPS-GSM", age = "A"),
        list(table = paste("appendix_2_", field_year), type = "GPS", age = "A")
    )
    for (i in seq_along(appendix_tables)) {
        current_table <- appendix_tables[[i]]

        table_i_ft <- create_appendix_table_ft(current_field_plan, current_table$type, current_table$age, target_species)

        export_appendix_table_excel(table_i_ft, file.path(export_dir, paste0(current_table$table, ".xlsx")))
    }
}
