#' Export tables for annual report
#'
#' This function generates tables for the annual report based on the field plan, metadata and database.
#' @param field_plan_path Path to field plan Excel file
#' @param export_dir Directory to export tables to
#' @param all_metadata Optional data frame of all metadata, if not provided will be loaded from master import
#' @param new_locations Optional dataframe of new locations to check against field plan
#' @param field_year Year to generate tables for, default is current year
#' @export
#' @concept annual_report
make_annual_report_tables <- function(field_plan_path, export_dir, all_metadata = NULL, new_locations = NULL, field_year = c(as.numeric(format(Sys.Date(), "%Y")))) {
    target_species <- c(
        "Atlantic puffin", "Brünnich's guillemot", "Common guillemot",
        "Little auk", "Razorbill", "Arctic tern", "Black-legged kittiwake", "Great skua",
        "Leach's Storm Petrel", "Northern fulmar", "Northern gannet", "Glaucous gull", "Herring gull",
        "Lesser black-backed gull", "European shag", "Common eider"
    )
    juv_species <- c("Atlantic puffin", "Brünnich's guillemot", "Common guillemot", "Little auk", "Black-legged kittiwake", "Glaucous gull")
    gps_species <- c("Brünnich's guillemot", "Common guillemot", "Great skua", "Northern fulmar", "Northern gannet", "Glaucous gull", "Herring gull", "European shag", "Common eider")

    if (is.null(all_locations)) {
        all_metadata <- load_all_master_import(skip = c(c("Blomstrand", "Keysite Vestland", "Lowestoft", "Iceland_processed_metadata", "not_processed", "no_location_not_processed")))
    }

    field_plan_sheet <- get_field_plan(field_plan_path)

    field_plan_sheet <- field_plan_check_locations(field_plan_sheet, new_locations)

    all_years <- c(2014:field_year)

    all_clean_field_plan <- lapply(all_years, function(target_year) {
        field_plan_clean <- get_clean_field_plan(field_plan_sheet, TRUE, all_locations, TRUE, target_year)
        if (target_year != field_year) {
            current_field <- current_field[current_field$source != "reported", ]
        }
        field_plan_year <- target_year
        # Try to get ocean areas
        field_plan_clean_lme <- field_plan_check_locations(dplyr::rename(field_plan_clean, "Colony" = Location, "Ocean area" = LME)) %>%
            dplyr::rename("Location" = Colony, "LME" = "Ocean area") %>%
            dplyr::select(!c(id, colony_nat_name))

        return(field_plan_clean_lme)
    })

    names(all_clean_field_plan) <- all_years
    current_field_plan <- all_clean_field_plan[which(names(all_clean_field_plan) == field_year)]


    # Table 1
    phases <- list(phase_1 = c(2014:2018), phase_2 = c(2019:2022), phase_3 = c(2023:2025))

    phase_summaries <- lapply(phases, function(current_phase) {
        phase_summary <- lapply(current_phase, function(target_year) {
            target_field_plan <- all_clean_field_plan[which(names(all_clean_field_plan) == target_year)]
            current_field_summary <- dplyr::group_by(target_field_plan, Species, year) %>% summarise(deployed = sum(deployed, na.rm = TRUE), retrieved = sum(retrieved, na.rm = TRUE), .groups = "drop")
            return(current_field_summary)
        })
        phase_summary <- do.call(rbind, phase_summary)
        phase_summary_med <- dplyr::group_by(phase_summary, Species) %>%
            dplyr::summarise(med_deployed = median(deployed, na.rm = TRUE)) %>%
            left_join(dplyr::group_by(phase_summary, Species) %>% summarise(med_retrieved = median(retrieved, na.rm = TRUE)), by = "Species")
        phase_summary_med <- phase_summary_med[match(target_species, phase_summary_med$Species), ]
    })

    current_field_summary <- dplyr::group_by(target_field_plan, Species) %>% summarise(deployed = sum(deployed, na.rm = TRUE), retrieved = sum(retrieved, na.rm = TRUE), planned = sum(planned, na.rm = TRUE), .groups = "drop")
    current_field_summary$success <- current_field_summary$deployed / current_field_summary$planned
    current_field_summary <- current_field_summary[match(phase_3_species, current_field_summary$Species), ]

    all_returns <- lapply(all_years, function(target_year) {
        target_field_plan <- all_clean_field_plan[which(names(all_clean_field_plan) == target_year)]
        target_field_summary <- dplyr::group_by(target_field_plan, Species, year) %>% summarise(deployed = sum(deployed, na.rm = TRUE), retrieved = sum(retrieved, na.rm = TRUE), .groups = "drop")
        return(target_field_summary)
    })
    all_returns <- do.call(rbind, all_returns)
    all_returns_sum <- dplyr::group_by(all_returns, Species) %>%
        dplyr::summarise(total_deployed = sum(deployed, na.rm = TRUE), total_retrieved = sum(retrieved, na.rm = TRUE)) %>%
        dplyr::mutate(prop_recovered = total_retrieved / total_deployed)
    all_returns_sum <- all_returns_sum[match(target_species, all_returns_sum$Species), ]

    ## Assemble table 1

    ## Format table 1

    ## Export table 1

    # Table 2
    juv_only <- current_field_plan[current_field_plan$age == "C", ]
    juv_only_sum <- dplyr::group_by(juv_only, Species) %>%
        dplyr::summarise(total_retrieved = sum(retrieved, na.rm = TRUE), total_deployed = sum(deployed, na.rm = TRUE), total_planned = sum(planned, na.rm = TRUE)) %>%
        dplyr::mutate(dep_success = total_deployed / total_planned)
    juv_only_sum <- juv_only_sum[match(juv_species, juv_only_sum$Species), ]
    # Format table 2

    # Export table 2

    # Table 3, 4, 5, 6

    tables_3_6 <- list(
        list(
            name = "table_3",
            event_type = "deployed",
            logger_type = "GLS",
            target_age = "A",
            target_species = phase_3_species
        ),
        list(
            name = "table_4",
            event_type = "retrieved",
            logger_type = "GLS",
            target_age = "A",
            target_species = phase_3_species
        ),
        list(
            name = "table_5",
            event_type = "deployed",
            logger_type = "GPS",
            target_age = "A",
            target_species = gps_species
        ),
        list(
            name = "table_6",
            event_type = "retrieved",
            logger_type = "GPS",
            target_age = "A",
            target_species = gps_species
        )
    )

    for (i in seq_along(tables_3_6)) {
        current_table <- tables_3_6[[i]]
        event_table_i <- do.call(event_table, current_table)

        # Format

        # Export
    }

    # Tables 7, 8, A1, A2
    appendix_tables <- list(list(table = "appendix_1_2025", type = "GLS", age = "A"), list(table = "table_8_2025", type = "GLS", age = "C"), list(table = "table_7_2025", type = "GPS-GSM", age = "A"), list(table = "appendix_2_2025", type = "GPS", age = "A"))
    for (i in seq_along(appendix_tables)) {
        current_table <- appendix_tables[[i]]

        ft <- create_appendix_table(field_plan_clean_lme, current_table$type, current_table$age, target_species)

        export_appendix_table_excel(ft, file.path(export_dir, paste0(current_table$table, ".xlsx")))
    }
}
