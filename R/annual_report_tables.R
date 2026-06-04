#' Export all tables for annual report
#'
#' This function generates tables for the annual report based on the field plan, metadata and database.
#' @param field_plan_path Path to field plan Excel file
#' @param field_year Year to generate tables for, default is current year
#' @param body_cell_width Width of body cells in the exported Excel file, default is 9
#' @param event_table_header_heights Heights of header rows in the event tables, default is c(100, 30)
#' @param export_dir Directory to export tables to. Default is "Admin/01_Annual reports/Status_Report_for_funders_" in the sea track folder. The field year will be appended to the directory name
#' @param all_metadata Optional data frame of all metadata, if not provided will be loaded from master import
#' @param new_locations Optional dataframe of new locations to check against field plan
#' @export
#' @concept annual_report
make_annual_report_tables <- function(
    field_plan_path = file.path(the$sea_track_folder, "Admin/Fieldplanning"),
    field_year = c(as.numeric(format(Sys.Date(), "%Y"))),
    body_cell_width = 9,
    event_table_header_heights = c(100, 30),
    export_dir = file.path(the$sea_track_folder, "Admin/01_Annual reports/Status_Report_for_funders_"), all_metadata = NULL, new_locations = NULL) {
    target_species <- c(
        "Atlantic puffin", "Brünnich's guillemot", "Common guillemot",
        "Little auk", "Razorbill", "Arctic tern", "Black-legged kittiwake", "Great skua",
        "Leach's Storm Petrel", "Northern fulmar", "Northern gannet", "Glaucous gull", "Herring gull",
        "Lesser black-backed gull", "European shag", "Common eider"
    )
    juv_species <- c("Atlantic puffin", "Brünnich's guillemot", "Common guillemot", "Little auk", "Black-legged kittiwake", "Glaucous gull")
    gps_species <- c("Brünnich's guillemot", "Common guillemot", "Great skua", "Northern fulmar", "Northern gannet", "Glaucous gull", "Herring gull", "European shag", "Common eider")

    export_dir <- file.path(paste0(export_dir, field_year), "tables")
    if (!dir.exists(export_dir)) {
        dir.create(export_dir, recursive = TRUE)
    }

    full_field_plan_path <- list.files(field_plan_path, recursive = TRUE, full.names = TRUE, pattern = paste0(".*", field_year, "_fieldsuksess.xlsx$"))[1]
    if (is.null(full_field_plan_path)) {
        stop(paste("Cannot find field plan for year", field_year))
    }

    if (is.null(all_metadata)) {
        log_info("Loading all metadata from master import...")
        all_metadata <- load_all_master_import(skip = c(c("Blomstrand", "Keysite Vestland", "Lowestoft", "Iceland_processed_metadata", "not_processed", "no_location_not_processed")))
    }

    field_plan_sheet <- get_field_plan(full_field_plan_path)

    log_info("Checking field plan locations...")
    field_plan_sheet <- field_plan_check_locations(field_plan_sheet, new_locations)

    all_years <- c(2014:field_year)

    log_info("Cleaning field plan for all years...")
    all_clean_field_plan <- lapply(all_years, function(target_year) {
        log_info(paste("Cleaning field plan for year", target_year, "..."))
        field_plan_clean <- get_clean_field_plan(field_plan_sheet, TRUE, all_metadata, TRUE, target_year)
        if (target_year != field_year) {
            field_plan_clean <- field_plan_clean[field_plan_clean$source != "reported", ]
        }
        field_plan_clean$year <- target_year
        # Try to get ocean areas
        field_plan_clean_lme <- field_plan_check_locations(dplyr::rename(field_plan_clean, "Colony" = Location, "Ocean area" = LME)) %>%
            dplyr::rename("Location" = Colony, "LME" = "Ocean area") %>%
            dplyr::select(!c(id, colony_nat_name))

        return(field_plan_clean_lme)
    })

    names(all_clean_field_plan) <- all_years
    current_field_plan <- all_clean_field_plan[[which(names(all_clean_field_plan) == field_year)]]

    log_info("Generating tables for annual report...")

    # Table 1
    log_info("Generating table 1...")
    ## Assemble table 1
    phases <- list(phase_1 = c(2014:2018), phase_2 = c(2019:2022), phase_3 = c(2023:2025))

    phase_summaries <- lapply(phases, function(current_phase) {
        phase_summary <- lapply(current_phase, function(target_year) {
            target_field_plan <- all_clean_field_plan[[which(names(all_clean_field_plan) == target_year)]]
            current_field_summary <- dplyr::group_by(target_field_plan, Species, year) %>% summarise(deployed = sum(deployed, na.rm = TRUE), retrieved = sum(retrieved, na.rm = TRUE), .groups = "drop")
            current_field_summary <- rbind(current_field_summary, data.frame(
                Species = "Total", year = target_year,
                deployed = sum(current_field_summary$deployed, na.rm = TRUE),
                retrieved = sum(current_field_summary$retrieved, na.rm = TRUE)
            ))
            return(current_field_summary)
        })
        phase_summary <- do.call(rbind, phase_summary)
        phase_summary_med <- dplyr::group_by(phase_summary, Species) %>%
            dplyr::summarise(med_deployed = median(deployed, na.rm = TRUE)) %>%
            left_join(dplyr::group_by(phase_summary, Species) %>% summarise(med_retrieved = median(retrieved, na.rm = TRUE)), by = "Species")
        phase_summary_med <- phase_summary_med[match(tolower(c(target_species, "Total")), tolower(phase_summary_med$Species)), ]
    })

    current_field_summary <- dplyr::group_by(current_field_plan, Species) %>% summarise(deployed = sum(deployed, na.rm = TRUE), retrieved = sum(retrieved, na.rm = TRUE), planned = sum(planned, na.rm = TRUE), .groups = "drop")
    current_field_summary <- rbind(current_field_summary, data.frame(
        Species = "Total",
        retrieved = sum(current_field_summary$retrieved, na.rm = TRUE),
        deployed = sum(current_field_summary$deployed, na.rm = TRUE),
        planned = sum(current_field_summary$planned, na.rm = TRUE)
    ))

    current_field_summary$prop_success <- current_field_summary$deployed / current_field_summary$planned
    current_field_summary <- current_field_summary[match(tolower(c(target_species, "Total")), tolower(current_field_summary$Species)), ]

    all_returns <- lapply(all_years, function(target_year) {
        target_field_plan <- all_clean_field_plan[[which(names(all_clean_field_plan) == target_year)]]
        target_field_summary <- dplyr::group_by(target_field_plan, Species, year) %>% summarise(deployed = sum(deployed, na.rm = TRUE), retrieved = sum(retrieved, na.rm = TRUE), .groups = "drop")
        target_field_summary <- rbind(target_field_summary, data.frame(
            Species = "Total", year = target_year,
            retrieved = sum(target_field_summary$retrieved, na.rm = TRUE),
            deployed = sum(target_field_summary$deployed, na.rm = TRUE)
        ))
        return(target_field_summary)
    })
    all_returns <- do.call(rbind, all_returns)
    all_returns_sum <- dplyr::group_by(all_returns, Species) %>%
        dplyr::summarise(total_deployed = sum(deployed, na.rm = TRUE), total_retrieved = sum(retrieved, na.rm = TRUE)) %>%
        dplyr::mutate(prop_recovered = total_retrieved / total_deployed)
    all_returns_sum <- all_returns_sum[match(tolower(c(target_species, "Total")), tolower(all_returns_sum$Species)), ]

    # Construct dataframe
    table_1_df <- do.call(data.frame, lapply(seq_along(phase_summaries), function(phase_idx) {
        current_summary <- phase_summaries[[phase_idx]][, c("med_retrieved", "med_deployed")]

        names(current_summary) <- paste(names(phase_summaries)[phase_idx], names(current_summary), sep = "_._")
        return(current_summary)
    }))

    current_field_summary_df <- dplyr::select(current_field_summary, -Species)
    names(current_field_summary_df) <- paste("current_year", names(current_field_summary_df), sep = "_._")

    table_1_df <- data.frame(Species = c(target_species, "Total"), table_1_df, current_field_summary_df, current_year_._prop_recovered = all_returns_sum$prop_recovered)

    # Prepare formatting
    perc_cols <- which(grepl("prop", names(table_1_df), fixed = TRUE))
    table_1_df[, perc_cols] <- table_1_df[, perc_cols] * 100
    numeric_cols <- sapply(table_1_df, is.numeric)
    table_1_df[numeric_cols] <- lapply(table_1_df[numeric_cols], function(x) round(x, 0))
    table_1_df[, perc_cols] <- apply(table_1_df[, perc_cols], 2, function(x) {
        paste0(x, "%")
    })


    ## Format table 1
    # convert to flextable
    table_1_ft <- flextable(table_1_df, use_labels = FALSE)

    # change header labels
    header_labels <- names(table_1_df)
    split_header_labels <- strsplit(header_labels, "_._", fixed = TRUE)
    replace_table <- data.frame(
        name = c(NA, "med_retrieved", "med_deployed", "deployed", "retrieved", "planned", "prop_success", "prop_recovered"),
        new_name = c("Species", "Median\nRet", "Median\nDep", "Dep", "Ret", "Expected\nDep", "Dep\nSuccess", "% Ret")
    )
    new_header_labels <- sapply(split_header_labels, function(x) {
        replace_table$new_name[match(x[2], replace_table$name)]
    })
    names(new_header_labels) <- names(table_1_df)
    table_1_ft <- set_header_labels(table_1_ft, values = new_header_labels)

    # add extra phase header
    table_1_ft <- add_header_row(table_1_ft, values = c("", "Phase I:\n 2014 - 2018", "Phase II:\n 2019 - 2022", "Phase III:\n 2023 - 2025", field_year, "Total"), colwidths = c(1, 2, 2, 2, 4, 1))

    # backgrounds
    table_1_ft <- bg(table_1_ft, j = 1, bg = species_groups$colour[match(table_1_df$Species, species_groups$species)])
    table_1_ft <- bg(table_1_ft, i = 1, bg = table_bg$deployed_bg, part = "header")
    table_1_ft <- bg(table_1_ft, i = 2, bg = table_bg$col_name_bg, part = "header")
    table_1_ft <- bg(table_1_ft, i = 2, j = 1, bg = table_bg$header, part = "header")
    table_1_ft <- bg(table_1_ft, i = nrow(table_1_df), bg = table_bg$col_name_bg, part = "body")

    # font
    table_1_ft <- bold(table_1_ft, part = "header", i = 1)
    table_1_ft <- bold(table_1_ft, part = "header", i = 2, j = 1)
    table_1_ft <- bold(table_1_ft, part = "body", i = which(table_1_df$Species == "Total"), j = 1)

    # alignment
    table_1_ft <- align(table_1_ft, align = "center", part = "header")
    table_1_ft <- align(table_1_ft, align = "center", part = "body")
    table_1_ft <- align(table_1_ft, j = 1, align = "left", part = "header")
    table_1_ft <- align(table_1_ft, j = 1, align = "left", part = "body")

    # borders

    table_1_ft <- border_remove(table_1_ft)
    table_1_ft <- border_inner(table_1_ft, border = table_borders$st_border_inner)
    table_1_ft <- border_outer(table_1_ft, border = table_borders$st_border_outer)

    target_borders <- seq(3, by = 2, length.out = length(phases))
    current_border <- max(target_borders) + 4
    table_1_ft <- vline(table_1_ft, j = c(1, target_borders, current_border), border = table_borders$st_border_outer)
    table_1_ft <- hline(table_1_ft, i = 1, part = "header", border = table_borders$st_border_outer)
    table_1_ft <- hline(table_1_ft, i = nrow(table_1_df) - 1, part = "body", border = table_borders$st_border_outer)


    ft_dims <- dim_pretty(table_1_ft, part = "body")
    ft_dims_cells_min <- lapply(ft_dims, function(x) {
        min(x[c(3:length(x))])
    })
    ft_dims_cells_max <- lapply(ft_dims, function(x) {
        max(x[c(3:length(x))])
    })

    table_1_ft <- width(table_1_ft, width = ft_dims_cells_min$width)
    table_1_ft <- width(table_1_ft, width = ft_dims$widths[1], j = 1)

    ## Export table 1

    sheet_name <- paste0("table_1_", field_year)
    table_1_wb <- openxlsx2::wb_workbook()$add_worksheet(sheet_name)
    table_1_wb <- flexlsx::wb_add_flextable(table_1_wb, sheet_name, table_1_ft)
    table_1_wb <- openxlsx2::wb_set_col_widths(table_1_wb, cols = 2:ncol(table_1_ft$body$content$data), widths = body_cell_width)

    table_1_wb$save(file.path(export_dir, paste0(sheet_name, ".xlsx")))

    # Table 2
    min_juv_year <- 2019
    target_juv_years <- c(min_juv_year:(field_year - 1))

    juv_year_summary <- lapply(target_juv_years, function(target_year) {
        target_field_plan <- all_clean_field_plan[[which(names(all_clean_field_plan) == target_year)]]
        target_field_plan <- target_field_plan[target_field_plan$age == "C", ]
        current_field_summary <- dplyr::group_by(target_field_plan, Species, year) %>% summarise(deployed = sum(deployed, na.rm = TRUE), retrieved = sum(retrieved, na.rm = TRUE), .groups = "drop")
        current_field_summary <- rbind(current_field_summary, data.frame(
            Species = "Total", year = target_year,
            deployed = sum(current_field_summary$deployed, na.rm = TRUE),
            retrieved = sum(current_field_summary$retrieved, na.rm = TRUE)
        ))
        return(current_field_summary)
    })
    names(juv_year_summary) <- target_juv_years

    juv_only_current <- current_field_plan[current_field_plan$age == "C", ]
    juv_only_sum_current <- dplyr::group_by(juv_only_current, Species) %>%
        dplyr::summarise(retrieved = sum(retrieved, na.rm = TRUE), deployed = sum(deployed, na.rm = TRUE), planned = sum(planned, na.rm = TRUE))
    juv_only_sum_current <- rbind(
        juv_only_sum_current,
        data.frame(
            Species = "Total",
            as.data.frame(t(colSums(juv_only_sum_current[sapply(juv_only_sum_current, is.numeric)])))
        )
    )

    juv_only_sum_current <- dplyr::mutate(juv_only_sum_current, prop_success = deployed / planned)
    juv_only_sum_current <- juv_only_sum_current[match(tolower(c(juv_species, "Total")), tolower(juv_only_sum_current$Species)), ]
    juv_only_sum_current <- dplyr::select(juv_only_sum_current, -Species)
    names(juv_only_sum_current) <- paste("current_year", names(juv_only_sum_current), sep = "_._")

    all_returns <- lapply(target_juv_years, function(target_year) {
        target_field_plan <- all_clean_field_plan[[which(names(all_clean_field_plan) == target_year)]]
        target_field_plan <- target_field_plan[target_field_plan$age == "C", ]
        target_field_summary <- dplyr::group_by(target_field_plan, Species, year) %>% summarise(deployed = sum(deployed, na.rm = TRUE), retrieved = sum(retrieved, na.rm = TRUE), .groups = "drop")
        target_field_summary <- rbind(target_field_summary, data.frame(
            Species = "Total", year = target_year,
            deployed = sum(target_field_summary$deployed, na.rm = TRUE),
            retrieved = sum(target_field_summary$retrieved, na.rm = TRUE)
        ))
        return(target_field_summary)
    })
    all_returns <- do.call(rbind, all_returns)
    all_returns_sum <- dplyr::group_by(all_returns, Species) %>%
        dplyr::summarise(deployed = sum(deployed, na.rm = TRUE), retrieved = sum(retrieved, na.rm = TRUE)) %>%
        dplyr::mutate(prop_recovered = retrieved / deployed)
    all_returns_sum <- all_returns_sum[match(tolower(c(juv_species, "Total")), tolower(all_returns_sum$Species)), ]

    # Construct dataframe
    table_2_df <- do.call(data.frame, lapply(seq_along(juv_year_summary), function(year_idx) {
        current_summary <- juv_year_summary[[year_idx]][, c("retrieved", "deployed")]

        names(current_summary) <- paste(names(juv_year_summary)[year_idx], names(current_summary), sep = "_._")
        return(current_summary)
    }))


    table_2_df <- data.frame(Species = c(juv_species, "Total"), table_2_df, juv_only_sum_current, current_year_._prop_recovered = all_returns_sum$prop_recovered)

    # Prepare formatting
    perc_cols <- which(grepl("prop", names(table_2_df), fixed = TRUE))
    table_2_df[, perc_cols] <- table_2_df[, perc_cols] * 100
    numeric_cols <- sapply(table_2_df, is.numeric)
    table_2_df[numeric_cols] <- lapply(table_2_df[numeric_cols], function(x) round(x, 0))
    table_2_df[, perc_cols] <- apply(table_2_df[, perc_cols], 2, function(x) {
        paste0(x, "%")
    })

    # Format table 2
    # convert to flextable
    table_2_ft <- flextable(table_2_df, use_labels = FALSE)

    # change header labels
    header_labels <- names(table_2_df)
    split_header_labels <- strsplit(header_labels, "_._", fixed = TRUE)
    replace_table <- data.frame(
        name = c(NA, "med_retrieved", "med_deployed", "deployed", "retrieved", "planned", "prop_success", "prop_recovered"),
        new_name = c("Species", "Median\nRet", "Median\nDep", "Dep", "Ret", "Expected\nDep", "Dep\nSuccess", "% Ret")
    )
    new_header_labels <- sapply(split_header_labels, function(x) {
        replace_table$new_name[match(x[2], replace_table$name)]
    })
    names(new_header_labels) <- names(table_2_df)
    table_2_ft <- set_header_labels(table_2_ft, values = new_header_labels)

    # add extra year header
    table_2_ft <- add_header_row(table_2_ft, values = c("", target_juv_years, field_year, "Total"), colwidths = c(1, rep(2, length(target_juv_years)), 4, 1))

    # backgrounds
    table_2_ft <- bg(table_2_ft, j = 1, bg = species_groups$colour[match(table_2_df$Species, species_groups$species)])
    table_2_ft <- bg(table_2_ft, i = 1, bg = table_bg$deployed_bg, part = "header")
    table_2_ft <- bg(table_2_ft, i = 2, bg = table_bg$col_name_bg, part = "header")
    table_2_ft <- bg(table_2_ft, i = 2, j = 1, bg = table_bg$header, part = "header")
    table_2_ft <- bg(table_2_ft, i = nrow(table_2_df), bg = table_bg$col_name_bg, part = "body")

    # font
    table_2_ft <- bold(table_2_ft, part = "header", i = 1)
    table_2_ft <- bold(table_2_ft, part = "header", i = 2, j = 1)
    table_2_ft <- bold(table_2_ft, part = "body", i = which(table_2_df$Species == "Total"), j = 1)

    # alignment
    table_2_ft <- align(table_2_ft, align = "center", part = "header")
    table_2_ft <- align(table_2_ft, align = "center", part = "body")
    table_2_ft <- align(table_2_ft, j = 1, align = "left", part = "header")
    table_2_ft <- align(table_2_ft, j = 1, align = "left", part = "body")

    # border
    table_2_ft <- border_remove(table_2_ft)
    table_2_ft <- border_inner(table_2_ft, border = table_borders$st_border_inner)
    table_2_ft <- border_outer(table_2_ft, border = table_borders$st_border_outer)

    target_borders <- seq(3, by = 2, length.out = length(target_juv_years))
    current_border <- max(target_borders) + 4
    table_2_ft <- vline(table_2_ft, j = c(1, target_borders, current_border), border = table_borders$st_border_outer)
    table_2_ft <- hline(table_2_ft, i = 1, part = "header", border = table_borders$st_border_outer)
    table_2_ft <- hline(table_2_ft, i = nrow(table_2_df) - 1, part = "body", border = table_borders$st_border_outer)

    ft_dims <- dim_pretty(table_2_ft, part = "body")
    ft_dims_cells_min <- lapply(ft_dims, function(x) {
        min(x[c(3:length(x))])
    })
    ft_dims_cells_max <- lapply(ft_dims, function(x) {
        max(x[c(3:length(x))])
    })

    table_2_ft <- width(table_2_ft, width = ft_dims_cells_min$width)
    table_2_ft <- width(table_2_ft, width = ft_dims$widths[1], j = 1)

    # Export table 2
    sheet_name <- paste0("table_2_", field_year)
    table_2_wb <- openxlsx2::wb_workbook()$add_worksheet(sheet_name)
    table_2_wb <- flexlsx::wb_add_flextable(table_2_wb, sheet_name, table_2_ft)
    table_2_wb <- openxlsx2::wb_set_col_widths(table_2_wb, cols = 2:ncol(table_2_ft$body$content$data), widths = body_cell_width)

    table_2_wb$save(file.path(export_dir, paste0(sheet_name, ".xlsx")))

    # Table 3, 4, 5, 6

    tables_3_6 <- list(
        table_3 = list(
            name = paste("Light-level geolocators\nDeployments", field_year),
            event_type = "deployed",
            logger_type = "GLS",
            target_age = "A",
            target_species <- target_species
        ),
        table_4 = list(
            name = paste("Light-level geolocators\nRetrievals", field_year),
            event_type = "retrieved",
            logger_type = "GLS",
            target_age = "A",
            target_species <- target_species
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
        list(table = "appendix_1_2025", type = "GLS", age = "A"),
        list(table = "table_8_2025", type = "GLS", age = "C"),
        list(table = "table_7_2025", type = "GPS-GSM", age = "A"),
        list(table = "appendix_2_2025", type = "GPS", age = "A")
    )
    for (i in seq_along(appendix_tables)) {
        current_table <- appendix_tables[[i]]

        table_i_ft <- create_appendix_table_ft(current_field_plan, current_table$type, current_table$age, target_species)

        export_appendix_table_excel(table_i_ft, file.path(export_dir, paste0(current_table$table, ".xlsx")))
    }
}
