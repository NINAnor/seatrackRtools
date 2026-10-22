#' Function to generate a summary table of field plan data by phase
#'
#' This function will generate a summary table of field plan data by phase, including median deployed and retrieved devices, as well as the proportion of successful deployments.
#' The table will be formatted and exported as an Excel file.
#'
#' @param all_clean_field_plan Cleaned field plan data, a list of data frames for each year.
#' @param export_dir Directory where the summary table will be exported.
#' @param table_name Name of the summary table to be exported.
#' @param phases List of phases, each containing a vector of years.
#' @param target_species Vector of target species to include in the summary.
#' @param target_age Target age class to filter the data (default is "A" for adults).
#' @param target_logger_type Optional vector of target logger types to filter the data.
#' @param target_colony Optional vector of target colonies to filter the data.
#' @return None. The function will export the summary table as an Excel file in the specified directory.
#'
#' @export
#' @concept annual_report
phase_summary_table <- function(
    all_clean_field_plan,
    export_dir,
    table_name,
    field_year = c(as.numeric(format(Sys.Date(), "%Y"))),
    phases = list(phase_1 = c(2014:2018), phase_2 = c(2019:2022), phase_3 = c(2023:2026)),
    target_species = c(
        "Atlantic puffin", "Brünnich's guillemot", "Common guillemot",
        "Little auk", "Razorbill", "Arctic tern", "Black-legged kittiwake", "Great skua",
        "Leach's Storm Petrel", "Northern fulmar", "Northern gannet", "Glaucous gull", "Herring gull",
        "Lesser black-backed gull", "European shag", "Common eider"
    ),
    target_age = "A",
    target_logger_type = NULL,
    target_colony = NULL,
    body_cell_width = 9) {
    filtered_field_plan <- lapply(names(all_clean_field_plan), function(target_year) {
        target_field_plan <- all_clean_field_plan[[which(names(all_clean_field_plan) == target_year)]]

        target_field_plan <- target_field_plan[target_field_plan$Species %in% target_species, ]

        if (!is.null(target_age)) {
            target_field_plan <- target_field_plan[target_field_plan$age %in% target_age, ]
        }

        if (!is.null(target_logger_type)) {
            target_field_plan <- target_field_plan[target_field_plan$logger_type %in% target_logger_type, ]
        }

        if (!is.null(target_colony)) {
            target_field_plan <- target_field_plan[target_field_plan$Location %in% target_colony, ]
        }
        return(target_field_plan)
    })
    names(filtered_field_plan) <- names(all_clean_field_plan)


    phase_summaries <- lapply(phases, function(current_phase) {
        phase_summary <- lapply(current_phase, function(target_year) {
            target_field_plan <- filtered_field_plan[[which(names(filtered_field_plan) == target_year)]]
            current_species <- filter_species_intro_year(target_year)
            target_field_plan <- target_field_plan[target_field_plan$Species %in% current_species, ]

            current_field_summary <- dplyr::group_by(target_field_plan, Species, year) %>%
                summarise(
                    deployed = sum(deployed, na.rm = TRUE),
                    retrieved = sum(retrieved, na.rm = TRUE),
                    missing_data = flag_missing_data(source), .groups = "drop"
                )
            current_field_summary <- rbind(current_field_summary, data.frame(
                Species = "Total", year = target_year,
                deployed = sum(current_field_summary$deployed, na.rm = TRUE),
                retrieved = sum(current_field_summary$retrieved, na.rm = TRUE),
                missing_data = any(current_field_summary$missing_data[!is.na(current_field_summary$Species)])
            ))
            return(current_field_summary)
        })
        phase_summary <- do.call(rbind, phase_summary)
        phase_summary_med <- dplyr::group_by(phase_summary, Species) %>%
            dplyr::summarise(
                med_deployed = median(deployed, na.rm = TRUE),
                missing_data = any(missing_data)
            ) %>%
            left_join(dplyr::group_by(phase_summary, Species) %>% summarise(med_retrieved = median(retrieved, na.rm = TRUE)), by = "Species")
        phase_summary_med <- phase_summary_med[match(c(target_species, "Total"), phase_summary_med$Species), ]
    })

    current_species <- filter_species_intro_year(field_year)

    current_field_plan <- filtered_field_plan[[which(names(filtered_field_plan) == field_year)]]

    current_field_summary <- dplyr::group_by(current_field_plan, Species) %>%
        summarise(
            deployed = sum(deployed, na.rm = TRUE),
            retrieved = sum(retrieved, na.rm = TRUE),
            planned = sum(planned, na.rm = TRUE),
            missing_data = flag_missing_data(source), .groups = "drop"
        )

    current_field_summary <- rbind(current_field_summary, data.frame(
        Species = "Total",
        retrieved = sum(current_field_summary$retrieved, na.rm = TRUE),
        deployed = sum(current_field_summary$deployed, na.rm = TRUE),
        planned = sum(current_field_summary$planned, na.rm = TRUE),
        missing_data = any(current_field_summary$missing_data)
    ))

    current_field_summary$prop_success <- current_field_summary$deployed / current_field_summary$planned
    current_field_summary <- current_field_summary[match(tolower(c(target_species, "Total")), tolower(current_field_summary$Species)), ]

    all_returns <- lapply(all_years, function(target_year) {
        target_field_plan <- filtered_field_plan[[which(names(filtered_field_plan) == target_year)]]
        target_field_plan <- target_field_plan[target_field_plan$logger_type != "GPS-GSM", ]

        current_species <- filter_species_intro_year(target_year)
        target_field_plan <- target_field_plan[target_field_plan$Species %in% current_species, ]

        target_field_summary <- dplyr::group_by(target_field_plan, Species, year) %>%
            summarise(
                deployed = sum(deployed, na.rm = TRUE),
                retrieved = sum(retrieved, na.rm = TRUE), .groups = "drop",
                missing_data = flag_missing_data(source)
            )
        target_field_summary <- rbind(target_field_summary, data.frame(
            Species = "Total", year = target_year,
            retrieved = sum(target_field_summary$retrieved, na.rm = TRUE),
            deployed = sum(target_field_summary$deployed, na.rm = TRUE),
            missing_data = any(target_field_summary$missing_data[!is.na(target_field_summary$Species)])
        ))
        return(target_field_summary)
    })

    all_returns <- do.call(rbind, all_returns)
    all_returns_sum <- dplyr::group_by(all_returns, Species) %>%
        dplyr::summarise(
            total_deployed = sum(deployed, na.rm = TRUE),
            total_retrieved = sum(retrieved, na.rm = TRUE),
            missing_data = any(missing_data)
        ) %>%
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
    table_1_df[perc_cols] <- lapply(table_1_df[perc_cols], function(x) ifelse(is.na(x), 0, x))
    table_1_df[, perc_cols] <- apply(table_1_df[, perc_cols], 2, function(x) {
        paste0(x, "%")
    })


    # Add asterisk to indicate missing data
    table_1_missing_data <- table_1_df$current_year_._missing_data
    table_1_df$current_year_._missing_data <- NULL
    current_cols <- which(grepl("current_year", names(table_1_df), fixed = TRUE) & !grepl("planned", names(table_1_df), fixed = TRUE))
    table_1_df[table_1_missing_data, current_cols] <- apply(table_1_df[table_1_missing_data, current_cols], 2, function(x) {
        paste0(x, "*")
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

    phase_headers <- c(phase_1 = "Phase I:\n 2014 - 2018", phase_2 = "Phase II:\n 2019 - 2022", phase_3 = "Phase III:\n 2023 - 2025")
    selected_phase_headers <- phase_headers[names(phase_headers) %in% names(phases)]

    table_1_ft <- add_header_row(table_1_ft, values = c("", selected_phase_headers, field_year, "Total"), colwidths = c(1, rep(2, length(selected_phase_headers)), 4, 1))

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

    sheet_name <- paste(table_name, field_year, sep = "_")
    table_1_wb <- openxlsx2::wb_workbook()$add_worksheet(sheet_name)
    table_1_wb <- flexlsx::wb_add_flextable(table_1_wb, sheet_name, table_1_ft)
    table_1_wb <- openxlsx2::wb_set_col_widths(table_1_wb, cols = 2:ncol(table_1_ft$body$content$data), widths = body_cell_width)

    table_1_wb$save(file.path(export_dir, paste0(sheet_name, ".xlsx")))
}
