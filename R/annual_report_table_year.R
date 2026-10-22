# This is the old per year juvenile table.
year_summary_table <- function() {
    min_juv_year <- 2019
    target_juv_years <- c(min_juv_year:(field_year - 1))

    juv_year_summary <- lapply(target_juv_years, function(target_year) {
        target_field_plan <- all_clean_field_plan[[which(names(all_clean_field_plan) == target_year)]]
        target_field_plan <- target_field_plan[target_field_plan$age == "C", ]
        current_species <- unlist(sapply(seq_along(species_introduction_year), function(i) {
            if (species_introduction_year[[i]] <= target_year) {
                return(names(species_introduction_year)[i])
            }
        }))
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
    names(juv_year_summary) <- target_juv_years

    current_species <- filter_species_intro_year(field_year)

    juv_only_current <- current_field_plan[current_field_plan$age == "C" & current_field_plan$Species %in% current_species, ]
    juv_only_sum_current <- dplyr::group_by(juv_only_current, Species) %>%
        dplyr::summarise(
            retrieved = sum(retrieved, na.rm = TRUE),
            deployed = sum(deployed, na.rm = TRUE),
            planned = sum(planned, na.rm = TRUE),
            missing_data = flag_missing_data(source)
        )
    juv_only_sum_current <- rbind(
        juv_only_sum_current,
        data.frame(
            Species = "Total",
            as.data.frame(t(colSums(juv_only_sum_current[sapply(juv_only_sum_current, is.numeric)]))),
            missing_data = any(juv_only_sum_current$missing_data[!is.na(juv_only_sum_current$Species)])
        )
    )

    juv_only_sum_current <- dplyr::mutate(juv_only_sum_current, prop_success = deployed / planned)
    juv_only_sum_current <- juv_only_sum_current[match(tolower(c(juv_species, "Total")), tolower(juv_only_sum_current$Species)), ]
    juv_only_sum_current <- dplyr::select(juv_only_sum_current, -Species)
    names(juv_only_sum_current) <- paste("current_year", names(juv_only_sum_current), sep = "_._")

    all_returns <- lapply(target_juv_years, function(target_year) {
        target_field_plan <- all_clean_field_plan[[which(names(all_clean_field_plan) == target_year)]]
        target_field_plan <- target_field_plan[target_field_plan$age == "C", ]
        current_species <- unlist(sapply(seq_along(species_introduction_year), function(i) {
            if (species_introduction_year[[i]] <= target_year) {
                return(names(species_introduction_year)[i])
            }
        }))
        target_field_plan <- target_field_plan[target_field_plan$Species %in% current_species, ]

        target_field_summary <- dplyr::group_by(target_field_plan, Species, year) %>%
            summarise(
                deployed = sum(deployed, na.rm = TRUE),
                retrieved = sum(retrieved, na.rm = TRUE),
                missing_data = flag_missing_data(source),
                missing_data = flag_missing_data(source), .groups = "drop"
            )
        target_field_summary <- rbind(target_field_summary, data.frame(
            Species = "Total", year = target_year,
            deployed = sum(target_field_summary$deployed, na.rm = TRUE),
            retrieved = sum(target_field_summary$retrieved, na.rm = TRUE),
            missing_data = any(target_field_summary$missing_data[!is.na(target_field_summary$Species)])
        ))
        return(target_field_summary)
    })
    all_returns <- do.call(rbind, all_returns)
    all_returns_sum <- dplyr::group_by(all_returns, Species) %>%
        dplyr::summarise(
            deployed = sum(deployed, na.rm = TRUE),
            retrieved = sum(retrieved, na.rm = TRUE),
            missing_data = any(missing_data)
        ) %>%
        dplyr::mutate(prop_recovered = retrieved / deployed)
    all_returns_sum <- all_returns_sum[match(tolower(c(juv_species, "Total")), tolower(all_returns_sum$Species)), ]

    # Construct dataframe
    table_2_df <- do.call(data.frame, lapply(seq_along(juv_year_summary), function(year_idx) {
        current_summary <- juv_year_summary[[year_idx]]
        current_summary <- current_summary[match(c(juv_species, "Total"), current_summary$Species), c("retrieved", "deployed")]

        names(current_summary) <- paste(names(juv_year_summary)[year_idx], names(current_summary), sep = "_._")
        return(current_summary)
    }))

    table_2_df <- data.frame(Species = c(juv_species, "Total"), table_2_df, juv_only_sum_current, current_year_._prop_recovered = all_returns_sum$prop_recovered)

    # Prepare formatting
    perc_cols <- which(grepl("prop", names(table_2_df), fixed = TRUE))
    table_2_df[, perc_cols] <- table_2_df[, perc_cols] * 100
    numeric_cols <- sapply(table_2_df, is.numeric)
    table_2_df[numeric_cols] <- lapply(table_2_df[numeric_cols], function(x) round(x, 0))
    table_2_df[perc_cols] <- lapply(table_2_df[perc_cols], function(x) ifelse(is.na(x), 0, x))
    table_2_df[, perc_cols] <- apply(table_2_df[, perc_cols], 2, function(x) {
        paste0(x, "%")
    })

    # Add asterisk to indicate missing data
    table_2_missing_data <- table_2_df$current_year_._missing_data
    table_2_df$current_year_._missing_data <- NULL
    current_cols <- which(grepl("current_year", names(table_2_df), fixed = TRUE) & !grepl("planned", names(table_2_df), fixed = TRUE))
    table_2_df[table_2_missing_data, current_cols] <- apply(table_2_df[table_2_missing_data, current_cols], 2, function(x) {
        paste0(x, "*")
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

    table_2_ft <- width(table_2_ft, width = ft_dims_cells_min$width * 0.8)
    table_2_ft <- width(table_2_ft, width = ft_dims$widths[1], j = 1)

    # Export table 2
    sheet_name <- paste0("table_2_", field_year)
    table_2_wb <- openxlsx2::wb_workbook()$add_worksheet(sheet_name)
    table_2_wb <- flexlsx::wb_add_flextable(table_2_wb, sheet_name, table_2_ft)
    table_2_wb <- openxlsx2::wb_set_col_widths(table_2_wb, cols = 2:ncol(table_2_ft$body$content$data), widths = body_cell_width)

    table_2_wb$save(file.path(export_dir, paste0(sheet_name, ".xlsx")))
}
