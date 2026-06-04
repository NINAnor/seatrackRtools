#' Annual report event tables
#' This function exports an excel file of an annual report event table. The table is generated using the get_event_table_ft function, which creates a flextable object based on the field plan and database. The export_event_table function then formats the table and saves it as an excel file in the specified directory.
#' @param event_table_i A flextable object representing the event table to be exported.
#' @param field_year The year for which the event table is being generated, used in the sheet name and file name of the exported excel file.
#' @param current_table_name The name of the current table, used in the sheet name and file name of the exported excel file.
#' @param body_cell_width The width of the body cells in the exported excel file.
#' @param header_heights The heights of the header rows in the exported excel file.
#' @param export_dir The directory where the exported excel file will be saved.
#' @concept annual_report
export_event_table <- function(event_table_i, field_year, current_table_name, body_cell_width, header_heights, export_dir) {
        sheet_name <- paste(current_table_name, field_year, sep = "_")
        table_i_wb <- openxlsx2::wb_workbook()$add_worksheet(sheet_name)
        table_i_wb <- flexlsx::wb_add_flextable(table_i_wb, sheet_name, event_table_i)

        lme_range <- openxlsx2::wb_dims(x = event_table_i$body$content$data, from_row = 1, from_col = 2, rows = 1, col_names = FALSE)
        table_i_wb$add_cell_style(dims = lme_range, textRotation = 90)
        col_name_range <- openxlsx2::wb_dims(x = event_table_i$body$content$data, from_row = 2, from_col = 2, rows = 1, col_names = FALSE)
        table_i_wb$add_cell_style(dims = col_name_range, textRotation = 90)

        table_i_wb <- openxlsx2::wb_set_col_widths(table_i_wb, cols = 2:ncol(event_table_i$body$content$data), widths = body_cell_width * 0.75)
        table_i_wb <- openxlsx2::wb_set_row_heights(table_i_wb, rows = c(1, 2), heights = header_heights)
        # Export
        table_i_wb$save(file.path(export_dir, paste0(sheet_name, ".xlsx")))
}

#' Get event table as flextable
#' This function generates a flextable object for an event table based on the field plan and database. It retrieves the relevant data for the specified event type, logger type, target age, and target species, and formats it into a flextable with appropriate styling and formatting for use in the annual report.
#' @param current_field_plan The current field plan data frame containing information about the planned and actual events.
#' @param name The name of the event table to be generated, used for labeling the table.
#' @param target_species A vector of target species to include in the table.
#' @param event_type The type of event to generate the table for, either "deployed" or "retrieved".
#' @param logger_type The type of logger to filter the data by. Either "GLS", "GPS", or "GPS-GSM".
#' @param target_age The age class to filter the data by. Either "A" or "C".
#' @return A flextable object representing the event table.
#' @concept annual_report
get_event_table_ft <- function(current_field_plan, name, target_species, event_type = c("deployed", "retrieved"), logger_type = c("GLS", "GPS", "GPS-GSM"), target_age = c("A", "C")) {
    event_type <- match.arg(event_type)
    logger_type <- match.arg(logger_type)
    target_age <- match.arg(target_age)

    event_table_df <- get_event_table_df(current_field_plan = current_field_plan, event_type = event_type, logger_type = logger_type, target_age = target_age, target_species = target_species)
    # flatten df

    event_table_df_flat <- event_table_df[, sapply(strsplit(names(event_table_df), "_-_"), function(x) {
        x[1] %in% c("Species", "n_event", "n_site")
    })]

    event_table_success <- event_table_df[, sapply(strsplit(names(event_table_df), "_-_"), function(x) {
        x[1] == "event_success"
    })]

    n_cols <- names(event_table_df)[sapply(strsplit(names(event_table_df), "_-_"), function(x) {
        x[1] == "n_event"
    })]

    event_table_ft <- flextable(event_table_df_flat, use_labels = FALSE)
    # change header labels
    header_labels <- names(event_table_df_flat)
    split_header_labels <- strsplit(header_labels, "_-_", fixed = TRUE)
    replace_table <- data.frame(
        name = c("Species", "n_event", "n_site"),
        new_name = c("Species", "N", "Sites")
    )
    new_header_labels <- sapply(split_header_labels, function(x) {
        replace_table$new_name[match(x[1], replace_table$name)]
    })
    names(new_header_labels) <- header_labels
    event_table_ft <- set_header_labels(event_table_ft, values = new_header_labels)

    df_regions <- unique(sapply(split_header_labels, function(x) {
        x[2]
    }))

    df_regions <- df_regions[!is.na(df_regions)]
    df_regions_format <- gsub(" and ", " & ", df_regions)
    df_regions_format <- sapply(df_regions_format, function(x) {
        long_string <- nchar(x) >= 20
        if (!long_string) {
            return(x)
        }

        x2 <- gsub(" - ", "\n- ", x)
        long_string <- any(nchar(strsplit(x2, "\n", fixed = TRUE)[[1]]) >= 20)

        if (!long_string) {
            return(x2)
        }

        x3 <- gsub(" & ", " &\n", x2)
        return(x3)
    })


    event_table_ft <- add_header_row(event_table_ft, values = c(name, df_regions_format), colwidths = c(1, rep(2, length(df_regions))))

    # background
    event_table_ft <- bg(event_table_ft, j = 1, bg = species_groups$colour[match(tolower(event_table_df_flat$Species), tolower(species_groups$species))])
    event_table_ft <- bg(event_table_ft, i = 1, bg = table_bg$deployed_bg, part = "header")
    event_table_ft <- bg(event_table_ft, i = 2, bg = table_bg$col_name_bg, part = "header")
    event_table_ft <- bg(event_table_ft, i = 2, j = 1, bg = table_bg$header, part = "header")
    # Colour cells by success
    bg_cols <- success_col_func(unlist(as.vector(event_table_success)))
    text_cols <- rep("black", length(bg_cols))
    text_cols[bg_cols == "#EF3F27"] = "white"

    event_table_ft <- bg(event_table_ft,
        j = n_cols,
        bg = bg_cols,
        part = "body"
    )
    event_table_ft <- bg(event_table_ft, j = c((ncol(event_table_df_flat) - 1), ncol(event_table_df_flat)), bg = "white", part = "body")
    event_table_ft <- bg(event_table_ft, i = nrow(event_table_df_flat), bg = table_bg$col_name_bg, part = "body")

    event_table_ft <- color(event_table_ft, j = n_cols, color = text_cols, part = "body")

    # fonts
    event_table_ft <- bold(event_table_ft, part = "header", i = 1)
    event_table_ft <- bold(event_table_ft, part = "header", i = 2, j = 1)
    event_table_ft <- bold(event_table_ft, part = "body", i = which(event_table_df_flat$Species == "Total"), j = 1)

    # alignment
    event_table_ft <- align(event_table_ft, align = "center", part = "header")
    event_table_ft <- align(event_table_ft, align = "center", part = "body")
    event_table_ft <- align(event_table_ft, j = 1, align = "left", part = "header")
    event_table_ft <- align(event_table_ft, j = 1, align = "left", part = "body")

    # Rotate label
    event_table_ft <- rotate(event_table_ft, i = 1, j = 2:ncol(event_table_df_flat), rotation = "btlr", part = "header", align = "bottom")
    event_table_ft <- rotate(event_table_ft, i = 1, j = 1, rotation = "lrtb", part = "header", align = "bottom")
    event_table_ft <- rotate(event_table_ft, i = 2, j = 2:ncol(event_table_df_flat), rotation = "btlr", part = "header", align = "center")

    event_table_ft <- border_remove(event_table_ft)
    event_table_ft <- border_inner(event_table_ft, border = table_borders$st_border_inner)
    event_table_ft <- border_outer(event_table_ft, border = table_borders$st_border_outer)

    target_borders <- seq(3, by = 2, length.out = length(df_regions) - 1)
    event_table_ft <- vline(event_table_ft, j = c(1, target_borders), border = table_borders$st_border_outer)
    event_table_ft <- hline(event_table_ft, i = 1, part = "header", border = table_borders$st_border_outer)
    event_table_ft <- hline(event_table_ft, i = nrow(event_table_df_flat) - 1, part = "body", border = table_borders$st_border_outer)

    ft_dims <- dim_pretty(event_table_ft, part = "body")
    ft_dims_cells_min <- lapply(ft_dims, function(x) {
        min(x[c(2:length(x))])
    })
    ft_dims_cells_max <- lapply(ft_dims, function(x) {
        max(x[c(2:length(x))])
    })

    header_dims <- dim_pretty(event_table_ft, part = "header")

    event_table_ft <- width(event_table_ft, width = ft_dims_cells_min$width)
    event_table_ft <- width(event_table_ft, width = ft_dims$widths[1], j = 1)
    return(event_table_ft)
}

#' Get event table data frame
#' This function retrieves the data for the event table based on the field plan and database, filtered by the specified event type, logger type, target age, and target species. It processes the data to calculate the number of events, number of sites, and event success rate for each species and LME, and formats it into a data frame suitable for creating the event table.
#' @param current_field_plan The current field plan data frame containing information about the planned and actual events.
#' @param event_type The type of event to generate the table for, either "deployed" or "retrieved".
#' @param logger_type The type of logger to filter the data by. Either "GLS", "GPS", or "GPS-GSM".
#' @param target_age The age class to filter the data by. Either "A" or "C".
#' @param target_species A vector of target species to include in the table.
#' @return A data frame containing the number of events, number of sites, and event success rate for each species and LME, formatted for creating the event table.
#' @keywords internal
get_event_table_df <- function(current_field_plan, target_species, event_type = c("deployed", "retrieved"), logger_type = c("GLS", "GPS", "GPS-GSM"), target_age = c("A", "C")) {
    regions <- c(
        "Barents Sea",
        "Norwegian Sea",
        "Iceland and East Greenland",
        "North Sea and Faroes",
        "Baltic Sea",
        "Hudson and Baffin Bay - West Greenland",
        "Scotian Shelf - Newfoundland",
        "Celtic-Biscay Shelf"
    )

    target_events <- current_field_plan[current_field_plan$logger_type == logger_type & current_field_plan$age == target_age, ]
    if (event_type == "deployed") {
        target_events$success_col <- target_events$planned
    } else if (event_type == "retrieved") {
        target_events$success_col <- target_events$prev_deployed
    }

    target_event_summary <- dplyr::mutate(target_events, event_col = pull(target_events, event_type)) %>%
        dplyr::group_by(LME, Species) %>%
        dplyr::summarise(n_event = sum(event_col), n_site = n_distinct(Location), success_col = sum(success_col, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(event_success = n_event / success_col)

    target_event_summary$event_success[is.infinite(target_event_summary$event_success)] <- 1

    target_event_lme <- tidyr::pivot_wider(
        target_event_summary,
        names_from = c(LME),
        values_from = c(n_event, n_site, success_col, event_success),
        values_fill = NA,
        names_sep = "_-_"
    )

    ordered_cols <- colnames(target_event_lme)[unlist(lapply(regions, function(x) {
        which(grepl(x, colnames(target_event_lme), ignore.case = TRUE))
    }))]


    target_event_lme <- target_event_lme[, c(
        "Species",
        rbind(
            grep("n_event", ordered_cols, fixed = TRUE, value = TRUE),
            grep("n_site", ordered_cols, fixed = TRUE, value = TRUE),
            grep("event_success", ordered_cols, fixed = TRUE, value = TRUE),
            grep("success_col", ordered_cols, fixed = TRUE, value = TRUE)
        )
    )]

    total_df <- data.frame(Species = "Total", data.frame(t(colSums(target_event_lme[, 2:ncol(target_event_lme)], na.rm = TRUE))))
    names(total_df) <- names(target_event_lme)

    target_event_lme <- rbind(
        target_event_lme,
        total_df
    )
    target_event_lme <- target_event_lme[match(tolower(c(target_species, "Total")), tolower(target_event_lme$Species)), ]

    target_event_lme <- dplyr::mutate(target_event_lme,
        `n_site_-_Total` = rowSums(dplyr::select(target_event_lme, dplyr::starts_with("n_site")), na.rm = TRUE),
        `n_event_-_Total` = rowSums(dplyr::select(target_event_lme, dplyr::starts_with("n_event")), na.rm = TRUE),
        `success_col_-_Total` = rowSums(dplyr::select(target_event_lme, dplyr::starts_with("success_col")), na.rm = TRUE)
    ) %>% dplyr::mutate(`event_success_-_Total` = `n_event_-_Total` / `success_col_-_Total`)

    return(target_event_lme)
}
