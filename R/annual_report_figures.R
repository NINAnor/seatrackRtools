cumulative_count_figure <- function(type = c("all", "adult", "juv"), min_year = 2006, max_year = as.numeric(format(Sys.Date(), "%Y"))) {
    summary_table <- dplyr::tbl(con, dbplyr::in_schema("views", "per_year_summary"))
    summary_table <- dplyr::filter(summary_table, year >= min_year, year < max_year)
    if (type == "all") {
        summary_table <- dplyr::select(summary_table, year, n_deployment, n_retrieval, n_individ_tracked)
    } else if (type == "adult") {
        summary_table <- dplyr::select(summary_table, year, n_adult_deployment, n_adult_retrieval, n_adult_individ_tracked)
    } else if (type == "juv") {
        summary_table <- dplyr::select(summary_table, year, n_juv_deployment, n_juv_retrieval)
    } else {
        stop("Enter a valid type")
    }
    summary_table <- dplyr::collect(summary_table)

    names(summary_table) <- c("year", "n_deployment", "n_retrieval", "n_individ")[1:length(names(summary_table))]
    cols_to_plot <- names(summary_table)[2:length(names(summary_table))]

    cols_to_use <- c(
        "n_deployment" = "#1BAE82",
        "n_retrieval"  = "#d95f02",
        "n_individ"    = "#7570b3"
    )

    labels_to_use <- c(
        "n_deployment" = "Deployments",
        "n_retrieval"  = "Retrievals",
        "n_individ"    = "Individuals"
    )

    summary_table_long <- summary_table |>
        tidyr::pivot_longer(
            cols = dplyr::any_of(cols_to_plot),
            names_to = "category",
            values_to = "value"
        ) |>
        dplyr::mutate(
            category = ordered(
                category,
                levels = cols_to_plot
            )
        ) |>
        dplyr::arrange(category, year) |>
        dplyr::group_by(category) |>
        dplyr::mutate(
            value = ifelse(!is.na(value), value, 0)
        ) |>
        dplyr::mutate(
            cumulative_value = cumsum(value)
        ) |>
        dplyr::ungroup()

    summary_plot <- ggplot2::ggplot(
        summary_table_long,
        ggplot2::aes(
            x = factor(year),
            y = cumulative_value,
            fill = category
        )
    ) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::labs(
            x = "Year",
            y = "Cumulative number of devices/individuals",
            fill = "Category"
        ) +
        ggplot2::scale_fill_manual(
            values = cols_to_use,
            labels = labels_to_use
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            legend.position = if (type == "juv") "left" else c(0.02, 0.98),
            legend.justification = if (type == "juv") "top" else c(0, 1),
            legend.background = ggplot2::element_rect(
                fill = NA,
                colour = NA
            ),
            axis.text.x = ggplot2::element_text(
                angle = -50,
                hjust = 1,
                vjust = -1
            ),
            panel.grid = ggplot2::element_blank(),
            text = ggplot2::element_text(size = 8)
        )
    return(summary_plot)
}

export_summary_plots <- function(
    base_export_dir = file.path(the$sea_track_folder, "Admin/01_Annual reports/Status_Report_for_funders_"),
    field_year = c(as.numeric(format(Sys.Date(), "%Y"))),
    min_juvenile_year = 2019,
    min_year = 2006,
    max_year = field_year) {
    export_dir <- file.path(paste0(base_export_dir, field_year), "figures")
    if (!dir.exists(export_dir)) {
        dir.create(export_dir, recursive = TRUE)
    }

    arglist <- list(
        list(type = "all", min_year = min_year, max_year = max_year),
        list(type = "adult", min_year = min_year, max_year = max_year),
        list(type = "juv", min_year = min_juvenile_year, max_year = max_year)
    )
    sapply(arglist, function(x) {
        plot <- do.call(cumulative_count_figure, x)
        png(file.path(export_dir, paste0("cumulative_plot_", x$type, "_", field_year, ".png")), width = ifelse(x$type == "juv", 1200, 1000), height = 960, pointsize = 8, res = 300)
        print(plot)
        dev.off()
    })
}
