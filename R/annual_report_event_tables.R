event_table <- function(current_field_plan, name, event_type, logger_type, target_age, target_species) {
    regions <- c(
        "Barents Sea",
        "Norwegian Sea",
        "Iceland and East Greenland",
        "North Sea and Faroes",
        "Baltic Sea",
        "Hudson and Baffin Bay - West Greenland",
        "Scotian Shelf - Newfoundland",
        "Celtic-Biscay Shelf",
        "Total"
    )

    target_events <- field_plan_clean_lme[field_plan_clean_lme$logger_type == logger_type & field_plan_clean_lme$age == target_age, ]
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
        values_fill = NA
    )

    target_event_lme <- target_event_lme[match(target_species, target_event_lme$Species), ]
    target_event_lme <- dplyr::mutate(target_event_lme,
        n_site_total = rowSums(dplyr::select(target_event_lme, dplyr::starts_with("n_site")), na.rm = TRUE),
        n_event_total = rowSums(dplyr::select(target_event_lme, dplyr::starts_with("n_event")), na.rm = TRUE),
        success_col_total = rowSums(dplyr::select(target_event_lme, dplyr::starts_with("success_col")), na.rm = TRUE)
    ) %>% dplyr::mutate(event_success_total = n_event_total / success_col_total)


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


    return(target_event_lme)
}
