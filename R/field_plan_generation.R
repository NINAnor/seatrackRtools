#' Generate the next year's field plan based on the current year's field plan sheet.
#'
#' This function takes the current year's field plan sheet, processes it, and generates a new field plan sheet for the next year. It updates the relevant columns, moves delivery dates, and fills in retrievals and deployments based on the current year's data.
#'
#' @param field_year The year for which to generate the next field plan (default is the current year).
#' @return A new field plan sheet for the next year is saved in the appropriate directory. The function does not return any value.
#' @export
#' @concept field_planning
generate_next_field_plan <- function(field_year = c(as.numeric(format(Sys.Date(), "%Y"))), all_locations = NULL) {
    target_species <- c(
        "Atlantic puffin", "Brünnich's guillemot", "Common guillemot",
        "Little auk", "Razorbill", "Arctic tern", "Black-legged kittiwake", "Great skua",
        "Leach's Storm Petrel", "Northern fulmar", "Northern gannet", "Glaucous gull", "Herring gull",
        "Lesser black-backed gull", "European shag", "Common eider"
    )

    current_field_sheet_df <- load_field_plan_sheet(return_df = TRUE, field_year = field_year)
    current_field_sheet_workbook <- load_field_plan_sheet(return_df = FALSE, field_year = field_year)

    all_sheets <- openxlsx2::wb_get_sheet_names(current_field_sheet_workbook)
    for (sheet_name in all_sheets[all_sheets != "Main"]) {
        tryCatch(
            {
                current_field_sheet_workbook <- openxlsx2::wb_clean_sheet(current_field_sheet_workbook, sheet = sheet_name)
                current_field_sheet_workbook <- openxlsx2::wb_remove_worksheet(current_field_sheet_workbook, sheet = sheet_name)
            },
            error = function(e) {}
        )
    }

    comment_dims <- c()
    for (s in seq_along(current_field_sheet_workbook$comments[[1]])) {
        if (!is.null(current_field_sheet_workbook$comments[[1]][[s]])) {
            # Extract cell coordinates or names where comments exist
            comment_dims <- c(comment_dims, current_field_sheet_workbook$comments[[1]][[s]]$ref)
        }
    }

    if (length(comment_dims) > 0) {
        for (d in comment_dims) {
            current_field_sheet_workbook <- current_field_sheet_workbook$remove_comment(sheet = 1, dims = d)
        }
    }

    field_plan_clean <- get_clean_field_plan(current_field_sheet_df, target_species = target_species, use_master_sheets = TRUE, field_year = field_year, all_locations = all_locations)

    # Check wb for prev year columns and remove
    # Add new columns for (current) prev year and fill with counts
    # append new rows for missing locations

    new_field_sheet_df <- current_field_sheet_df

    colnames(new_field_sheet_df) <- tolower(colnames(new_field_sheet_df))

    new_field_sheet_df$lat <- NULL
    new_field_sheet_df$lon <- NULL

    prev_year_cols <- colnames(new_field_sheet_df)[grepl(paste0("^", field_year - 1), colnames(new_field_sheet_df))]
    curr_year_cols <- sub(field_year - 1, field_year, colnames(new_field_sheet_df)[colnames(new_field_sheet_df) %in% prev_year_cols])
    # Wipe the previous year's columns

    new_field_sheet_df[, prev_year_cols] <- new_field_sheet_df[, curr_year_cols]
    # wipe these columns clean
    new_field_sheet_df[, curr_year_cols] <- NA

    # current columns change to next year
    colnames(new_field_sheet_df)[colnames(new_field_sheet_df) %in% curr_year_cols] <- sub(field_year, field_year + 1, colnames(new_field_sheet_df)[colnames(new_field_sheet_df) %in% curr_year_cols])

    # prev year columns change to this year
    colnames(new_field_sheet_df)[colnames(new_field_sheet_df) %in% prev_year_cols] <- sub(field_year - 1, field_year, colnames(new_field_sheet_df)[colnames(new_field_sheet_df) %in% prev_year_cols])


    # Clean and increase year of planning columns (if not data tracking)
    other_curr_year_cols <- colnames(new_field_sheet_df)[grepl(paste0("^", field_year), colnames(new_field_sheet_df))]
    other_curr_year_cols <- other_curr_year_cols[!other_curr_year_cols %in% curr_year_cols]
    new_field_sheet_df[, other_curr_year_cols] <- NA

    colnames(new_field_sheet_df)[colnames(new_field_sheet_df) %in% other_curr_year_cols] <- sub(field_year, field_year + 1, colnames(new_field_sheet_df)[colnames(new_field_sheet_df) %in% other_curr_year_cols])

    # Move delivery dates
    new_field_sheet_df$`deadline (delivery)` <- format(as.Date(new_field_sheet_df$`deadline (delivery)`), paste0(field_year + 1, "-%m-%d"))

    # Fill retrievals/deployments with correct numbers

    # Check for combos in field plan that are not in sheet?
    missing_rows <- dplyr::anti_join(dplyr::filter(field_plan_clean, deployed > 0), dplyr::select(new_field_sheet_df, colony, species, age),
        by =
            dplyr::join_by(
                Location == colony,
                Species == species, age == age
            )
    )

    clean_counts <- lapply(c("GPS", "GLS", "GPS-GSM"), function(logger_type) {
        logger_counts <- dplyr::select(new_field_sheet_df, colony, species, age) %>%
            mutate(logger_type = logger_type) %>%
            dplyr::left_join(field_plan_clean,
                by =
                    dplyr::join_by(
                        colony == Location,
                        species == Species, age == age,
                        logger_type == logger_type
                    )
            ) %>%
            select(retrieved, deployed)
        names(logger_counts) <- c(paste(tolower(logger_type), c("retrievals", "deployments")))
        return(logger_counts[, !apply(logger_counts, 2, function(x) {
            all(is.na(x) | x == 0)
        })])
    })
    clean_counts <- do.call(tibble, c(dplyr::select(new_field_sheet_df, colony, species, age), clean_counts))

    # Update counts in the sheet
    new_field_sheet_df <- rows_update(new_field_sheet_df, clean_counts, by = c("colony", "species", "age"))


    # Insert the modified data into the workbook

    current_field_sheet_workbook <- openxlsx2::wb_add_data(current_field_sheet_workbook, "Main", new_field_sheet_df, na.strings = "")


    new_path <- gsub(field_year, field_year + 1, current_field_sheet_workbook$path, fixed = TRUE)
    if (!dir.exists(dirname(new_path))) {
        dir.create(dirname(new_path), recursive = TRUE)
    }

    openxlsx2::wb_save(current_field_sheet_workbook, file = new_path)
}
