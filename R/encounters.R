# Append encounter data
#'
#' Append encounter data to the master import metadata
#'
#' @param master_metadata A data frame representing the master import metadata.
#' @param encounter_data A data frame representing the encounter data to be appended.
#' @param version Version of processing to use
#' @return A data frame with the encounter data appended to the master metadata.
#' @examples
#' \dontrun{
#' updated_master_metadata <- append_encounter_data(master_metadata, encounter_data)
#' }
#' @export
#' @concept encounters
append_encounter_data <- function(master_metadata, encounter_data, version = 2026) {
    if (nrow(encounter_data) == 0) {
        log_info("No encounter data to append.")
        return(master_metadata)
    }

    # Change longer column names in encounter_data to match those in master_metadata
    names(encounter_data)[grep("other relevant variables", names(encounter_data), fixed = TRUE)] <- "other"

    # If the master_metadata sheet is is lacking the nest_latitude and nest_longitude columns, add them
    if (!"nest_latitude" %in% colnames(master_metadata)) {
        master_metadata$nest_latitude <- NA
    }
    if (!"nest_longitude" %in% colnames(master_metadata)) {
        master_metadata$nest_longitude <- NA
    }

    if (version > 2025 && "scull" %in% names(master_metadata)) {
        # Rename scull
        master_metadata <- dplyr::rename(master_metadata, skull = scull)
    }

    # remove invalid rows from encounter_data
    encounter_data <- encounter_data[!is.na(encounter_data$date), ]

    # Add any columns from master_metadata that are missing from encounter_data
    missing_cols <- setdiff(colnames(master_metadata), colnames(encounter_data))

    for (col in missing_cols) {
        encounter_data[[col]] <- NA
    }

    if (any(is.na(as.Date(encounter_data$date)))) {
        stop("NA values in encounter data date column. Check excel column type.")
    }

    if (any(encounter_data$date <= as.Date("1000-01-01"))) {
        stop("Date format was loaded incorrectly. Check excel column type.")
    }


    # Throw an error if there are any columns in encounter_data that are not in master_metadata
    if (any(!colnames(encounter_data) %in% colnames(master_metadata))) {
        # just warn about columns getting discarded?
        stop(paste("The following columns are in encounter_data but not in master_metadata:", paste(colnames(encounter_data)[!colnames(encounter_data) %in% colnames(master_metadata)], collapse = ", ")))
    }

    if (any(!colnames(master_metadata) %in% colnames(encounter_data))) {
        # just warn about columns getting discarded?
        stop(paste("The following columns are in master_metadata but not in encounter_data:", paste(colnames(master_metadata)[!colnames(master_metadata) %in% colnames(encounter_data)], collapse = ", ")))
    }

    # Make sure the column order matches
    encounter_data <- encounter_data[, colnames(master_metadata)]

    # Check if dates are odd

    # Check if duplicate rows exist based on logger_id and date

    encounter_id_date <- paste(encounter_data$ring_number, encounter_data$logger_id_retrieved, encounter_data$logger_id_deployed, encounter_data$date)
    master_metadata_id_date <- paste(master_metadata$ring_number, master_metadata$logger_id_retrieved, master_metadata$logger_id_deployed, master_metadata$date)

    if (sum(encounter_id_date %in% master_metadata_id_date) > 0) {
        log_trace("Duplicate rows found based on logger_id and date. These rows will not be appended to the master metadata.")
        # Remove duplicate rows from encounter_data
        encounter_data <- encounter_data[!encounter_id_date %in% master_metadata_id_date, ]
    }

    updated_metadata <- rbind(master_metadata, encounter_data)

    log_success("Appended ", nrow(encounter_data), " rows to master metadata. New total is ", nrow(updated_metadata), " rows.")

    return(updated_metadata)
}
