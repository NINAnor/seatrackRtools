#' Load light data from file
#'
#' This function loads light data from a specified file, processes it according to the file type (Lotek or Migrate), and returns a cleaned data frame with date-time and light information. The function includes error checking for data quality and consistency.
#' @param file_info A dataframe containing information about the file to be processed, including session_id, filename, individ_id, deployment_date, retrieval_date, full_path, and extension.
#' @return A cleaned data frame with date-time, light, and standardized light information, or NULL if the file fails quality checks.
#' @export
#' @concept activity_db_import
load_light_data <- function(file_info) {
    # load a light data file
    if (file_info$extension == "lig") {
        light_data <- handle_light_lotek(file_info$full_path)
    } else if (file_info$extension == "lux") {
        light_data <- handle_light_migrate(file_info$full_path)
    }
    return(light_data)
}

#' Handle light data from Migrate logger
#'
#' This function processes light data from a Migrate logger file. It reads the file, extracts date-time and light information, performs error checking for data quality, and returns a cleaned data frame with standardized light values.
#' @param filepath The full path to the Migrate logger file to be processed.
#' @return A cleaned data frame with date-time, light, and standardized light information, or NULL if the file fails quality checks.
#' @export
#' @concept activity_db_import
handle_light_migrate <- function(filepath) {
    # FROM VEGARD'S ORIGINAL SCRIPT

    # Read header
    file_header <- readLines(filepath, n = 11)
    clipped <- FALSE
    if (length(grep("clipped", file_header[5])) == 1 || length(grep("clipped ON", file_header[2])) == 1) {
        clipped <- TRUE
    } else {
        clipped <- FALSE
    }

    file <- read.table(filepath, sep = "\t", header = FALSE, fill = TRUE, skip = 20)

    if (nrow(file) < 5) {
        log_warn(glue::glue("{basename(filepath)} has too few lines."))
        return(NULL)
    }


    if (ncol(file) == 5) file <- file %>% dplyr::select(1, 4)
    colnames(file) <- c("V1", "V2")

    if (length(grep(",", file$V2[1])) == 1) file$V2 <- as.numeric(gsub(file$V2, ",", ".", fixed = TRUE))

    ## Error in light:
    # remove empty data rows from light
    file <- file[!is.na(file$V2), ]

    # remove logger data that exceed maximum possible value of light
    if (clipped == "TRUE") {
        file <- file[file$V2 < 1200, ]
    }

    # remove logger data that has only 0 values of light
    if (max(file$V2) == 0) {
        log_warn(glue::glue("{basename(filepath)} has only 0 values of light."))
        return(NULL)
    }
    # remove logger data that has negative values of light
    if (min(file$V2) < 0) {
        log_warn(glue::glue("{basename(filepath)} has negative values of light."))
        return(NULL)
    }

    # date format
    if (substr(file$V1[1], 3, 3) == ".") {
        file$V3 <- as.Date(substr(file$V1, 1, 10), "%d.%m.%Y")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%d.%m.%Y %H:%M:%S")
    }
    if (substr(file$V1[1], 2, 2) == ".") {
        file$V3 <- as.Date(substr(file$V1, 1, 9), "%d.%m.%Y")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%d.%m.%Y %H:%M:%S")
    }
    if (substr(file$V1[1], 4, 4) == ".") {
        file$V3 <- as.Date(substr(file$V1, 1, 8), "%d.%m.%Y")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%d.%m.%Y %H:%M:%S")
    }
    if (substr(file$V1[1], 3, 3) == "/") {
        file$V3 <- as.Date(substr(file$V1, 1, 10), "%d/%m/%Y")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%d/%m/%Y %H:%M:%S")
    }
    if (substr(file$V1[1], 5, 5) == "-") {
        file$V3 <- as.Date(substr(file$V1, 1, 10), "%Y-%m-%d")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%Y-%m-%d %H:%M:%S")
    }

    ## Error in date and time:
    # remove if data holes exceed 3 months or difference in dates are negative
    diff_time <- as.numeric(difftime(file$V1[2:length(file$V1)], file$V1[1:(length(file$V1) - 1)], "GMT", units = c("mins")))
    if (min(diff_time) < 0) {
        log_warn(glue::glue("{basename(filepath)} has negative time differences between rows."))
        return(NULL)
    }

    if (max(diff_time) > 129600) {
        log_warn(glue::glue("{basename(filepath)} has time differences between rows that exceed 3 months."))
        return(NULL)
    }

    file_final <- data.frame(
        date_time = lubridate::as_datetime(file$V1),
        clipped = clipped,
        raw_light = as.numeric(file$V2),
        std_light = as.numeric(file$V2) / max(file$V2)
    )

    return(file_final)
}

#' Handle light data from Lotek logger
#'
#' This function processes light data from a Lotek logger file. It reads the file, extracts date-time and light information, performs error checking for data quality, and returns a cleaned data frame with standardized light values.
#' @param filepath The full path to the Lotek logger file to be processed.
#' @return A cleaned data frame with date-time, light, and standardized light information, or NULL if the file fails quality checks.
#' @export
#' @concept activity_db_import
handle_light_lotek <- function(filepath) {
    file <- read.table(filepath, sep = ",", header = FALSE, fill = TRUE, skip = 1)
    if (nrow(file) < 5) {
        log_warn(glue::glue("{basename(filepath)} has too few lines."))
        return(NULL)
    }

    file$V2 <- as.POSIXct(file$V2, format = "%d/%m/%y %H:%M:%S", tz = "GMT")
    file$V5 <- as.Date(substr(file$V2, 1, 10))

    ## Error in light:
    # remove NA's data from light
    file <- file[!is.na(file$V4), ]
    # remove 'suspect' lines (light_bt$V1)
    file <- file[file$V1 == "ok", ]

    # remove logger data that exceed maximum possible value of light
    if (max(file$V4) > 64) {
        log_warn(glue::glue("{basename(filepath)} has values of light that exceed the maximum possible value of 64."))
        return(NULL)
    }
    # remove logger data that has only 0 values of light
    if (max(file$V4) == 0) {
        log_warn(glue::glue("{basename(filepath)} has only 0 values of light."))
        return(NULL)
    }
    # remove logger data that has negative values of light
    if (min(file$V4) < 0) {
        log_warn(glue::glue("{basename(filepath)} has negative values of light."))
    }

    ## Error in date and time:
    # remove if data holes exceed 3 months or difference in dates are negative
    diff_time <- as.numeric(difftime(file$V2[2:length(file$V2)], file$V2[1:(length(file$V2) - 1)], "GMT", units = c("mins")))
    if (min(diff_time) < 0) {
        log_warn(glue::glue("{basename(filepath)} has negative time differences between rows."))
        return(NULL)
    }
    if (max(diff_time) > 129600) {
        log_warn(glue::glue("{basename(filepath)} has time differences between rows that exceed 3 months."))
        return(NULL)
    }

    file_final <- data.frame(
        date_time = lubridate::as_datetime(file$V2),
        clipped = "TRUE",
        raw_light = as.numeric(file$V4),
        std_light = as.numeric(file$V4)
    )

    return(file_final)
}
