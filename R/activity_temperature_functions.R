load_temperature_data <- function(file_info) {
    # load a light data file
    if (file_info$extension == "tem") {
        light_data <- handle_temperature_lotek(file_info$full_path)
    } else if (file_info$extension == "sst") {
        light_data <- handle_temperature_migrate(file_info$full_path)
    }
    return(light_data)
}

handle_temperature_migrate <- function(filepath) {
    file <- read.table(filepath, sep = "\t", header = FALSE, fill = TRUE, skip = 20)
    if (nrow(file) < 5) {
        log_warn(glue::glue("{basename(filepath)} has too few lines."))
        return(NULL)
    }

    # date format
    if (substr(file$V1[1], 3, 3) == ".") {
        file$V6 <- as.Date(substr(file$V1, 1, 10), "%d.%m.%Y")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%d.%m.%Y %H:%M:%S")
    }
    if (substr(file$V1[1], 2, 2) == ".") {
        file$V6 <- as.Date(substr(file$V1, 1, 9), "%d.%m.%Y")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%d.%m.%Y %H:%M:%S")
    }
    if (substr(file$V1[1], 4, 4) == ".") {
        file$V6 <- as.Date(substr(file$V1, 1, 8), "%d.%m.%Y")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%d.%m.%Y %H:%M:%S")
    }
    if (substr(file$V1[1], 3, 3) == "/") {
        file$V6 <- as.Date(substr(file$V1, 1, 10), "%d/%m/%Y")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%d/%m/%Y %H:%M:%S")
    }
    if (substr(file$V1[1], 5, 5) == "-") {
        file$V6 <- as.Date(substr(file$V1, 1, 10), "%Y-%m-%d")
        file$V1 <- as.POSIXlt(file$V1, tz = "GMT", "%Y-%m-%d %H:%M:%S")
    }

    #######################
    # error check
    #######################

    # if ',' instead of '.' in column V2, correcting it!
    if (length(grep(",", file$V2[1])) == 1) file$V2 <- as.numeric(str_replace(file$V2, ",", "."))
    # format(as.numeric(temp_mt$V2), decimal.mark=".")
    if (length(grep(",", file$V3[1])) == 1) file$V2 <- as.numeric(str_replace(file$V3, ",", "."))
    if (length(grep(",", file$V4[1])) == 1) file$V2 <- as.numeric(str_replace(file$V4, ",", "."))
    if (length(grep(",", file$V5[1])) == 1) file$V2 <- as.numeric(str_replace(file$V5, ",", "."))

    ## Error in temperature:
    # remove empty data rows from temperature
    file <- file[!is.na(file$V4), ]

    # remove logger data that has only 0 values of temperature
    if (max(file$V4) == 0) {
        log_warn(glue::glue("{basename(filepath)} has only 0 values of temperature."))
        return(NULL)
    }

    diff_time <- as.numeric(difftime(file$V1[2:length(file$V1)], file$V1[1:(length(file$V1) - 1)], "GMT", units = c("mins")))
    if (min(diff_time) < 0 || max(diff_time) > 129600) {
        log_warn(glue::glue("{basename(filepath)} has time differences between rows that are negative or exceed 3 months."))
        return(NULL)
    }

    file_final <- data.frame(
        date_time = lubridate::as_datetime(file$V1),
        wet_temp_min = as.numeric(file$V2),
        wet_temp_max = as.numeric(file$V3),
        wet_temp_mean = as.numeric(file$V4),
        num_samples = as.numeric(file$V5)
    )
    return(file_final)
}

handle_temperature_lotek <- function(filepath) {
    file <- read.table(filepath, sep = ",", header = FALSE, fill = TRUE, skip = 1)
    if (nrow(file) < 5) {
        log_warn(glue::glue("{basename(filepath)} has too few lines."))
        return(NULL)
    }
    file$V2 <- as.POSIXct(file$V2, format = "%d/%m/%y %H:%M:%S", tz = "GMT")
    file$V6 <- as.Date(substr(file$V2, 1, 10))

    #######################
    # error check
    #######################

    ## Error in temperature:
    # remove NA's data from temperature
    file <- file[!is.na(file$V4), ]
    # remove 'suspect' lines
    file <- file[file$V1 == "ok", ]

    # remove logger data that exceed maximum possible value of temperature
    file <- file[file$V4 < 55, ]
    # remove logger data that has only 0 values of temperature
    if (max(file$V4) == 0) {
        log_warn(glue::glue("{basename(filepath)} has only 0 values of temperature."))
        return(NULL)
    }
    # remove logger data that is under a minimum value of temp (-40 deg)
    file <- file[file$V4 > -40, ]

    ## Error in date and time:
    # remove if data holes exceed 3 months or difference in dates are negative
    diff_time <- as.numeric(difftime(file$V2[2:length(file$V2)], file$V2[1:(length(file$V2) - 1)], "GMT", units = c("mins")))
    if (min(diff_time) < 0 | max(diff_time) > 129600) {
        log_warn(glue::glue("{basename(filepath)} has time differences between rows that are negative or exceed 3 months."))
        return(NULL)
    }

    file_final <- data.frame(
        date_time = lubridate::as_datetime(file$V2),
        wet_temp_min = NA,
        wet_temp_max = NA,
        wet_temp_mean = as.numeric(file$V4),
        num_samples = NA
    )
    return(file_final)
}
