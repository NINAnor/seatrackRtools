#' Load immersion data from file
#'
#' This function loads immersion data from a specified file, processes it according to the file type (Lotek or Migrate), and returns a cleaned data frame with date-time and conductivity information. The function includes error checking for data quality and consistency.
#' @param file_info A dataframe containing information about the file to be processed, including session_id, filename, individ_id, deployment_date, retrieval_date, full_path, and extension.
#' @return A cleaned data frame with date-time, conductivity, and standardized conductivity information, or NULL if the file fails quality checks.
#' @export
#' @concept activity_db_prep
load_immersion_data <- function(file_info) {
    # load a data file
    if (file_info$extension == "act") {
        light_data <- handle_immersion_lotek(file_info$full_path)
    } else if (file_info$extension == "deg") {
        light_data <- handle_immersion_migrate(file_info$full_path)
    }
    return(light_data)
}

#' Handle immersion data from Migrate logger
#'
#' This function processes immersion data from a Migrate logger file. It reads the file, extracts date-time and conductivity information, performs error checking for data quality, and returns a cleaned data frame with standardized conductivity values.
#' @param filepath The full path to the Migrate logger file to be processed.
#' @return A cleaned data frame with date-time, conductivity, and standardized conductivity information, or NULL if the file fails quality checks.
#' @export
#' @concept activity_db_prep
handle_immersion_migrate <- function(filepath) {
    file <- read.table(filepath, sep = "\t", header = FALSE, fill = TRUE, skip = 20)
    if (nrow(file) < 5) {
        log_warn(glue::glue("{basename(filepath)} has too few lines."))
        return(NULL)
    }

    if (ncol(file) == 5) {
        file <- file %>% dplyr::select(1, 4)
    }
    colnames(file) <- c("V1", "V2")

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


    #######################
    # error check
    #######################

    ## Error in light:
    # remove empty data rows from activity
    file <- file[!is.na(file$V2), ]

    # remove logger data that exceed maximum possible value of conductivity
    if (max(file$V2) > 480) {
        log_warn(glue::glue("{basename(filepath)} has immersion values that exceed the maximum possible value of 480."))
        return(NULL)
    }
    # remove logger data that has only 0 values of conductivity
    if (max(file$V2) == 0) {
        log_warn(glue::glue("{basename(filepath)} has only 0 values of immersion."))
        return(NULL)
    }
    # remove logger data that has negative values of conductivity
    if (min(file$V2, na.rm = TRUE) < 0) {
        log_warn(glue::glue("{basename(filepath)} has negative values of immersion."))
        return(NULL)
    }

    ## Error in date and time:
    # remove if data holes exceed 3 months or difference in dates are negative
    diff_time <- as.numeric(difftime(file$V1[2:length(file$V1)], file$V1[1:(length(file$V1) - 1)], "GMT", units = c("mins")))
    if (min(diff_time) < 0 || max(diff_time) > 129600) {
        log_warn(glue::glue("{basename(filepath)} has time differences between rows that are negative or exceed 3 months."))
        return(NULL)
    }

    file_final <- data.frame(
        date_time = lubridate::as_datetime(file$V1), conductivity = as.numeric(file$V2)
    )

    if (max(file_final$conductivity) == 480) {
        file_final$std_conductivity <- file_final$conductivity / 480
    } else if (max(file_final$conductivity) == 50) {
        file_final$std_conductivity <- file_final$conductivity / 50
    } else {
        file_final$std_conductivity <- file_final$conductivity / 20
    }

    return(file_final)
}

#' Handle immersion data from Lotek logger
#'
#' This function processes immersion data from a Lotek logger file. It reads the file, extracts date-time and conductivity information, performs error checking for data quality, and returns a cleaned data frame with standardized conductivity values.
#' @param filepath The full path to the Lotek logger file to be processed.
#' @return A cleaned data frame with date-time, conductivity, and standardized conductivity information, or NULL if the file fails quality checks.
#' @export
#' @concept activity_db_prep
handle_immersion_lotek <- function(filepath) {
    file <- read.table(filepath, sep = ",", header = FALSE, fill = TRUE, skip = 1)
    if (nrow(file) < 5) {
        log_warn(glue::glue("{basename(filepath)} has too few lines."))
        return(NULL)
    }
    fix_col_bool <- any(c(sapply(c("mk3005", "mk19", "mk7"), grepl, x = tolower(basename(filepath)), fixed = TRUE), ncol(file) == 5))
    if (fix_col_bool) {
        log_trace("Handle older model")

        # From Vegard script
        act_bt2 <- file[file$V5 %in% "wet", ]
        act_bt2$V4 <- as.numeric(act_bt2$V4)
        act_bt2$V2 <- as.POSIXct(file$V2, format = "%d/%m/%y %H:%M:%S", tz = "GMT")
        act_bt_test <- aggregate(V4 ~ cut(V2, breaks = "10 mins"), act_bt2, sum)
        act_bt2 <- as.data.frame(act_bt_test[, 1])
        act_bt2$V1 <- "ok"
        act_bt2$V3 <- as.numeric(as_datetime(act_bt2[, 1]))
        act_bt2$V4 <- as.numeric(act_bt_test[, 2])
        act_bt2 <- act_bt2[, c(2, 1, 3, 4)]
        names(act_bt2) <- c("V1", "V2", "V3", "V4")
        act_bt2$V2 <- as.vector(act_bt2$V2)
        act_bt2$V2 <- as.POSIXct(act_bt2$V2, tz = "GMT")

        ts <- seq.POSIXt(min(act_bt2$V2), max(act_bt2$V2), by = "10 min")
        df <- data.frame(timestamp = ts)
        act_bt2$timestamp <- act_bt2$V2
        act_bt2 <- full_join(df, act_bt2)
        act_bt2$V2 <- act_bt2$timestamp
        act_bt2$timestamp <- NULL
        act_bt2$V1 <- "ok"
        act_bt2$V3 <- 0
        act_bt2$V5 <- 0
        act_bt2$V4[is.na(act_bt2$V4)] <- 0
        act_bt2$V4 <- act_bt2$V4 / 3 / 200

        act_bt2$V6 <- floor(act_bt2$V4)
        act_bt2$V5[(act_bt2$V4 * 200) > 200] <- 200
        act_bt2$V5[(act_bt2$V4 * 200) < 201] <- act_bt2$V4[(act_bt2$V4 * 200) < 201] * 200

        t <- 1
        for (t in 1:length(act_bt2$V1)) {
            tryCatch(
                {
                    if (act_bt2$V6[t] > 0.99) {
                        act_bt2$V5[t:(t + (act_bt2$V6[t]) - 1)] <- 200
                    }
                    if (act_bt2$V6[t] > 0.99) {
                        act_bt2$V5[(t + (act_bt2$V6[t]))] <- (act_bt2$V4[t] - floor(act_bt2$V4[t])) * 200
                    }
                },
                error = function(e) {
                    log_error("ERROR :", conditionMessage(e), "\n")
                }
            )
        }

        act_bt2$V4 <- act_bt2$V5
        act_bt2$V5 <- NULL
        act_bt2$V6 <- NULL
        file <- act_bt2
    }

    file$V2 <- as.POSIXct(file$V2, format = "%d/%m/%y %H:%M:%S", tz = "GMT")
    file$V5 <- as.Date(substr(file$V2, 1, 10))

    #######################
    # error check
    #######################

    ## Error in immersion:
    # remove NA's data from light
    file <- file[!is.na(file$V4), ]
    # remove 'suspect' lines
    file <- file[file$V1 == "ok", ]

    # remove logger data that exceed maximum possible value of immersed
    if (max(file$V4) > 200) {
        log_warn(glue::glue("{basename(filepath)} has immersion values that exceed the maximum possible value of 200."))
        return(NULL)
    }
    # remove logger data that has only 0 values of immersion
    # if(max(file$V4)==0) {file<-NULL}
    # remove logger data that has negative values of immersion
    if (min(file$V4) < 0) {
        log_warn(glue::glue("{basename(filepath)} has negative values of immersion."))
        return(NULL)
    }

    ## Error in date and time:
    # remove if data holes exceed 3 months or difference in dates are negative
    diff_time <- as.numeric(difftime(file$V2[2:length(file$V2)], file$V2[1:(length(file$V2) - 1)], "GMT", units = c("mins")))
    if (min(diff_time) < 0 | max(diff_time) > 129600) {
        log_warn(glue::glue("{basename(filepath)} has time differences between rows that are negative or exceed 3 months."))
        return(NULL)
    }

    file_final <- data.frame(
        date_time = lubridate::as_datetime(file$V2),
        conductivity = as.numeric(file$V4),
        std_conductivity = as.numeric(file$V4 / 200)
    )

    return(file_final)
}
