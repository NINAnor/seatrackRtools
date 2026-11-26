#' Parse the header of a migratetech file
#'
#' Convert the header of a migratetech file into a vector of column names and extract the wet number.
#'
#' @param filepath The full path to the migratetech file to be processed.
#' @return A list containing the vector of column names and the wet number.
#'
#' @concept activity_db_prep
#' @export
parse_migratetech_header <- function(filepath) {
    # parse header
    full_header <- readLines(filepath, n = 20)
    firmware_version_string <- sub(".*Type: ([^,]+).*", "\\1", full_header[2])
    firmware_version_numeric <- lapply(strsplit(firmware_version_string, ".", fixed = TRUE)[[1]], as.numeric)
    names(firmware_version_numeric) <- c("major", "minor", "patch")
    firmware_version <- list(string = firmware_version_string, numeric = firmware_version_numeric)
    if (grepl("v", full_header[1])) {
        software_version_string <- sub(".*v([^,]+).*", "\\1", full_header[1])
        software_version_numeric <- lapply(strsplit(software_version_string, ".", fixed = TRUE)[[1]], as.numeric)
        names(software_version_numeric) <- c("major", "minor", "patch")
    } else {
        software_version_string <- "<1.18.1"
        software_version_numeric <- list(major = 1, minor = 17, patch = 0)
    }
    software_version <- list(string = software_version_string, numeric = software_version_numeric)
    data_header <- full_header[20]
    file_extension <- tools::file_ext(filepath)
    wet_number <- NA_real_

    if (length(data_header) > 0 && file_extension == "deg") {
        wet_number <- max(as.numeric(unlist(regmatches(
            data_header, gregexpr("(?<=wets)\\d+|(?<=-)\\d+", data_header, perl = TRUE)
        ))))
    }
    data_header_vector <- strsplit(data_header, "\t")[[1]]
    data_header_vector <- gsub("\\s*\\([^)]*\\)", "", data_header_vector)
    data_header_vector <- trimws(data_header_vector)
    return(list(firmware_version = firmware_version, software_version = software_version, data_header_vector = data_header_vector, wet_number = wet_number))
}
