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
    data_header <- readLines(filepath, n = 20)[20]
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
    return(list(data_header_vector = data_header_vector, wet_number = wet_number))
}
