# Test helpers and setup functions
# Source this file at the top of any test file that might need database connections

#' Connect to test database
#'
#' Attempts to establish a connection to the seatrackR test database.
#' Returns NULL and issues a warning if connection fails (typically in CI/non-dev environments).
#'
#' @param username Username for test database (default: "postgres")
#' @param password Password for test database (default: "postgres")
#' @param host Host and port (default: "127.0.0.1:5432")
#' @param dbname Database name (default: "seatrack_test")
#'
#' @return Database connection object, or NULL if connection fails
#'
#' @details
#' This is a wrapper around \code{seatrackR::connectSeatrack()} that gracefully
#' handles connection failures. It's designed for use in test environments where
#' a test database may not be available.
#'
#' If the connection fails, a warning is issued and NULL is returned. Tests that
#' use this function should check for NULL and skip() if the database is unavailable.
#'
#' @examples
#' \dontrun{
#'   con <- connect_to_test_database()
#'   if (is.null(con)) {
#'     skip("Test database not available")
#'   }
#'   # Use connection...
#'   DBI::dbDisconnect(con)
#' }
#'
#' @export
#' @keyword internal
connect_to_test_database <- function(
    username = "postgres",
    password = "postgres",
    host = "127.0.0.1:5432",
    dbname = "seatrack_test") {
  
  tryCatch(
    {
      con <- seatrackR::connectSeatrack(
        username = username,
        password = password,
        host = host,
        dbname = dbname
      )
      return(con)
    },
    error = function(e) {
      warning(
        "Failed to connect to test database '", dbname, "' at ", host,
        ". This is expected in CI/non-dev environments. Error: ", conditionMessage(e)
      )
      return(NULL)
    }
  )
}

#' Skip test if database connection unavailable
#'
#' Convenience wrapper that calls \code{connect_to_test_database()} and skips
#' the current test if connection fails.
#'
#' @param message Message to display if test is skipped
#'
#' @return NULL invisibly. If connection succeeds, returns the connection object.
#'
#' @examples
#' \dontrun{
#'   test_that("my database test", {
#'     con <- skip_if_no_test_db("Test database not available")
#'     # Use connection...
#'     DBI::dbDisconnect(con)
#'   })
#' }
#'
#' @export
#' @keyword internal
skip_if_no_test_db <- function(message = "Test database not available") {
  con <- connect_to_test_database()
  
  if (is.null(con)) {
    testthat::skip(message)
  }
  
  return(con)
}

#' Create test partner metadata
#' Generates a test partner metadata object with predefined encounter data, restart times, and return times. This function is useful for testing functions that require a partner metadata object without needing to read from an actual file.
#' @return A list representing a test partner metadata object, containing encounter data, restart times, and return times.
#' @export
#' @keyword internal
create_test_partner_metadata <- function() {
  list(
    data = list(
      `ENCOUNTER DATA` = create_test_encounter_data(),
      `RESTART TIMES` = create_test_restart_sheet(),
      `RETURN TIMES` = create_test_returns_sheet()
    ),
    path = "/path/to/partner.xlsx",
    modified = FALSE
  )
}

#' Create test encounter data
#'
#' Generates a test encounter data tibble for use in tests.
#' @return A tibble representing test encounter data.
#' @export
#' @keyword internal
create_test_encounter_data <- function() {
  encounter_data <- tibble(
    ring_number = c("R1", "R2"),
    logger_id_deployed = c("L1", "L2"),
    logger_model_deployed = c("ModelA", "ModelB"),
    logger_id_retrieved = NA,
    logger_model_retrieved = NA,
    date = as.Date("2025-01-01"),
    colony = "Colony1",
    nest_latitude = c(60.0, 61.0),
    nest_longitude = c(10.0, 11.0)
  )
}

#' Create test encounter data
#' Generates a test encounter data tibble for use in tests.
#' @return A tibble representing test encounter data.
#' @export
#' @keyword internal
create_test_returns_sheet <- function() {
  tibble(
    logger_id = c("L1", "L2"),
    logger_model = c("ModelA", "ModelB"),
    download_by = c("User1", "User2"),
    `download / stop_date` = as.Date(c("2025-01-15", "2025-01-16")),
    status = c("Returned", "Returned"),
    `stored or sent to?` = c("Stored", "Sent"),
    comment = c("", "")
  )
}

#' Create test restart sheet
#' Generates a test restart sheet as a tibble for use in tests.
#' @return A tibble representing a test restart sheet.
#' @export
#' @keyword internal
create_test_restart_sheet <- function() {
  tibble(
    logger_id = c("L1", "L2"),
    logger_model = c("ModelA", "ModelB"),
    startdate_GMT = as.Date(c("2025-01-15", "2025-01-16")),
    starttime_GMT = as.POSIXct(c("2025-01-15 10:00:00", "2025-01-16 12:00:00"), tz = "GMT"),
    logging_mode = c("Mode1", "Mode2"),
    intended_species = c("species_A", "species_B"),
    comment = c("", "")
  )
}

#' Create test startup data
#' Generates a test startup sheet as a tibble for use in tests.
#' @return A tibble representing a test startup sheet.
#' @export
#' @keyword internal
create_test_startup_data <- function(n_rows = 5) {
  tibble(
    logger_serial_no = paste0("L", seq_len(n_rows)),
    logger_model = rep(c("birdTracker5000", "birdTracker3000"), length.out = n_rows),
    producer = rep(c("Lotek", "MigrateTech"), length.out = n_rows),
    production_year = rep(2024:2023, length.out = n_rows),
    project = rep("seatrack", n_rows),
    starttime_gmt = as.POSIXct(paste0("2025-01-0", seq_len(min(n_rows, 9)), " 10:00:00"), tz = "GMT", format = "%Y-%m-%d %H:%M:%S"),
    logging_mode = rep(NA, n_rows),
    started_by = paste0("User", seq_len(n_rows)),
    started_where = paste0("Colony", seq_len(n_rows)),
    days_delayed = rep(0:1, length.out = n_rows),
    programmed_gmt_time = as.POSIXct(paste0("2025-01-0", seq_len(min(n_rows, 9)), " 10:00:00"), tz = "GMT", format = "%Y-%m-%d %H:%M:%S"),
    intended_species = paste0("Species", seq_len(n_rows)),
    intended_location = paste0("Location", seq_len(n_rows)),
    intended_deployer = paste0("Deployer", seq_len(n_rows)),
    shutdown_session = rep(NA, n_rows),
    field_status = rep(NA, n_rows),
    downloaded_by = rep(NA, n_rows),
    download_type = rep(NA, n_rows),
    download_date = as.Date(rep(NA, n_rows)),
    decomissioned = as.Date(rep(NA, n_rows)),
    shutdown_date = as.Date(rep(NA, n_rows)),
    comment = rep("", n_rows)
  )
}

#' Create test master import workbook
#' Generates a test workbook with metadata and startup sheets for testing master import functionality.
#' @param file_path Optional file path to save the generated workbook. If NULL, the workbook is not saved to disk.
#' @return A list containing the workbook object and the data frames for metadata and startup sheets.
#' @export
#' @keyword internal
create_test_master_import <- function(file_path = NULL) {
  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("METADATA")
  wb$add_worksheet("STARTUP_SHUTDOWN")

  metadata <- create_test_encounter_data()
  startup <- create_test_startup_data()

  wb$add_data("METADATA", metadata)
  wb$add_data("STARTUP_SHUTDOWN", startup)

  if (!is.null(file_path)) {
    openxlsx2::wb_save(wb, file_path)
  }

  return(list(wb = wb, metadata = metadata, startup = startup))
}