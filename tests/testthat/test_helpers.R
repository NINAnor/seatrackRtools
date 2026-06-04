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
skip_if_no_test_db <- function(message = "Test database not available") {
  con <- connect_to_test_database()
  
  if (is.null(con)) {
    testthat::skip(message)
  }
  
  return(con)
}
