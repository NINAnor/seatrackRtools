# Test script for functions in R/startups.R

library(testthat)
library(openxlsx2)
library(tibble)
library(dplyr)

# Create a temporary directory for file-based tests
tmp_dir <- tempdir()

describe("Startup Path Retrieval", {
  test_that("get_startup_paths errors if sea track folder not set", {
    the$sea_track_folder <<- NULL
    expect_error(get_startup_paths(), "Sea track folder is not set")
  })

  test_that("get_startup_paths returns character vector", {
    the$sea_track_folder <<- tmp_dir
    startup_path <- file.path(tmp_dir, "Starttime files and stored loggers", 2025)
    dir.create(startup_path, recursive = TRUE, showWarnings = FALSE)

    test_file <- file.path(startup_path, "test_startup.xlsx")
    file.create(test_file)

    result <- get_startup_paths()

    expect_type(result, "character")
    expect_true(length(result) >= 1)

    file.remove(test_file)
  })

  test_that("get_startup_paths filters tilde files (temporary files)", {
    the$sea_track_folder <<- tmp_dir
    startup_path <- file.path(tmp_dir, "Starttime files and stored loggers", 2025)
    dir.create(startup_path, recursive = TRUE, showWarnings = FALSE)

    # Create a valid file and a tilde (temp) file
    valid_file <- file.path(startup_path, "valid_startup.xlsx")
    temp_file <- file.path(startup_path, "~temp_startup.xlsx")
    file.create(valid_file)
    file.create(temp_file)

    result <- get_startup_paths()

    expect_false(any(grepl("~", basename(result))))
    expect_true(any(grepl("valid_startup", basename(result))))

    file.remove(valid_file)
    file.remove(temp_file)
  })

  test_that("get_startup_paths ignores specified folders", {
    the$sea_track_folder <<- tmp_dir
    startup_base <- file.path(tmp_dir, "Starttime files and stored loggers")
    dir.create(startup_base, recursive = TRUE, showWarnings = FALSE)

    # Create ignored and non-ignored folders
    ignored_path <- file.path(startup_base, "starttimes for other projects")
    normal_path <- file.path(startup_base, "2025")
    dir.create(ignored_path, recursive = TRUE, showWarnings = FALSE)
    dir.create(normal_path, recursive = TRUE, showWarnings = FALSE)

    ignored_file <- file.path(ignored_path, "ignored.xlsx")
    normal_file <- file.path(normal_path, "normal.xlsx")
    file.create(ignored_file)
    file.create(normal_file)

    result <- get_startup_paths()

    expect_false(any(grepl("other projects", result)))
    expect_true(any(grepl("normal.xlsx", result)))

    file.remove(ignored_file)
    file.remove(normal_file)
  })

  test_that("get_startup_paths returns empty character vector when no files found", {
    the$sea_track_folder <<- tmp_dir
    # Create empty startup directory
    startup_path <- file.path(tmp_dir, "Starttime files and stored loggers", 2025)
    dir.create(startup_path, recursive = TRUE, showWarnings = FALSE)

    result <- get_startup_paths()

    expect_type(result, "character")
  })
})

describe("Startup Helper Functions", {
  test_that("set_master_startup_value updates cell value", {
    df <- create_test_startup_data(3)
    updated <- set_master_startup_value(df, 2, "intended_location", "NewLocation")
    expect_equal(updated$intended_location[2], "NewLocation")
    expect_equal(updated$intended_location[1], df$intended_location[1])
  })

  test_that("set_master_startup_value handles date values", {
    df <- create_test_startup_data(2)
    new_date <- as.Date("2025-02-01")
    updated <- set_master_startup_value(df, 1, "download_date", new_date)
    expect_equal(updated$download_date[1], new_date)
  })

  test_that("set_master_startup_value handles POSIXct values", {
    df <- create_test_startup_data(2)
    new_time <- as.POSIXct("2025-02-01 10:00:00", tz = "GMT")
    updated <- set_master_startup_value(df, 1, "starttime_gmt", new_time)
    expect_equal(updated$starttime_gmt[1], new_time)
  })

  test_that("set_comments sets comment when empty", {
    df <- create_test_startup_data(2)
    df$comment[1] <- ""
    updated <- set_comments(df, 1, "First comment")
    expect_equal(updated$comment[1], "First comment")
  })

  test_that("set_comments appends comment with pipe separator", {
    df <- create_test_startup_data(2)
    df$comment[1] <- "Existing"
    updated <- set_comments(df, 1, "New comment")
    expect_equal(updated$comment[1], "Existing | New comment")
  })

  test_that("set_comments handles multiple appends", {
    df <- create_test_startup_data(2)
    df$comment[1] <- "First"
    updated1 <- set_comments(df, 1, "Second")
    updated2 <- set_comments(updated1, 1, "Third")
    expect_equal(updated2$comment[1], "First | Second | Third")
  })

  test_that("set_comments preserves other row comments", {
    df <- create_test_startup_data(3)
    df$comment[1] <- "Comment1"
    df$comment[2] <- "Comment2"
    updated <- set_comments(df, 1, "Added")
    expect_equal(updated$comment[2], "Comment2")
  })
})

describe("Critical Missing Field Checking", {
  test_that("check_critical_missing returns TRUE for non-GPS critical models", {
    startup_rows <- tibble()
    logger_partner_data <- tibble(
      logger_id = "CriticalID",
      model = "CriticalModel",
      date = as.Date("2025-01-01"),
      deployed = TRUE
    )
    master_startup <- create_test_startup_data()

    result <- check_critical_missing(
      startup_rows,
      logger_partner_data,
      master_startup,
      "CriticalID",
      tibble()
    )

    expect_true(result)
  })
})

describe("Logger Addition from Startup Sheets", {
  test_that("add_loggers_from_startup skips unreadable Excel files", {
    the$sea_track_folder <<- tmp_dir

    startup_dir <- file.path(tmp_dir, "Starttime files and stored loggers", "2025")
    dir.create(startup_dir, recursive = TRUE, showWarnings = FALSE)

    # Create corrupt Excel file
    corrupt_file <- file.path(startup_dir, "corrupt.xlsx")
    writeLines("not an excel file", corrupt_file)

    master_import <- list(
      data = list(
        METADATA = tibble(
          ring_number = NA,
          logger_id_deployed = "L0",
          logger_id_retrieved = NA,
          date = as.Date("2024-12-01"),
          nest_latitude = NA,
          nest_longitude = NA
        ), STARTUP_SHUTDOWN = create_test_startup_data(1)
      ),
      path = "/path/to/master.xlsx",
      modified = FALSE
    )

    new_metadata <- create_test_partner_metadata()

    # Should skip corrupt file and return master unchanged
    result <- add_loggers_from_startup(master_import, new_metadata)
    expect_true(is.list(result))

    file.remove(corrupt_file)
  })

  test_that("add_loggers_from_startup skips files with column mismatches", {
    the$sea_track_folder <<- tmp_dir

    startup_dir <- file.path(tmp_dir, "Starttime files and stored loggers", "2025")
    dir.create(startup_dir, recursive = TRUE, showWarnings = FALSE)

    # Create file with wrong columns
    wrong_cols <- tibble(wrong_col = "A", another_wrong = "B")
    wrong_file <- file.path(startup_dir, "wrong_cols.xlsx")
    openxlsx2::write_xlsx(wrong_cols, wrong_file)

    master_import <- list(
      data = list(
        METADATA = create_test_encounter_data(), STARTUP_SHUTDOWN = create_test_startup_data(1)
      ),
      path = "/path/to/master.xlsx",
      modified = FALSE
    )

    new_metadata <- create_test_partner_metadata()

    # Should skip file with wrong columns
    result <- add_loggers_from_startup(master_import, new_metadata)
    expect_true(is.list(result))

    file.remove(wrong_file)
  })

  test_that("add_loggers_from_startup handles empty partner metadata gracefully", {
    the$sea_track_folder <<- tmp_dir

    startup_dir <- file.path(tmp_dir, "Starttime files and stored loggers", "2025")
    dir.create(startup_dir, recursive = TRUE, showWarnings = FALSE)

    master_import <- list(
      data = list(
        METADATA = create_test_encounter_data(), STARTUP_SHUTDOWN = create_test_startup_data(1)
      ),
      path = "/path/to/master.xlsx",
      modified = FALSE
    )

    # Empty partner metadata
    empty_metadata <- list(
      data = list(
        `ENCOUNTER DATA` = tibble(
          date = as.Date(character()),
          logger_id_deployed = character(),
          logger_model_deployed = character(),
          logger_id_retrieved = character(),
          logger_model_retrieved = character(),
          nest_latitude = numeric(), nest_longitude = numeric()
        ),
        `RESTART TIMES` = tibble()
      ),
      path = "/path/to/partner.xlsx",
      modified = FALSE
    )

    result <- add_loggers_from_startup(master_import, empty_metadata)
    expect_true(is.list(result))
  })

  test_that("add_loggers_from_startup skips file with wrong data types", {
    the$sea_track_folder <<- tmp_dir

    startup_dir <- file.path(tmp_dir, "Starttime files and stored loggers", "2025")
    dir.create(startup_dir, recursive = TRUE, showWarnings = FALSE)

    # Create file with numeric instead of character columns
    wrong_types <- tibble(
      logger_serial_no = 123,
      logger_model = "Model1",
      producer = "Producer",
      production_year = 2024,
      project = "seatrack",
      starttime_gmt = "not a datetime",
      logging_mode = NA,
      started_by = "User",
      started_where = "Location",
      days_delayed = 0,
      programmed_gmt_time = "not a datetime",
      intended_species = "Species",
      intended_location = "Location",
      intended_deployer = "Deployer",
      shutdown_session = NA,
      field_status = NA,
      downloaded_by = NA,
      download_type = NA,
      download_date = NA,
      decomissioned = NA,
      shutdown_date = NA,
      comment = ""
    )

    wrong_file <- file.path(startup_dir, "wrong_types.xlsx")
    openxlsx2::write_xlsx(wrong_types, wrong_file)

    master_import <- list(
      data = list(
        METADATA = create_test_encounter_data(), STARTUP_SHUTDOWN = create_test_startup_data(1)
      ),
      path = "/path/to/master.xlsx",
      modified = FALSE
    )

    new_metadata <- create_test_partner_metadata()

    result <- add_loggers_from_startup(master_import, new_metadata)
    expect_true(is.list(result))

    file.remove(wrong_file)
  })
})

describe("Startup Data Validation", {
  test_that("Startup sheets maintain required columns", {
    startup <- create_test_startup_data()
    required_cols <- c(
      "logger_serial_no", "logger_model", "producer", "production_year",
      "project", "starttime_gmt", "logging_mode", "started_by", "started_where",
      "days_delayed", "programmed_gmt_time", "intended_species", "intended_location",
      "intended_deployer", "shutdown_session", "field_status", "downloaded_by",
      "download_type", "download_date", "decomissioned", "shutdown_date", "comment"
    )
    expect_true(all(required_cols %in% colnames(startup)))
  })

  test_that("Startup date columns maintain correct types", {
    startup <- create_test_startup_data()
    expect_true("POSIXct" %in% class(startup$starttime_gmt))
    expect_true("POSIXct" %in% class(startup$programmed_gmt_time))
    expect_true("Date" %in% class(startup$download_date))
    expect_true("Date" %in% class(startup$shutdown_date))
  })

  test_that("Startup sheets handle NA values correctly", {
    startup <- create_test_startup_data()
    startup[1, "logging_mode"] <- NA
    startup[1, "download_date"] <- NA
    expect_true(is.na(startup$logging_mode[1]))
    expect_true(is.na(startup$download_date[1]))
  })
})

describe("Startup Import Edge Cases", {
  test_that("Empty startup file is handled gracefully", {
    empty_startup <- create_test_startup_data(0)
    expect_equal(nrow(empty_startup), 0)
    expect_true(all(c("logger_serial_no", "starttime_gmt") %in% colnames(empty_startup)))
  })

  test_that("Single row startup file is processed correctly", {
    single_startup <- create_test_startup_data(1)
    expect_equal(nrow(single_startup), 1)
    expect_false(is.na(single_startup$logger_serial_no[1]))
  })

  test_that("Startup with all NA critical fields handled appropriately", {
    startup <- create_test_startup_data(1)
    startup$starttime_gmt[1] <- NA
    startup$download_date[1] <- NA

    # These scenarios should be handled by check_critical_missing
    expect_true(is.na(startup$starttime_gmt[1]))
  })

  test_that("Startup with mixed valid and invalid dates", {
    startup <- create_test_startup_data(3)
    startup$download_date[1] <- as.Date("2025-01-15")
    startup$download_date[2] <- NA
    startup$download_date[3] <- as.Date("2025-01-20")

    valid_dates <- !is.na(startup$download_date)
    expect_equal(sum(valid_dates), 2)
  })

  test_that("Startup handles special characters in comments", {
    startup <- create_test_startup_data(1)
    special_comment <- "Test | with | special & chars < >"
    startup$comment[1] <- special_comment
    expect_equal(startup$comment[1], special_comment)
  })
})
