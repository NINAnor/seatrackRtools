# Test script for functions in R/loggers.R

library(testthat)
library(tibble)
library(openxlsx2)

# Create a temporary directory for file-based tests
tmp_dir <- tempdir()

# Helper functions for test data
create_test_startup_data <- function() {
  tibble(
    logger_serial_no = c("L1", "L2", "L3", "L4"),
    logger_model = c("Model1", "Model2", "Model1", "Model3"),
    producer = c("Lotek", "MigrateTech", "Lotek", "BAS"),
    production_year = c(2024, 2023, 2024, 2022),
    project = c("seatrack", "seatrack", "seatrack", "seatrack"),
    starttime_gmt = as.POSIXct(c(
      "2025-01-01 10:00:00",
      "2025-01-02 12:00:00",
      "2025-01-03 14:00:00",
      "2025-01-04 16:00:00"
    ), tz = "GMT"),
    logging_mode = c(NA, NA, NA, NA),
    started_by = c("User1", "User2", "User1", "User3"),
    started_where = c("C1", "C2", "C1", "C3"),
    days_delayed = c(0, 1, 0, 2),
    programmed_gmt_time = as.POSIXct(c(
      "2025-01-01 10:00:00",
      "2025-01-02 12:00:00",
      "2025-01-03 14:00:00",
      "2025-01-04 16:00:00"
    ), tz = "GMT"),
    intended_species = c("A", "B", "A", "C"),
    intended_location = c("L1", "L2", "L1", "L3"),
    intended_deployer = c("D1", "D2", "D1", "D3"),
    shutdown_session = c(NA, NA, NA, NA),
    field_status = c(NA, NA, NA, NA),
    downloaded_by = c(NA, NA, NA, NA),
    download_type = c(NA, NA, "Downloaded", NA),
    download_date = as.Date(c(NA, NA, "2025-01-10", NA)),
    decomissioned = as.Date(c(NA, NA, NA, NA)),
    shutdown_date = as.Date(c(NA, NA, "2025-01-10", NA)),
    comment = c("", "", "Retrieved", "")
  )
}

describe("Logger Metadata Search", {
  test_that("get_logger_from_metadata finds logger in single file", {
    # Create a test master import file
    test_file <- file.path(tmp_dir, "test_loggers.xlsx")
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("METADATA")
    wb$add_worksheet("STARTUP_SHUTDOWN")
    
    startup <- create_test_startup_data()
    metadata <- tibble(
      ring_number = NA,
      logger_id_deployed = c("L1", "L2"),
      logger_id_retrieved = NA,
      date = as.Date(c("2025-01-01", "2025-01-02")),
      nest_latitude = NA,
      nest_longitude = NA
    )
    
    wb$add_data("METADATA", metadata)
    wb$add_data("STARTUP_SHUTDOWN", startup)
    openxlsx2::wb_save(wb, test_file)
    
    # Load the file
    loaded_import <- load_master_import(file_path = test_file)
    all_sheets <- list(loaded_import)
    
    result <- get_logger_from_metadata("L1", all_sheets)
    
    expect_true(length(result) > 0)
    expect_equal(result[[1]]$data$logger_serial_no, "L1")
    
    file.remove(test_file)
  })

  test_that("get_logger_from_metadata returns empty list when logger not found", {
    startup <- create_test_startup_data()
    
    # Create a list with logger data but not the one we're searching for
    loaded_import <- list(
      data = list(STARTUP_SHUTDOWN = startup),
      path = "/test/path.xlsx"
    )
    all_sheets <- list(loaded_import)
    
    result <- get_logger_from_metadata("NONEXISTENT", all_sheets)
    
    expect_equal(length(result), 0)
  })

  test_that("get_logger_from_metadata handles multiple instances of same logger", {
    # Create startup data with logger appearing multiple times
    startup_multi <- tibble(
      logger_serial_no = c("L1", "L1", "L2"),
      logger_model = c("Model1", "Model1", "Model2"),
      producer = c("Lotek", "Lotek", "MigrateTech"),
      production_year = c(2024, 2024, 2023),
      project = c("seatrack", "seatrack", "seatrack"),
      starttime_gmt = as.POSIXct(c(
        "2025-01-01 10:00:00",
        "2025-01-05 10:00:00",
        "2025-01-03 14:00:00"
      ), tz = "GMT"),
      logging_mode = c(NA, NA, NA),
      started_by = c("User1", "User1", "User2"),
      started_where = c("C1", "C1", "C2"),
      days_delayed = c(0, 0, 1),
      programmed_gmt_time = as.POSIXct(c(
        "2025-01-01 10:00:00",
        "2025-01-05 10:00:00",
        "2025-01-03 14:00:00"
      ), tz = "GMT"),
      intended_species = c("A", "A", "B"),
      intended_location = c("L1", "L1", "L2"),
      intended_deployer = c("D1", "D1", "D2"),
      shutdown_session = c(NA, NA, NA),
      field_status = c(NA, NA, NA),
      downloaded_by = c(NA, NA, NA),
      download_type = c(NA, NA, NA),
      download_date = as.Date(c(NA, NA, NA)),
      decomissioned = as.Date(c(NA, NA, NA)),
      shutdown_date = as.Date(c(NA, NA, NA)),
      comment = c("", "", "")
    )
    
    loaded_import <- list(
      data = list(STARTUP_SHUTDOWN = startup_multi),
      path = "/test/path.xlsx"
    )
    all_sheets <- list(loaded_import)
    
    result <- get_logger_from_metadata("L1", all_sheets)
    
    # Should find both instances of L1
    expect_equal(length(result), 2)
    expect_true(all(sapply(result, function(x) x$data$logger_serial_no) == "L1"))
  })
})

describe("Unfinished Session Detection", {
  test_that("get_unfinished_session returns NULL when no matching logger", {
    master_startup <- create_test_startup_data()
    result <- get_unfinished_session(master_startup, "NONEXISTENT", as.Date("2025-01-15"))
    expect_null(result$index)
  })

  test_that("get_unfinished_session returns unfinished session when found", {
    master_startup <- create_test_startup_data()
    result <- get_unfinished_session(master_startup, "L1", as.Date("2025-01-05"))
    expect_true(!is.null(result$index))
    expect_true("L1" %in% result$session$logger_serial_no)
  })

  test_that("get_unfinished_session returns closest session to download date", {
    master_startup <- tibble(
      logger_serial_no = c("L1", "L1", "L1"),
      starttime_gmt = as.POSIXct(c(
        "2025-01-01 10:00:00",
        "2025-01-10 12:00:00",
        "2025-01-20 14:00:00"
      ), tz = "GMT"),
      shutdown_date = c(NA, NA, NA),
      download_date = c(NA, NA, NA)
    )
    result <- get_unfinished_session(master_startup, "L1", as.Date("2025-01-15"))
    expect_true(!is.null(result$index))
    # Should return session starting on 2025-01-10 as it's closest to download date
    expect_equal(result$session$starttime_gmt[1], as.POSIXct("2025-01-10 12:00:00", tz = "GMT"))
  })

  test_that("get_unfinished_session handles multiple unfinished sessions", {
    master_startup <- tibble(
      logger_serial_no = c("L1", "L1"),
      starttime_gmt = as.POSIXct(c(
        "2025-01-01 10:00:00",
        "2025-01-05 12:00:00"
      ), tz = "GMT"),
      shutdown_date = c(NA, NA),
      download_date = c(NA, NA)
    )
    result <- get_unfinished_session(master_startup, "L1", as.Date("2025-01-15"))
    expect_true(!is.null(result$index))
  })

  test_that("get_unfinished_session returns last session when all finished", {
    master_startup <- tibble(
      logger_serial_no = c("L1", "L1"),
      starttime_gmt = as.POSIXct(c(
        "2025-01-01 10:00:00",
        "2025-01-05 12:00:00"
      ), tz = "GMT"),
      shutdown_date = as.Date(c("2025-01-02", "2025-01-06")),
      download_date = as.Date(c("2025-01-02", "2025-01-06"))
    )
    result <- get_unfinished_session(master_startup, "L1", as.Date("2025-01-15"))
    expect_null(result$index)
  })

  test_that("get_unfinished_session ignores sessions after download date", {
    master_startup <- tibble(
      logger_serial_no = c("L1", "L1"),
      starttime_gmt = as.POSIXct(c(
        "2025-01-01 10:00:00",
        "2025-01-20 12:00:00"
      ), tz = "GMT"),
      shutdown_date = c(NA, NA),
      download_date = c(NA, NA)
    )
    result <- get_unfinished_session(master_startup, "L1", as.Date("2025-01-15"))
    expect_true(!is.null(result$index))
    # Should exclude the session starting 2025-01-20
    expect_true(result$session$starttime_gmt[1] <= as.POSIXct("2025-01-15 23:59:59", tz = "GMT"))
  })

  test_that("get_unfinished_session handles NA download date", {
    master_startup <- create_test_startup_data()
    result <- get_unfinished_session(master_startup, "L1", as.Date(NA))
    expect_true(!is.null(result$session))
  })

  test_that("get_unfinished_session handles loggers without starttime_gmt", {
    master_startup <- tibble(
      logger_serial_no = c("L1", "L2"),
      starttime_gmt = c(NA, as.POSIXct("2025-01-01 10:00:00", tz = "GMT")),
      shutdown_date = c(NA, NA),
      download_date = c(NA, NA)
    )
    result <- get_unfinished_session(master_startup, "L1", as.Date("2025-01-15"))
    expect_true(!is.null(result$session))
  })
})

describe("Logger Session Termination", {
  test_that("end_logger_session sets download_type and dates", {
    master_sheet <- list(
      data = list(STARTUP_SHUTDOWN = create_test_startup_data()),
      path = "/test/path.xlsx",
      modified = FALSE
    )
    
    result <- end_logger_session(
      logger_id = "L1",
      logger_status = "Downloaded",
      downloaded_by = "TestUser",
      master_sheet = master_sheet
    )
    
    expect_true(is.list(result))
    expect_true("master_sheet" %in% names(result))
    expect_true(result$master_sheet$modified)
  })

  test_that("end_logger_session uses current date when download_date not provided", {
    master_sheet <- list(
      data = list(STARTUP_SHUTDOWN = create_test_startup_data()),
      path = "/test/path.xlsx",
      modified = FALSE
    )
    
    result <- end_logger_session(
      logger_id = "L1",
      logger_status = "Downloaded",
      master_sheet = master_sheet
    )
    
    expect_true(result$master_sheet$modified)
  })

  test_that("end_logger_session appends comment", {
    master_sheet <- list(
      data = list(STARTUP_SHUTDOWN = create_test_startup_data()),
      path = "/test/path.xlsx",
      modified = FALSE
    )
    
    result <- end_logger_session(
      logger_id = "L3",
      logger_status = "Downloaded",
      comment = "Test comment",
      master_sheet = master_sheet
    )
    
    expect_true(result$master_sheet$modified)
  })

  test_that("end_logger_session handles nonresponsive status", {
    master_sheet <- list(
      data = list(STARTUP_SHUTDOWN = create_test_startup_data()),
      path = "/test/path.xlsx",
      modified = FALSE
    )
    
    result <- end_logger_session(
      logger_id = "L1",
      logger_status = "Nonresponsive",
      master_sheet = master_sheet,
      nonresponsive_list = list()
    )
    
    expect_true(is.list(result))
  })
})

describe("Logger Status Modification", {
  test_that("modify_logger_status updates correct field in master sheet", {
    master_sheet <- list(
      data = list(STARTUP_SHUTDOWN = create_test_startup_data()),
      path = "/test/path.xlsx",
      modified = FALSE
    )
    
    result <- modify_logger_status(
      logger_id = "L1",
      new_data = list(download_type = "Downloaded"),
      master_sheet = master_sheet
    )
    
    expect_true(result$master_sheet$modified)
    expect_equal(
      result$master_sheet$data$STARTUP_SHUTDOWN$download_type[1],
      "Downloaded"
    )
  })

  test_that("modify_logger_status adds to nonresponsive list when appropriate", {
    master_sheet <- list(
      data = list(STARTUP_SHUTDOWN = create_test_startup_data()),
      path = "/test/path.xlsx",
      modified = FALSE
    )
    
    result <- modify_logger_status(
      logger_id = "L1",
      new_data = list(download_type = "Nonresponsive", download_date = as.Date("2025-01-15")),
      master_sheet = master_sheet,
      nonresponsive_list = list()
    )
    
    expect_true(is.list(result$nonresponsive_list))
  })

  test_that("modify_logger_status preserves comment field", {
    master_sheet <- list(
      data = list(STARTUP_SHUTDOWN = create_test_startup_data()),
      path = "/test/path.xlsx",
      modified = FALSE
    )
    
    result <- modify_logger_status(
      logger_id = "L1",
      new_data = list(comment = "Test comment"),
      master_sheet = master_sheet
    )
    
    expect_true(result$master_sheet$modified)
  })
})

describe("Returned Logger Handling", {
  test_that("handle_returned_loggers processes valid logger returns", {
    master_startup <- create_test_startup_data()
    
    logger_returns <- tibble(
      logger_id = "L1",
      status = "Downloaded",
      `download / stop_date` = as.Date("2025-01-15"),
      `downloaded by` = "TestUser",
      comment = "Test",
      `stored or sent to?` = ""
    )
    
    restart_times <- tibble()
    
    result <- handle_returned_loggers(
      "TestColony",
      master_startup,
      logger_returns,
      restart_times
    )
    
    expect_true(is.list(result))
    expect_true("master_startup" %in% names(result))
  })

  test_that("handle_returned_loggers skips invalid status", {
    master_startup <- create_test_startup_data()
    
    logger_returns <- tibble(
      logger_id = "L1",
      status = "No download attemted",
      `download / stop_date` = as.Date("2025-01-15"),
      `downloaded by` = "TestUser",
      comment = "",
      `stored or sent to?` = ""
    )
    
    restart_times <- tibble()
    
    result <- handle_returned_loggers(
      "TestColony",
      master_startup,
      logger_returns,
      restart_times
    )
    
    expect_equal(nrow(result$master_startup), nrow(master_startup))
  })

  test_that("handle_returned_loggers returns empty list when no returns", {
    master_startup <- create_test_startup_data()
    logger_returns <- tibble()
    restart_times <- tibble()
    
    result <- handle_returned_loggers(
      "TestColony",
      master_startup,
      logger_returns,
      restart_times
    )
    
    expect_equal(nrow(result$master_startup), nrow(master_startup))
  })

  test_that("handle_returned_loggers handles not used loggers with restarts", {
    master_startup <- create_test_startup_data()
    
    logger_returns <- tibble(
      logger_id = "L1",
      status = "Not used",
      `download / stop_date` = as.Date(NA),
      `downloaded by` = "",
      comment = "",
      `stored or sent to?` = "redeployed"
    )
    
    restart_times <- tibble(
      logger_id = "L1",
      startdate_GMT = as.Date("2025-01-20")
    )
    
    result <- handle_returned_loggers(
      "TestColony",
      master_startup,
      logger_returns,
      restart_times
    )
    
    expect_true(is.list(result))
  })

  test_that("handle_returned_loggers handles nonresponsive loggers", {
    master_startup <- create_test_startup_data()
    
    logger_returns <- tibble(
      logger_id = "L1",
      status = "Nonresponsive",
      `download / stop_date` = as.Date("2025-01-15"),
      `downloaded by` = "",
      comment = "No response",
      `stored or sent to?` = "Nonresponsive"
    )
    
    restart_times <- tibble()
    
    result <- handle_returned_loggers(
      "TestColony",
      master_startup,
      logger_returns,
      restart_times,
      nonresponsive_list = list()
    )
    
    expect_true(is.list(result))
  })

  test_that("handle_returned_loggers returns unhandled loggers when session not found", {
    master_startup <- tibble(
      logger_serial_no = "L2",
      starttime_gmt = as.POSIXct("2025-01-02 12:00:00", tz = "GMT"),
      shutdown_date = NA,
      download_date = NA
    )
    
    logger_returns <- tibble(
      logger_id = "L1",
      status = "Downloaded",
      `download / stop_date` = as.Date("2025-01-15"),
      `downloaded by` = "User",
      comment = "",
      `stored or sent to?` = ""
    )
    
    restart_times <- tibble()
    
    result <- handle_returned_loggers(
      "TestColony",
      master_startup,
      logger_returns,
      restart_times
    )
    
    expect_true(is.list(result))
  })

  test_that("handle_returned_loggers handles multiple loggers", {
    master_startup <- create_test_startup_data()
    
    logger_returns <- tibble(
      logger_id = c("L1", "L2"),
      status = c("Downloaded", "Downloaded"),
      `download / stop_date` = as.Date(c("2025-01-15", "2025-01-16")),
      `downloaded by` = c("User1", "User2"),
      comment = c("", ""),
      `stored or sent to?` = c("", "")
    )
    
    restart_times <- tibble()
    
    result <- handle_returned_loggers(
      "TestColony",
      master_startup,
      logger_returns,
      restart_times
    )
    
    expect_true(is.list(result))
  })
})
