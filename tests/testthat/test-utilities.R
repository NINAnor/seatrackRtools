# Test script for utility functions (aaa.R, functions.R, nonresponsive.R)

library(testthat)
library(openxlsx2)
library(tibble)

# Create a temporary directory for file-based tests
tmp_dir <- tempdir()

describe("File System and Logging Operations", {
  test_that("start_logging creates log file", {
    log_file <- paste0("seatrack_functions_log_", Sys.Date(), ".txt")
    start_logging(tmp_dir, log_file)
    expect_true(file.exists(file.path(tmp_dir, log_file)))
  })

  test_that("set_sea_track_folder sets global variable and logs", {
    set_sea_track_folder(tmp_dir)
    expect_true(exists("sea_track_folder", envir = the))
    unlink(".Renviron")
  })
})

describe("Sheet Loading Operations", {
  test_that("load_sheets_as_list loads sheets", {
    test_file <- file.path(tmp_dir, "test.xlsx")
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("Sheet1")
    wb$add_worksheet("Sheet2")
    wb$add_data("Sheet1", data.frame(a = 1:3, b = 4:6))
    wb$add_data("Sheet2", data.frame(x = 7:9, y = 10:12))
    openxlsx2::wb_save(wb, test_file)
    sheets <- load_sheets_as_list(test_file, c("Sheet1", "Sheet2"))
    expect_equal(length(sheets$data), 2)
    expect_true(all(sapply(sheets$data, is.data.frame)))
    file.remove(test_file)
  })

  test_that("load_sheets_as_list with column types", {
    test_file <- file.path(tmp_dir, "test_types.xlsx")
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("Data")
    df <- data.frame(
      text_col = "A",
      num_col = 1,
      date_col = as.Date("2025-01-01")
    )
    wb$add_data("Data", df)
    openxlsx2::wb_save(wb, test_file)
    
    sheets <- load_sheets_as_list(test_file, "Data")
    expect_true(is.data.frame(sheets$data$Data))
    expect_equal(nrow(sheets$data$Data), 1)
    
    file.remove(test_file)
  })

  test_that("load_sheets_as_list preserves sheet names", {
    test_file <- file.path(tmp_dir, "test_names.xlsx")
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("CustomSheet1")
    wb$add_worksheet("CustomSheet2")
    wb$add_data("CustomSheet1", data.frame(a = 1))
    wb$add_data("CustomSheet2", data.frame(b = 2))
    openxlsx2::wb_save(wb, test_file)
    
    sheets <- load_sheets_as_list(test_file, c("CustomSheet1", "CustomSheet2"))
    expect_true("CustomSheet1" %in% names(sheets$data))
    expect_true("CustomSheet2" %in% names(sheets$data))
    
    file.remove(test_file)
  })
})

describe("Nonresponsive Logger Management", {
  test_that("load_nonresponsive loads existing file for Lotek", {
    file_path <- file.path(tmp_dir, "lotek_unresponsive.xlsx")
    df <- tibble(
      logger_serial_no = "L1",
      logger_model = "birdTracker5000",
      producer = "Lotek",
      production_year = 2024,
      project = "seatrack",
      starttime_gmt = as.POSIXct("2025-01-01"),
      download_type = "Nonresponsive",
      download_date = as.Date("2025-01-10"),
      comment = "No response"
    )
    openxlsx2::write_xlsx(df, file_path)
    result <- load_nonresponsive(c(file_path), c("Lotek"))
    expect_equal(nrow(result$sheets_list$lotek$data[[1]]), 1)
    expect_equal(result$sheets_list$lotek$data[[1]]$producer[1], "Lotek")
    file.remove(file_path)
  })

  test_that("load_nonresponsive initializes empty sheet if file missing", {
    file_path <- file.path(tmp_dir, "missing_lotek_unresponsive.xlsx")
    result <- load_nonresponsive(file_path, "Lotek")
    expect_equal(nrow(result$sheets_list$lotek$data[[1]]), 0)
    expect_true(all(c(
      "logger_serial_no", "logger_model", "producer", "production_year", "project",
      "starttime_gmt", "logging_mode", "days_delayed", "programmed_gmt_time",
      "download_type", "download_date", "comment", "priority"
    ) %in% names(result$sheets_list$lotek$data[[1]])))
  })

  test_that("load_nonresponsive handles multiple manufacturers", {
    lotek_file <- file.path(tmp_dir, "lotek_nonresp.xlsx")
    migrate_file <- file.path(tmp_dir, "migrate_nonresp.xlsx")
    
    lotek_df <- tibble(
      logger_serial_no = "L1",
      logger_model = "birdTracker5000",
      producer = "Lotek",
      production_year = 2024,
      project = "seatrack",
      starttime_gmt = as.POSIXct("2025-01-01"),
      download_type = "Nonresponsive",
      download_date = as.Date("2025-01-10"),
      comment = "No response"
    )
    
    migrate_df <- tibble(
      logger_serial_no = "L2",
      logger_model = "MigrationModel",
      producer = "MigrateTech",
      production_year = 2023,
      project = "seatrack",
      starttime_gmt = as.POSIXct("2025-01-02"),
      download_type = "Nonresponsive",
      download_date = as.Date("2025-01-11"),
      comment = "No response"
    )
    
    openxlsx2::write_xlsx(lotek_df, lotek_file)
    openxlsx2::write_xlsx(migrate_df, migrate_file)
    
    result <- load_nonresponsive(c(lotek_file, migrate_file), c("Lotek", "MigrateTech"))
    
    expect_true("lotek" %in% names(result$sheets_list))
    expect_true("migratetech" %in% names(result$sheets_list))
    
    file.remove(lotek_file)
    file.remove(migrate_file)
  })

  test_that("load_nonresponsive creates correct sheet structure", {
    file_path <- file.path(tmp_dir, "test_nonresp.xlsx")
    df <- tibble(
      logger_serial_no = "L1",
      logger_model = "Model1",
      producer = "Producer1",
      production_year = 2024,
      project = "test",
      starttime_gmt = as.POSIXct("2025-01-01"),
      download_type = "Nonresponsive",
      download_date = as.Date("2025-01-10"),
      comment = "Test"
    )
    openxlsx2::write_xlsx(df, file_path)
    
    result <- load_nonresponsive(file_path, "TestProducer")
    
    expect_true(is.list(result$sheets_list))
    expect_true("testproducer" %in% names(result$sheets_list))
    expect_true(is.list(result$sheets_list$testproducer$data))
    
    file.remove(file_path)
  })
})

describe("Colony Location Operations", {
  test_that("get_all_locations fails if sea track folder is not set", {
    the$sea_track_folder <<- NULL
    expect_error(get_all_locations(), "Sea track folder is not set")
  })

  test_that("get_all_locations fails if Locations folder doesn't exist", {
    the$sea_track_folder <<- tmp_dir
    locations_path <- file.path(tmp_dir, "Locations")
    unlink(locations_path, recursive = TRUE)
    expect_error(get_all_locations(), "Locations folder not found")
  })

  test_that("get_all_locations returns correct structure", {
    the$sea_track_folder <<- tmp_dir
    locations_path <- file.path(tmp_dir, "Locations")
    
    # Make sure clean slate for testing
    unlink(locations_path, recursive = TRUE)
    # Create test directory structure
    dir.create(file.path(locations_path, "Norway", "Jan Mayen"), recursive = TRUE)
    dir.create(file.path(locations_path, "Norway", "Sklinna"), recursive = TRUE)
    dir.create(file.path(locations_path, "Finland", "Tvärminne"), recursive = TRUE)

    colonies <- get_all_locations()
    expect_type(colonies, "list")
    expect_named(colonies, c("Finland", "Norway"))
    expect_equal(colonies$Norway, c("Jan Mayen", "Sklinna"))
    expect_equal(colonies$Finland, "Tvärminne")

    # Clean up test directories
    unlink(locations_path, recursive = TRUE)
  })

  test_that("get_all_locations handles single country", {
    the$sea_track_folder <<- tmp_dir
    locations_path <- file.path(tmp_dir, "Locations")
    
    dir.create(file.path(locations_path, "Iceland", "Breiðafjörður"), recursive = TRUE)
    dir.create(file.path(locations_path, "Iceland", "Dyrhólaey"), recursive = TRUE)

    colonies <- get_all_locations()
    expect_type(colonies, "list")
    expect_equal(length(colonies), 1)
    expect_true("Iceland" %in% names(colonies))
    expect_equal(length(colonies$Iceland), 2)

    unlink(locations_path, recursive = TRUE)
  })

  test_that("get_all_locations handles special characters in names", {
    the$sea_track_folder <<- tmp_dir
    locations_path <- file.path(tmp_dir, "Locations")
    
    # Create directories with special characters (if filesystem allows)
    dir.create(file.path(locations_path, "Norway", "Ålaskåta"), recursive = TRUE, showWarnings = FALSE)
    
    colonies <- get_all_locations()
    expect_type(colonies, "list")

    unlink(locations_path, recursive = TRUE)
  })
})

describe("Sheet Saving and File Operations", {
  test_that("save_master_sheet writes xlsx file with LoadedWB", {
    # Create test data list
    data_list <- list(
      "Sheet1" = tibble(a = 1:3, b = 4:6),
      "Sheet2" = tibble(x = 7:9, y = 10:12)
    )
    # Create LoadedWB instance
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("Sheet1")
    wb$add_worksheet("Sheet2")

    loaded_wb <- LoadedWB$new(data = data_list, wb = wb)
    
    out_file <- file.path(tmp_dir, "out.xlsx")
    save_master_sheet(loaded_wb, out_file)
    expect_true(file.exists(out_file))
    
    # Verify the saved file contains the correct data
    result <- load_sheets_as_list(out_file, c("Sheet1", "Sheet2"))
    expect_equal(result$data$Sheet1, data_list$Sheet1)
    expect_equal(result$data$Sheet2, data_list$Sheet2)
    
    file.remove(out_file)
  })

  test_that("save_master_sheet preserves data types", {
    data_list <- list(
      "Data" = tibble(
        text = "value",
        number = 42,
        date = as.Date("2025-01-01")
      )
    )
    
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("Data")
    
    loaded_wb <- LoadedWB$new(data = data_list, wb = wb)
    out_file <- file.path(tmp_dir, "typed_output.xlsx")
    
    save_master_sheet(loaded_wb, out_file)
    expect_true(file.exists(out_file))
    
    file.remove(out_file)
  })

  test_that("save_master_sheet handles empty data frames", {
    data_list <- list(
      "Empty" = tibble(col1 = character(), col2 = numeric())
    )
    
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("Empty")
    
    loaded_wb <- LoadedWB$new(data = data_list, wb = wb)
    out_file <- file.path(tmp_dir, "empty_output.xlsx")
    
    save_master_sheet(loaded_wb, out_file)
    expect_true(file.exists(out_file))
    
    file.remove(out_file)
  })
})
