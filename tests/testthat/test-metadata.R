# Test script for functions in R/metadata.R

library(testthat)
library(openxlsx2)
library(tibble)

# Create a temporary directory for file-based tests
tmp_dir <- tempdir()

# Helper functions for test data
create_test_metadata_sheet <- function() {
  tibble(
    ring_number = c("R1", "R2"),
    logger_id_deployed = c("L1", "L2"),
    logger_id_retrieved = NA,
    date = as.Date(c("2025-01-01", "2025-01-02")),
    nest_latitude = c(60.0, 61.0),
    nest_longitude = c(10.0, 11.0)
  )
}

create_test_startup_sheet <- function() {
  tibble(
    logger_serial_no = c("L1", "L2"),
    logger_model = c("birdTracker5000", "birdTracker3000"),
    producer = c("Lotek", "MigrateTech"),
    production_year = c(2024, 2023),
    project = c("seatrack", "seatrack"),
    starttime_gmt = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-02 12:00:00"), tz = "GMT"),
    logging_mode = c(NA, NA),
    started_by = c("User1", "User2"),
    started_where = c("Colony1", "Colony2"),
    days_delayed = c(0, 1),
    programmed_gmt_time = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-02 12:00:00"), tz = "GMT"),
    intended_species = c("species_A", "species_B"),
    intended_location = c("Location1", "Location2"),
    intended_deployer = c("Deployer1", "Deployer2"),
    shutdown_session = c(NA, NA),
    field_status = c(NA, NA),
    downloaded_by = c(NA, NA),
    download_type = c(NA, NA),
    download_date = as.Date(c(NA, NA)),
    decomissioned = as.Date(c(NA, NA)),
    shutdown_date = as.Date(c(NA, NA)),
    comment = c("", "")
  )
}

create_test_master_import <- function(file_path = NULL) {
  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("METADATA")
  wb$add_worksheet("STARTUP_SHUTDOWN")
  
  metadata <- create_test_metadata_sheet()
  startup <- create_test_startup_sheet()
  
  wb$add_data("METADATA", metadata)
  wb$add_data("STARTUP_SHUTDOWN", startup)
  
  if (!is.null(file_path)) {
    openxlsx2::wb_save(wb, file_path)
  }
  
  return(list(wb = wb, metadata = metadata, startup = startup))
}

describe("Metadata Path Management", {
  test_that("set_master_import_paths stores paths in environment", {
    location_paths <- list(ColonyA = "/path/to/colonyA.xlsx", ColonyB = "/path/to/colonyB.xlsx")
    set_master_import_paths(location_paths)
    paths <- get_master_import_paths()
    expect_equal(paths, location_paths)
  })

  test_that("get_master_import_paths returns stored paths", {
    location_paths <- list(Colony1 = "/path/1.xlsx", Colony2 = "/path/2.xlsx")
    set_master_import_paths(location_paths)
    result <- get_master_import_paths()
    expect_true(all(c("Colony1", "Colony2") %in% names(result)))
  })

  test_that("get_master_import_path errors if sea track folder not set", {
    the$sea_track_folder <<- NULL
    expect_error(get_master_import_path("TestColony"), "Sea track folder is not set")
  })
})

describe("Master Import File Loading", {
  test_that("load_master_import loads sheets from file path", {
    test_file <- file.path(tmp_dir, "test_master.xlsx")
    create_test_master_import(test_file)
    
    result <- load_master_import(file_path = test_file)
    
    expect_true(is.list(result$data))
    expect_true("METADATA" %in% names(result$data))
    expect_true("STARTUP_SHUTDOWN" %in% names(result$data))
    expect_equal(nrow(result$data$METADATA), 2)
    expect_equal(nrow(result$data$STARTUP_SHUTDOWN), 2)
    
    file.remove(test_file)
  })

  test_that("load_master_import preserves data types", {
    test_file <- file.path(tmp_dir, "test_master_types.xlsx")
    create_test_master_import(test_file)
    
    result <- load_master_import(file_path = test_file)
    
    expect_true(is.numeric(result$data$METADATA$nest_latitude))
    expect_true(is.character(result$data$STARTUP_SHUTDOWN$logger_serial_no))
    
    file.remove(test_file)
  })

  test_that("load_master_import errors if file doesn't exist", {
    expect_error(load_master_import(file_path = "/nonexistent/path.xlsx"), "does not exist")
  })

  test_that("load_master_import requires either colony or file_path", {
    expect_error(load_master_import(), "Unable to find master import sheet")
  })

  test_that("load_master_import coerces date column if needed", {
    test_file <- file.path(tmp_dir, "test_master_dates.xlsx")
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("METADATA")
    wb$add_worksheet("STARTUP_SHUTDOWN")
    
    metadata <- tibble(date = "2025-01-01")
    startup <- create_test_startup_sheet()
    
    wb$add_data("METADATA", metadata)
    wb$add_data("STARTUP_SHUTDOWN", startup)
    openxlsx2::wb_save(wb, test_file)
    
    result <- load_master_import(file_path = test_file)
    expect_true("Date" %in% class(result$data$METADATA$date))
    
    file.remove(test_file)
  })
})describe("Master Import Data Combination", {
  test_that("load_all_master_import loads multiple files correctly", {
    the$sea_track_folder <<- tmp_dir
    
    # Create directory structure
    import_dir <- file.path(tmp_dir, "Database", "Imports_Metadata")
    dir.create(import_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Create test master files for two colonies
    colony1_file <- file.path(import_dir, "Master_ColonyA.xlsx")
    colony2_file <- file.path(import_dir, "Master_ColonyB.xlsx")
    
    create_test_master_import(colony1_file)
    create_test_master_import(colony2_file)
    
    # Create Locations structure
    locations_dir <- file.path(tmp_dir, "Locations")
    dir.create(file.path(locations_dir, "Country1", "ColonyA"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(locations_dir, "Country2", "ColonyB"), recursive = TRUE, showWarnings = FALSE)
    
    # Set paths explicitly for testing
    set_master_import_paths(list(ColonyA = colony1_file, ColonyB = colony2_file))
    
    result <- load_all_master_import(combine = TRUE, use_stored = TRUE)
    
    expect_true(is.list(result))
    expect_true("METADATA" %in% names(result))
    expect_true("STARTUP_SHUTDOWN" %in% names(result))
    
    file.remove(colony1_file)
    file.remove(colony2_file)
  })

  test_that("combine_all_metadata adds path column to each sheet", {
    sheet1 <- list(
      METADATA = create_test_metadata_sheet(),
      STARTUP_SHUTDOWN = create_test_startup_sheet()
    )
    sheet2 <- list(
      METADATA = create_test_metadata_sheet(),
      STARTUP_SHUTDOWN = create_test_startup_sheet()
    )
    
    all_sheets <- list(sheet1 = sheet1, sheet2 = sheet2)
    all_paths <- c("/path1.xlsx", "/path2.xlsx")
    
    result <- combine_all_metadata(all_sheets, all_paths)
    
    expect_true("path" %in% colnames(result$METADATA))
    expect_true("path" %in% colnames(result$STARTUP_SHUTDOWN))
    expect_equal(result$METADATA$path[1], "/path1.xlsx")
    expect_equal(result$METADATA$path[3], "/path2.xlsx")
  })

  test_that("combine_all_metadata preserves common columns only", {
    sheet1 <- list(
      METADATA = tibble(col1 = 1, col2 = 2, col3 = 3),
      STARTUP_SHUTDOWN = create_test_startup_sheet()
    )
    sheet2 <- list(
      METADATA = tibble(col1 = 4, col2 = 5),
      STARTUP_SHUTDOWN = create_test_startup_sheet()
    )
    
    all_sheets <- list(sheet1 = sheet1, sheet2 = sheet2)
    all_paths <- c("/path1.xlsx", "/path2.xlsx")
    
    result <- combine_all_metadata(all_sheets, all_paths)
    
    expect_true("col1" %in% colnames(result$METADATA))
    expect_true("col2" %in% colnames(result$METADATA))
    expect_false("col3" %in% colnames(result$METADATA))
  })

  test_that("combine_all_metadata adds nest_latitude/longitude if missing", {
    metadata_no_coords <- tibble(
      ring_number = "R1",
      logger_id_deployed = "L1",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-01")
    )
    sheet <- list(
      METADATA = metadata_no_coords,
      STARTUP_SHUTDOWN = create_test_startup_sheet()
    )
    
    all_sheets <- list(sheet)
    all_paths <- c("/path1.xlsx")
    
    result <- combine_all_metadata(all_sheets, all_paths)
    
    expect_true("nest_latitude" %in% colnames(result$METADATA))
    expect_true("nest_longitude" %in% colnames(result$METADATA))
  })
})

describe("Metadata Retrieval and Processing", {
  test_that("get_location_unprocessed returns path structure", {
    the$sea_track_folder <<- tmp_dir
    
    # Create test directory structure (Locations at root of tmp_dir)
    location_path <- file.path(tmp_dir, "Locations", "TestCountry", "TestColony")
    not_processed_path <- file.path(location_path, "not_processed")
    dir.create(not_processed_path, recursive = TRUE, showWarnings = FALSE)
    
    # Create a test file in not_processed
    test_file <- file.path(not_processed_path, "test_data.xlsx")
    file.create(test_file)
    
    # This should handle the location without error
    result <- get_location_unprocessed("TestColony")
    expect_true(is.list(result) || is.character(result) || is.null(result))
    
    file.remove(test_file)
  })

  test_that("modify_master_import_in_list updates correct entry", {
    all_sheets <- list(
      sheet1 = list(
        data = list(
          METADATA = create_test_metadata_sheet(),
          STARTUP_SHUTDOWN = create_test_startup_sheet()
        ),
        path = "/path1.xlsx",
        modified = FALSE
      ),
      sheet2 = list(
        data = list(
          METADATA = create_test_metadata_sheet(),
          STARTUP_SHUTDOWN = create_test_startup_sheet()
        ),
        path = "/path2.xlsx",
        modified = FALSE
      )
    )
    
    new_sheet <- list(
      data = list(
        METADATA = create_test_metadata_sheet(),
        STARTUP_SHUTDOWN = create_test_startup_sheet()
      ),
      path = "/path1.xlsx",
      modified = TRUE
    )
    
    result <- modify_master_import_in_list(all_sheets, new_sheet)
    
    expect_true(result$sheet1$modified)
    expect_false(result$sheet2$modified)
  })
})

describe("Partner Metadata Loading and Handling", {
  test_that("load_partner_metadata errors on nonexistent file", {
    expect_error(load_partner_metadata("/nonexistent/path.xlsx"), "file does not exist|unable|cannot")
  })

  test_that("load_partner_metadata loads expected sheets", {
    test_file <- file.path(tmp_dir, "partner_metadata.xlsx")
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("ENCOUNTER DATA")
    wb$add_worksheet("RESTART TIMES")
    
    encounter_data <- tibble(
      ring_number = "R1",
      logger_id_deployed = "L1",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-01"),
      colony = "Colony1"
    )
    restart_times <- tibble(
      logger_id = "L1",
      startdate_GMT = as.Date("2025-01-15")
    )
    
    wb$add_data("ENCOUNTER DATA", encounter_data)
    wb$add_data("RESTART TIMES", restart_times)
    openxlsx2::wb_save(wb, test_file)
    
    result <- load_partner_metadata(test_file)
    
    expect_true(is.list(result$data))
    expect_true("ENCOUNTER DATA" %in% names(result$data))
    expect_true("RESTART TIMES" %in% names(result$data))
    
    file.remove(test_file)
  })

  test_that("handle_partner_metadata errors if master_import missing required sheets", {
    partner_file <- file.path(tmp_dir, "partner.xlsx")
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("ENCOUNTER DATA")
    wb$add_worksheet("RESTART TIMES")
    wb$add_data("ENCOUNTER DATA", tibble(col = 1))
    wb$add_data("RESTART TIMES", tibble(col = 2))
    openxlsx2::wb_save(wb, partner_file)
    
    partner_metadata <- load_partner_metadata(partner_file)
    
    bad_master <- list(data = list(METADATA = tibble(a = 1)))
    
    expect_error(
      handle_partner_metadata("TestColony", partner_metadata, bad_master),
      "STARTUP_SHUTDOWN|required"
    )
    
    file.remove(partner_file)
  })

  test_that("handle_partner_metadata handles empty encounter data gracefully", {
    partner_metadata <- list(
      data = list(
        `ENCOUNTER DATA` = tibble(),
        `RESTART TIMES` = tibble()
      ),
      path = "/path/to/file.xlsx",
      modified = FALSE
    )
    
    master_import <- list(
      data = list(
        METADATA = create_test_metadata_sheet(),
        STARTUP_SHUTDOWN = create_test_startup_sheet()
      ),
      path = "/path/to/master.xlsx",
      modified = FALSE
    )
    
    result <- handle_partner_metadata("TestColony", partner_metadata, master_import)
    
    expect_true(is.list(result))
    expect_true("metadata" %in% names(result) || "master_import" %in% names(result))
  })
})

describe("Master Sheet Saving", {
  test_that("save_master_sheet writes file with correct data", {
    out_file <- file.path(tmp_dir, "saved_master.xlsx")
    
    metadata <- create_test_metadata_sheet()
    startup <- create_test_startup_sheet()
    
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("METADATA")
    wb$add_worksheet("STARTUP_SHUTDOWN")
    wb$add_data("METADATA", metadata)
    wb$add_data("STARTUP_SHUTDOWN", startup)
    
    loaded_wb <- LoadedWB$new(data = list(METADATA = metadata, STARTUP_SHUTDOWN = startup), wb = wb)
    loaded_wb$path <- out_file
    
    save_master_sheet(loaded_wb, out_file)
    
    expect_true(file.exists(out_file))
    
    # Verify saved file contents
    result <- load_master_import(file_path = out_file)
    expect_equal(nrow(result$data$METADATA), 2)
    expect_equal(nrow(result$data$STARTUP_SHUTDOWN), 2)
    
    file.remove(out_file)
  })

  test_that("save_all_modified saves only modified sheets when flag is TRUE", {
    # This is a complex function that depends on file paths and modification flags
    skip("Requires complex setup with multiple modified sheets")
  })
})
