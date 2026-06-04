# Test script for functions in R/encounters.R

library(testthat)
library(tibble)

describe("Encounter Data Processing", {
  test_that("append_encounter_data appends non-duplicate rows", {
    master <- tibble(
      ring_number = NA,
      logger_id_deployed = c("A", "B"),
      logger_id_retrieved = NA,
      date = as.Date(c("2025-01-01", "2025-01-02")),
      nest_latitude = NA,
      nest_longitude = NA
    )
    encounter <- tibble(
      ring_number = NA,
      logger_id_deployed = c("C", "B"),
      logger_id_retrieved = NA,
      date = as.Date(c("2025-01-03", "2025-01-02")),
      nest_latitude = NA,
      nest_longitude = NA
    )
    result <- append_encounter_data(master, encounter)
    expect_equal(nrow(result), 3)
    expect_true(all(c("A", "B", "C") %in% result$logger_id_deployed))
  })

  test_that("append_encounter_data returns master when encounter data is empty", {
    master <- tibble(
      ring_number = NA,
      logger_id_deployed = "A",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-01"),
      nest_latitude = NA,
      nest_longitude = NA
    )
    encounter <- master[0, ]
    result <- append_encounter_data(master, encounter)
    expect_equal(nrow(result), 1)
    expect_equal(result, master)
  })

  test_that("append_encounter_data removes rows with NA dates", {
    master <- tibble(
      ring_number = NA,
      logger_id_deployed = "A",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-01"),
      nest_latitude = NA,
      nest_longitude = NA
    )
    encounter <- tibble(
      ring_number = NA,
      logger_id_deployed = c("B", "C"),
      logger_id_retrieved = NA,
      date = c(as.Date("2025-01-02"), NA),
      nest_latitude = NA,
      nest_longitude = NA
    )
    result <- append_encounter_data(master, encounter)
    # Master (1) + encounter row with date (1) = 2
    expect_equal(nrow(result), 2)
    expect_true(all(!is.na(result$date)))
  })

  test_that("append_encounter_data errors on invalid date format", {
    master <- tibble(
      ring_number = NA,
      logger_id_deployed = "A",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-01"),
      nest_latitude = NA,
      nest_longitude = NA
    )
    encounter <- tibble(
      ring_number = NA,
      logger_id_deployed = "B",
      logger_id_retrieved = NA,
      date = as.Date("0500-01-01"),
      nest_latitude = NA,
      nest_longitude = NA
    )
    expect_error(append_encounter_data(master, encounter), "Date format was loaded incorrectly")
  })

  test_that("append_encounter_data errors on column mismatch (missing columns in encounter)", {
    master <- tibble(
      ring_number = NA,
      logger_id_deployed = "A",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-01"),
      nest_latitude = NA,
      nest_longitude = NA,
      extra_col = NA
    )
    encounter <- tibble(
      ring_number = NA,
      logger_id_deployed = "B",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-02"),
      nest_latitude = NA,
      nest_longitude = NA
    )
    expect_error(append_encounter_data(master, encounter), "following columns are in master_metadata but not in encounter_data")
  })

  test_that("append_encounter_data errors on extra columns in encounter", {
    master <- tibble(
      ring_number = NA,
      logger_id_deployed = "A",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-01"),
      nest_latitude = NA,
      nest_longitude = NA
    )
    encounter <- tibble(
      ring_number = NA,
      logger_id_deployed = "B",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-02"),
      nest_latitude = NA,
      nest_longitude = NA,
      extra_col = NA
    )
    expect_error(append_encounter_data(master, encounter), "following columns are in encounter_data but not in master_metadata")
  })

  test_that("append_encounter_data adds missing nest_latitude/nest_longitude columns to master", {
    master <- tibble(
      ring_number = NA,
      logger_id_deployed = "A",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-01")
    )
    encounter <- tibble(
      ring_number = NA,
      logger_id_deployed = "B",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-02"),
      nest_latitude = NA,
      nest_longitude = NA
    )
    result <- append_encounter_data(master, encounter)
    expect_true("nest_latitude" %in% colnames(result))
    expect_true("nest_longitude" %in% colnames(result))
  })

  test_that("append_encounter_data preserves column order from master", {
    master <- tibble(
      a = 1,
      b = 2,
      c = 3,
      d = NA,
      e = NA
    )
    encounter <- tibble(
      a = 4,
      b = 5,
      c = 6,
      d = NA,
      e = NA
    )
    result <- append_encounter_data(master, encounter)
    expect_equal(colnames(result), c("a", "b", "c", "d", "e"))
  })

  test_that("append_encounter_data detects duplicate rows and excludes them", {
    master <- tibble(
      ring_number = "R1",
      logger_id_retrieved = "L1",
      logger_id_deployed = "L2",
      date = as.Date("2025-01-01"),
      nest_latitude = NA,
      nest_longitude = NA
    )
    encounter <- tibble(
      ring_number = c("R1", "R2"),
      logger_id_retrieved = c("L1", "L3"),
      logger_id_deployed = c("L2", "L4"),
      date = as.Date(c("2025-01-01", "2025-01-02")),
      nest_latitude = NA,
      nest_longitude = NA
    )
    result <- append_encounter_data(master, encounter)
    # Master has 1 row, encounter's first row is duplicate (skipped), second row is new
    expect_equal(nrow(result), 2)
    expect_true(all(!is.na(result$ring_number)))
  })

  test_that("append_encounter_data renames 'other relevant variables' column if present", {
    master <- tibble(
      ring_number = NA,
      logger_id_deployed = "A",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-01"),
      nest_latitude = NA,
      nest_longitude = NA,
      other = NA
    )
    encounter <- tibble(
      ring_number = NA,
      logger_id_deployed = "B",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-02"),
      nest_latitude = NA,
      nest_longitude = NA,
      `other relevant variables` = "test_value"
    )
    result <- append_encounter_data(master, encounter)
    expect_true("other" %in% colnames(result))
    expect_equal(result$other[2], "test_value")
  })

  test_that("append_encounter_data handles mixed NA and non-NA logger IDs correctly", {
    master <- tibble(
      ring_number = NA,
      logger_id_retrieved = c(NA, "L1"),
      logger_id_deployed = c("L2", NA),
      date = as.Date(c("2025-01-01", "2025-01-02")),
      nest_latitude = NA,
      nest_longitude = NA
    )
    encounter <- tibble(
      ring_number = NA,
      logger_id_retrieved = NA,
      logger_id_deployed = "L3",
      date = as.Date("2025-01-03"),
      nest_latitude = NA,
      nest_longitude = NA
    )
    result <- append_encounter_data(master, encounter)
    expect_equal(nrow(result), 3)
    expect_true(nrow(result[!is.na(result$logger_id_deployed), ]) >= 2)
  })
})
