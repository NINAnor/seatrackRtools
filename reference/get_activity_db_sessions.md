# Get activity database sessions

This function scans the import directory for activity data files, checks
for corresponding sessions in the database, and identifies any missing
files or sessions. It returns a list containing information about the
available files, missing sessions, and missing raw data files for each
recording type.

## Usage

``` r
get_activity_db_sessions(
  recording_types,
  import_directory = file.path(the$sea_track_folder,
    "Database\\Imports_Logger data\\Raw logger data\\ALL"),
  compare_file_to_db = FALSE,
  min_date = "2000-01-01"
)
```

## Arguments

- recording_types:

  A list of recording types, where each recording type is a list
  containing the table name, processing function, file extensions, and
  argument name for the recording type.

- import_directory:

  The directory to scan for activity data files. Defaults to the "ALL"
  folder in the SeaTrack database imports directory.

- compare_file_to_db:

  A boolean indicating whether to compare the files in the import
  directory to the records in the database to identify files that may
  need to be re-uploaded. As this can be heavy, this defaults to FALSE.

- min_date:

  A string representing the minimum modification date for files to be
  considered, in the format "YYYY-MM-DD". Defaults to "2000-01-01".
  Strongly reccomended when compare_file_to_db is TRUE to avoid
  comparing a large number of files that have not been modified
  recently.

## Value

A list containing the following elements:

- file_info: A dataframe containing information about the available
  files in the import directory, including full path, logger ID, year
  downloaded, model, ID year, extension, and filename.

- missing_sessions: A tibble summarizing the missing sessions in the
  database based on the available files in the import directory,
  including logger ID, year downloaded, and model.

- missing_raw_data_files: A list of dataframes for each recording type,
  containing information about the missing raw data files for each
  session, including session ID, individ ID, filename, deployment date,
  and retrieval date.

- updated_raw_data_files: A list of dataframes for each recording type,
  containing information about the files that have been modified and may
  need to be re-uploaded, including session ID, individ ID, filename,
  deployment date, and retrieval date.

- missing_files: A list of dataframes for each recording type,
  containing information about the files that are expected to be in the
  database but are missing from both the database and the import
  directory, including session ID and filename.
