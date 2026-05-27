# Push activity data to database

This function scans the import directory for activity data files, checks
for corresponding sessions in the database, processes the files, and
uploads the data to the SEATRACK database. It also handles error
checking and logs the progress of the upload. The function is designed
to be flexible and can handle different types of activity data (e.g.,
accelerometer, light, temperature) based on the file extensions and
specified processing functions.

## Usage

``` r
push_db_activity(
  import_directory = file.path(the$sea_track_folder,
    "Database\\Imports_Logger data\\Raw logger data\\ALL"),
  compare_file_to_db = FALSE,
  min_date = "2000-01-01"
)
```

## Arguments

- import_directory:

  The directory to scan for activity data files. Defaults to the "ALL"
  folder in the SeaTrack database imports directory.

- compare_file_to_db:

  A boolean indicating whether to compare the files in the import
  directory to the records in the database to identify files that may
  need to be re-uploaded. As this can be heavy, this defaults to FALSE.

- min_date:

  A string representing the minimum modification date for files to be
  considered for upload, in the format "YYYY-MM-DD". Defaults to
  "2000-01-01". Strongly reccomended when compare_file_to_db is TRUE to
  avoid comparing a large number of files that have not been modified
  recently.

## Value

None. The function uploads the activity data to the SEATRACK database
and logs the progress and any issues encountered during the process.
