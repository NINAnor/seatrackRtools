# Handle activity data file

This function processes a single activity data file based on the
specified recording type. It reads the file, clips the data to the
deployment and retrieval dates, and uploads the data to the SEATRACK
database. It also handles error checking and logs the progress of the
upload.

## Usage

``` r
handle_activity(file_info, recording_type, remove_existing = FALSE)
```

## Arguments

- file_info:

  A dataframe containing information about the file to be processed,
  including session_id, filename, individ_id, deployment_date,
  retrieval_date, full_path, and extension.

- recording_type:

  A list containing information about the recording type, including
  table_name, process_function, extensions, and argname.

- remove_existing:

  A boolean indicating whether to remove existing data for the
  session_id from the database before uploading the new data. This can
  be useful if you want to replace existing data with updated data.
  Defaults to FALSE.

## Value

None. The function uploads the processed activity data to the SEATRACK
database and logs the progress and any issues encountered during the
process.

## Details

The function performs the following steps:

1.  Reads the activity data file using the specified processing function
    for the recording type

2.  Clips the data to the deployment and retrieval dates specified in
    the file_info

3.  Uploads the clipped data to the SEATRACK database using the
    appropriate table based on the recording type

4.  Logs the progress of the upload and any issues encountered during
    the process

5.  If the upload is successful, it also uploads the raw file to the
    archive
