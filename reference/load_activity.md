# Load activity data from file

This function loads activity data from a specified file based on the
recording type. It uses the appropriate processing function for the
recording type to read the data, clips the data to the deployment and
retrieval dates, and returns the clipped activity data as a dataframe.
If there is an error in processing the file or if there is no data
within the deployment and retrieval dates, it logs a warning and returns
NULL.

## Usage

``` r
load_activity(file_info, recording_type)
```

## Arguments

- file_info:

  A dataframe containing information about the file to be processed,
  including session_id, filename, individ_id, deployment_date,
  retrieval_date, full_path, and extension.

- recording_type:

  A list containing information about the recording type, including
  table_name, process_function, extensions, and argname.

## Value

A dataframe containing the clipped activity data for the specified file
and recording type, or NULL if there was an error in processing the file
or if there is no data within the deployment and retrieval dates.
