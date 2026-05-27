# Load immersion data from file

This function loads immersion data from a specified file, processes it
according to the file type (Lotek or Migrate), and returns a cleaned
data frame with date-time and conductivity information. The function
includes error checking for data quality and consistency.

## Usage

``` r
load_immersion_data(file_info)
```

## Arguments

- file_info:

  A dataframe containing information about the file to be processed,
  including session_id, filename, individ_id, deployment_date,
  retrieval_date, full_path, and extension.

## Value

A cleaned data frame with date-time, conductivity, and standardized
conductivity information, or NULL if the file fails quality checks.
