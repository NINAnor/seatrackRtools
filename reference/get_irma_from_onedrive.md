# Get IRMA data from OneDrive

This function retrieves IRMA positional data from OneDrive for a
specified species, colony, time range. and age class. The function reads
the IRMA data from RDS files stored in the SEATRACK OneDrive folder,
filters the data based on the provided parameters, and returns a data
frame containing the relevant IRMA positional data.

## Usage

``` r
get_irma_from_onedrive(
  start_year = "2000",
  end_year = format(Sys.Date(), "%Y"),
  species = NULL,
  colony = NULL,
  session_ids = NULL,
  age_deployment = "A",
  release = "20241120",
  version = "v3.1"
)
```

## Arguments

- start_year:

  An integer representing the start year for the data retrieval.

- end_year:

  An integer representing the end year for the data retrieval. Defaults
  to the current year.

- species:

  An optional string specifying the species to filter the data. If NULL,
  data for all species will be retrieved.

- colony:

  An optional string specifying the colony to filter the data. If NULL,
  data for all colonies will be retrieved.

- age_deployment:

  An optional string specifying the age class to filter the data.
  Possible values are "A" for adults and "C" for juveniles. Defaults to
  "A".

- release:

  An optional string specifying the release of the IRMA data to
  retrieve. Defaults to "20241120".

- version:

  An optional string specifying the version of the IRMA data to
  retrieve. Defaults to "v3.1".

## Value

A named list to be appended inside the data_request function.
