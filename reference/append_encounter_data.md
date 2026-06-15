# Append encounter data to the master import metadata

Append encounter data to the master import metadata

## Usage

``` r
append_encounter_data(master_metadata, encounter_data, version = 2026)
```

## Arguments

- master_metadata:

  A data frame representing the master import metadata.

- encounter_data:

  A data frame representing the encounter data to be appended.

- version:

  Version of processing to use

## Value

A data frame with the encounter data appended to the master metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
updated_master_metadata <- append_encounter_data(master_metadata, encounter_data)
} # }
```
