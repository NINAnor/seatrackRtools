# Extracts data from the SEATRACK DB and prepares it for the ColonyRepr functions

This function will extract gls positional data for the specified
species-colony. An active connection with the data base is required for
this. It will then prepare the extracted data to be used in the
ColonyRepr functions

## Usage

``` r
prep_SEATRACK_ColonyRepr(species, colony)
```

## Arguments

- species:

  specify the species you'd like to extract the data from

- colony:

  specify the colony you'd like to select the data from

## Value

position data from the specified species-colony, ready to be inserted
into ColonyRepr functions
