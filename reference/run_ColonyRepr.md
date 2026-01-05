# Function to extract desired data from the SEATRACK database and fully run the colony representativeness analysis

This is a wrapper function around
[`prep_SEATRACK_ColonyRepr()`](https://ninanor.github.io/seatrackRtools/reference/prep_SEATRACK_ColonyRepr.md)
and the analysis pipeline. It will thus extract the specified gls
positional data and plug it into the pipeline that will produce a listed
output with various elements that can be used to indicate captured
colony representativeness. An active connection with the SEATRACK data
base is required.

## Usage

``` r
run_ColonyRepr(
  species,
  colony,
  Month_of_interest = 12,
  KDE_contours_of_interest = seq(from = 25, to = 95),
  smoothing_parameter = NULL,
  N_iterations = 20
)
```

## Arguments

- species:

  Specify species of interest (numeric)

- colony:

  Specify colony of interest (numeric)

- Month_of_interest:

  Specify month of interest (numeric)

- KDE_contours_of_interest:

  Specify KDE contours of interest (can be a vector)

- smoothing_parameter:

  Specify a smoothing parameter to be used in the UD calculation

- N_iterations:

  Specify the number of iterations you'd like to perform when combining
  surface areas (the mean will be used in fitting the MM-curve)

## Value

A list with all components of the colony representativeness analysis
