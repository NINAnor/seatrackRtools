# Process GLS position data using seatrackRgls

Function to process calibrated GLS position data using seatrackRgls.

## Usage

``` r
gls_process_positions(
  no_pos_only = TRUE,
  filter_plots = TRUE,
  skip_existing_files = TRUE,
  n_workers = 4
)
```

## Arguments

- no_pos_only:

  Logical indicating whether to include only loggers without position
  data in the database. Default is TRUE.

- filter_plots:

  Logical indicating whether to export filter plots. Default is FALSE.

- skip_existing_files:

  Logical indicating whether to skip processing for files that already
  have processed position data in the output directory. Default is TRUE.

- n_workers:

  Integer specifying the number of worker processes to use for parallel
  processing.

## Value

None. The function saves the processed position data to
`Database\\Imports_Logger data\\Output_GLSpositions`
