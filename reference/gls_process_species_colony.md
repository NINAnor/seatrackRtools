# Process GLS position data for a specific species/colony combination

Function to process calibrated GLS position data for a specific
species/colony combination using seatrackRgls.

## Usage

``` r
gls_process_species_colony(
  species,
  colony,
  import_directory = file.path(the$sea_track_folder,
    "Database\\Imports_Logger data\\Raw logger data\\ALL"),
  output_directory = file.path(the$sea_track_folder,
    "Database\\Imports_Logger data\\Output_GLSpositions"),
  all_colony_info = gls_seatrack_colony_info(),
  no_pos_only = TRUE,
  filter_plots = TRUE,
  skip_existing_files = TRUE,
  stop_on_error = FALSE
)
```

## Arguments

- species:

  Species name to filter metadata.

- colony:

  Colony name to filter metadata.

- import_directory:

  Path to the directory containing GLS files. Default is
  `Database\\Imports_Logger data\\Raw logger data\\ALL`.

- output_directory:

  Path to the directory where the processed position data will be saved.
  Default is `Database\\Imports_Logger data\\Output_GLSpositions`.

- all_colony_info:

  Dataframe containing colony information for all species/colony
  combinations. Default is obtained from
  [`gls_seatrack_colony_info()`](https://ninanor.github.io/seatrackRtools/reference/gls_seatrack_colony_info.md).

- no_pos_only:

  Logical indicating whether to include only loggers without position
  data in the database. Default is TRUE.

- filter_plots:

  Logical indicating whether to export filter plots. Default is TRUE.

## Value

None. The function saves the processed position data to the specified
output directory.
