# Nonresponsive loggers

First, set the sea trach folders

``` r
seatrack_path <- "path/to/Norsk Polarinstitutt/Benjamin Merkel - SEATRACK - shared"
set_sea_track_folder(seatrack_path)
```

Set up file paths for nonresponsive logger sheets and load the sheets.

``` r
manufacturers <- c("PathTrack", "Lotek", "Migrate Technology")
non_responsive_file_paths <- sapply(manufacturers, function(x) {
  file.path(seatrack_path, "Nonresponsive loggers and restored data", paste0(x, "_nonresponsive_2025.xlsx"))
})
nonresponsive_sheets <- load_nonresponsive(non_responsive_file_paths, manufacturers)
```

Load all master import sheets

``` r
all_metadata_combined <- load_all_master_import(TRUE)
```

Either generate new or append to nonresponsive sheets.

``` r
new_unresponsive_sheets <- nonresponsive_from_master(all_metadata_combined, nonresponsive_sheets)
```

Save the new/modified nonresponsive sheets

``` r
save_nonresponsive(non_responsive_file_paths, new_unresponsive_sheets)
```
