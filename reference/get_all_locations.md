# Get All Locations

Retrieves a list of all locations (colonies) organized by country from
the Sea Track folder.

## Usage

``` r
get_all_locations()
```

## Value

A named list where each element is a vector of colony names for a
country.

## Details

The function expects the global variable `the$sea_track_folder` to be
set, and looks for a "Locations" subfolder within it. Each country is
represented as a subdirectory within "Locations", and each colony is a
subdirectory within its respective country folder. If
`the$sea_track_folder` is not set, the function will stop with an error
message.

## Examples

``` r
if (FALSE) { # \dontrun{
set_sea_track_folder("/path/to/sea_track")
colonies <- get_all_locations()
print(colonies)
} # }
```
