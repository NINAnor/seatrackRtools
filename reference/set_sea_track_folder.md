# Set the base directory for the sea track folder

This function sets a global variable used by other functions. It also
sets system locale to allow the handling of Norwegian characters in
filenames.

## Usage

``` r
set_sea_track_folder(dir, language = "English_United Kingdom")
```

## Arguments

- dir:

  A character string specifying the path to the base directory.

- language:

  Character string specifying system language to add utf8 encoding to.

## Value

None

## Examples

``` r
if (FALSE) { # \dontrun{
set_sea_track_folder("/path/to/sea/track/folder")
} # }
```
