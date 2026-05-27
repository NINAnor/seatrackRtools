# Create SEATRACK documentation README

This function generates a README file for a SEATRACK data request
package using R Markdown. The README includes metadata about the data
request, such as species, colonies, time span, and additional notes.

## Usage

``` r
create_readme(
  request_name,
  file_list,
  species,
  colonies,
  times,
  data_responsible,
  data_dir,
  additional_notes = "",
  additional_files = list(),
  output_file = "README.html"
)
```

## Arguments

- request_name:

  A string representing the name of the data request.

- file_list:

  A character vector of file names included in the data request.

- species:

  A character vector of species included in the data request.

- colonies:

  A character vector of colonies included in the data request.

- times:

  A vector of two dates representing the start and end of the data
  request

- data_responsible:

  Table of people responsible for the data

- data_dir:

  A string representing the directory where the data files are located.

- additional_notes:

  An optional string containing additional notes to be included in the
  README file

- additional_files:

  An optional list of additional files to include in the README file.
  Each element of the list should contain the file path to the file to
  be included and a description.

- output_file:

  A string representing the name and path under which the README file
  will be saved.

## Value

None. The function creates a README file in the specified data
directory.
