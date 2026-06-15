# Parse data request PDFs and compile into a summary CSV This function reads all PDF files in a specified folder, extracts relevant information from each PDF, and compiles it into a summary. The summary can then be exported as a CSV file.

Parse data request PDFs and compile into a summary CSV This function
reads all PDF files in a specified folder, extracts relevant information
from each PDF, and compiles it into a summary. The summary can then be
exported as a CSV file.

## Usage

``` r
parse_data_request_folder(
  request_folder = file.path(the$sea_track_folder,
    "Admin\\08_Data requests and AoU\\Data applications_received\\"),
  first_id = NA,
  export = TRUE
)
```

## Arguments

- request_folder:

  The folder containing the PDF data request files. Default is set to a
  specific path within the SEATRACK folder.

- first_id:

  An optional parameter to specify the first ID to include in the
  summary. If NA, all entries will be included.

- export:

  A boolean indicating whether to export the summary as a CSV file.
  Default is TRUE.

## Value

A data frame containing the parsed information from the PDF files.
