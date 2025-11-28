# Load specified sheets from an Excel file into a list of data frames

This function reads specified sheets from an Excel file and returns them
as a list of data frames. It provides options to skip rows, force date
columns to be of Date type, and drop unnamed columns.

## Usage

``` r
load_sheets_as_list(
  file_path,
  sheets,
  skip_rows = 0,
  force_date = TRUE,
  drop_unnamed = TRUE,
  col_types = rep(list(NULL), length(sheets)),
  col_upper = rep(list(NULL), length(sheets))
)
```

## Arguments

- file_path:

  A character string specifying the path to the Excel file.

- sheets:

  A character vector specifying the names of the sheets to be read.

- skip_rows:

  An integer specifying the number of rows to skip at the beginning of
  each sheet. Default is 0.

- force_date:

  A logical indicating whether to attempt to convert date columns to
  Date type. Default is TRUE.

- drop_unnamed:

  A logical indicating whether to drop unnamed columns (columns with no
  header). Default is TRUE.

- col_types:

  A list the same length as sheets, containing either NULL or a numeric
  vector of classes as in
  [`openxlsx2::read_xlsx`](https://janmarvin.github.io/openxlsx2/reference/wb_to_df.html).

- col_upper:

  A list the same length as sheets, containing either NULL or a
  character vector of column names to be forced into uppercase.

## Value

A `LoadedMetadata` object, with a list of tibbles corresponding to each
sheet in `data` and the original workbook in \`wb“

## Examples

``` r
if (FALSE) { # \dontrun{
sheets_data <- load_sheets_as_list("path/to/file.xlsx", c("Sheet1", "Sheet2"), skip = 1)
} # }
```
