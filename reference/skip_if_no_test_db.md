# Skip test if database connection unavailable

Convenience wrapper that calls
[`connect_to_test_database()`](https://ninanor.github.io/seatrackRtools/reference/connect_to_test_database.md)
and skips the current test if connection fails.

## Usage

``` r
skip_if_no_test_db(message = "Test database not available")
```

## Arguments

- message:

  Message to display if test is skipped

## Value

NULL invisibly. If connection succeeds, returns the connection object.

## Examples

``` r
if (FALSE) { # \dontrun{
  test_that("my database test", {
    con <- skip_if_no_test_db("Test database not available")
    # Use connection...
    DBI::dbDisconnect(con)
  })
} # }
```
