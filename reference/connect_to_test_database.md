# Connect to test database

Attempts to establish a connection to the seatrackR test database.
Returns NULL and issues a warning if connection fails (typically in
CI/non-dev environments).

## Usage

``` r
connect_to_test_database(
  username = "postgres",
  password = "postgres",
  host = "127.0.0.1:5432",
  dbname = "seatrack_test"
)
```

## Arguments

- username:

  Username for test database (default: "postgres")

- password:

  Password for test database (default: "postgres")

- host:

  Host and port (default: "127.0.0.1:5432")

- dbname:

  Database name (default: "seatrack_test")

## Value

Database connection object, or NULL if connection fails

## Details

This is a wrapper around
[`seatrackR::connectSeatrack()`](https://ninanor.github.io/seatrackR/reference/connectSeatrack.html)
that gracefully handles connection failures. It's designed for use in
test environments where a test database may not be available.

If the connection fails, a warning is issued and NULL is returned. Tests
that use this function should check for NULL and skip() if the database
is unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
  con <- connect_to_test_database()
  if (is.null(con)) {
    skip("Test database not available")
  }
  # Use connection...
  DBI::dbDisconnect(con)
} # }
```
