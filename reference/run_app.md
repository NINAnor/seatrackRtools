# Run the Shiny App

This function launches the Shiny application for seatrackRtools. It sets
up the necessary options for the app, including paths for settings and
logs, and then runs the app from the specified directory within the
package.

## Usage

``` r
run_app(
  settings_path = file.path(getwd(), "seatrackRtools_app"),
  log_path = file.path(getwd(), "seatrackRtools_app", "logs"),
  test = FALSE
)
```

## Arguments

- settings_path:

  A string specifying the path to the settings directory for the app.
  Defaults to a "seatrackRtools_app" directory in the current working
  directory.

- log_path:

  A string specifying the path to the logs directory for the app.
  Defaults to a "logs" directory within the "seatrackRtools_app"
  directory in the current working directory.

- test:

  A boolean indicating whether to run the app in test mode. Defaults to
  FALSE. If TRUE, the app will use the test database.

## Value

None. This function launches the Shiny app and does not return a value.
