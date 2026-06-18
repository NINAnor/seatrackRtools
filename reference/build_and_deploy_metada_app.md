# Get and prepare data for the SEATRACK metadata Shiny app.

Convenience function calling `get_web_metadata_data` to get the latest
data from the database and then pushing it to shinyapps.io. Note that
the default is to upload to the test app. Set test = FALSE to update the
production app.

## Usage

``` r
build_and_deploy_metada_app(
  test = TRUE,
  account = NULL,
  token = NULL,
  secret = NULL
)
```

## Arguments

- test:

  A boolean indicating whether to deploy the app in test mode. Defaults
  to TRUE, which deploys the app under the name
  "seatrack_shinyapp_TEST". If FALSE, the app will be deployed under the
  name "seatrack_shinyapp".

- account:

  A string specifying the shinyapps.io account name. If NULL, it will
  attempt to retrieve the account name from the SHINYAPPS_ACCOUNT
  environment variable or prompt the user for input.

- token:

  A string specifying the shinyapps.io token. If NULL, it will attempt
  to retrieve the token from the SHINYAPPS_TOKEN environment variable or
  prompt the user for input.

- secret:

  A string specifying the shinyapps.io secret. If NULL, it will attempt
  to retrieve the secret from the SHINYAPPS_SECRET environment variable
  or prompt the user for input.

## Value

None. This function processes data, saves it as CSV files for use in the
Shiny app, and deploys the app to shinyapps.io, but does not return a
value.
