# Deploy the SEATRACK metadata Shiny app to shinyapps.io.

This function deploys the SEATRACK metadata Shiny app to the
shinyapps.io platform. It sets up the necessary credentials for
deployment, checks for the presence of required data, and then uses the
rsconnect package to deploy the app under a specified account and app
name.

## Usage

``` r
deploy_web_metadata_app(
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

None. This function deploys the Shiny app to shinyapps.io and does not
return a value.
