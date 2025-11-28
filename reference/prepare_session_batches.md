# Prepare session batches for database import

This function prepares a list of session batches for database import by
processing each batch and extracting relevant metadata. It filters out
any batches that do not contain sessions and returns a list of prepared
session batches. It adds deployments and retrievals associated with the
sessions in each batch. In cases where sessions have multiple
deployments or retrievals, those sessions are removed from the batch and
a warning is logged.

## Usage

``` r
prepare_session_batches(session_batches, metadata)
```

## Arguments

- session_batches:

  A vector of SessionBatch objects to be prepared for database import.

- metadata:

  A dataframe containing metadata information for the sessions.

## Value

A vector of DBImportCollection objects representing the prepared session
batches for database import.
