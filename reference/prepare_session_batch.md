# Prepare session batch for database import

This function prepares a session batch for database import by processing
each batch and extracting relevant metadata. It adds deployments and
retrievals associated with the sessions in each batch. In cases where
sessions have multiple deployments or retrievals, those sessions are
removed from the batch and a warning is logged.

## Usage

``` r
prepare_session_batch(session_batch, metadata)
```

## Arguments

- session_batch:

  A SessionBatch object to be prepared for database import.

- metadata:

  A dataframe containing metadata information for the sessions.

## Value

DBImportCollection object representing the prepared session batches for
database import.
