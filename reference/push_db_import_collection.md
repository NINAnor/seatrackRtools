# Push database import collection to database

Function to push a DBImportCollection object to the database. The
function handles the import of startups, deployments, retrievals, and
shutdowns associated with the sessions in the collection. It checks the
database for existing entries and only imports new entries to avoid
duplicates. It performs checks to make sure sessions are started before
a logger is deployed, deployed before it is retrieved and (if it has a
deployment) is retrieved before being shutdown. A database error leads
to the whole function stopping and no partial imports of the table the
database fails on.

## Usage

``` r
push_db_import_collection(db_import_collection)
```

## Arguments

- db_import_collection:

  A DBImportCollection object containing session, deployment, and
  retrieval information.
