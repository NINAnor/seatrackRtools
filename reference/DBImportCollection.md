# DBImportCollection Class

A convenience class for storing logger sessions alongside the
deployments and retrievals that are associated with them.

## See also

Other classes:
[`LoadedWB`](https://ninanor.github.io/seatrack-metadata/reference/LoadedWB.md),
[`LoadedWBCollection`](https://ninanor.github.io/seatrack-metadata/reference/LoadedWBCollection.md),
[`SessionBatch`](https://ninanor.github.io/seatrack-metadata/reference/SessionBatch.md)

## Public fields

- `sessions`:

  SessionBatch containing session information from master import
  startup_shutdown.

- `retrievals`:

  Tibble containing retrievals events information from master import
  metadata

- `deployments`:

  Tibble containing retrievals events information from master import
  metadata

## Active bindings

- `type`:

  The type of import that needs to occur.

## Methods

### Public methods

- [`DBImportCollection$new()`](#method-DBImportCollection-new)

- [`DBImportCollection$print()`](#method-DBImportCollection-print)

- [`DBImportCollection$clone()`](#method-DBImportCollection-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new DBImportCollection object

#### Usage

    DBImportCollection$new(
      sessions = SessionBatch$new(),
      retrievals = tibble(),
      deployments = tibble()
    )

#### Arguments

- `sessions`:

  A SessionBatch containing session information from master import
  startup_shutdown.

- `retrievals`:

  A tibble containing retrievals events information from master import
  metadata

- `deployments`:

  A tibble containing deployments events information from master import
  metadata

#### Returns

A new DBImportCollection object

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for DBImportCollection

#### Usage

    DBImportCollection$print()

#### Returns

The DBImportCollection object invisibly

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DBImportCollection$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
