# SessionBatch Class

A convenience class for storing a set of logger sessions along with the
type of database import they require.

## See also

Other classes:
[`DBImportCollection`](https://ninanor.github.io/seatrackRtools/reference/DBImportCollection.md),
[`LoadedWB`](https://ninanor.github.io/seatrackRtools/reference/LoadedWB.md),
[`LoadedWBCollection`](https://ninanor.github.io/seatrackRtools/reference/LoadedWBCollection.md)

## Public fields

- `sessions`:

  Tibble containing session information from master import
  startup_shutdown.

- `type`:

  The type of import that needs to occur. Must be either "close_only",
  "open_only" or "open_and_close".

## Methods

### Public methods

- [`SessionBatch$new()`](#method-SessionBatch-new)

- [`SessionBatch$print()`](#method-SessionBatch-print)

- [`SessionBatch$clone()`](#method-SessionBatch-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new SessionBatch object

#### Usage

    SessionBatch$new(
      sessions = tibble(),
      type = c("close_only", "open_only", "open_and_close")
    )

#### Arguments

- `sessions`:

  A tibble containing session information from master import
  startup_shutdown.

- `type`:

  The type of import that needs to occur. Must be either "close_only",
  "open_only" or "open_and_close".

#### Returns

A new SessionBatch object

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for SessionBatch

#### Usage

    SessionBatch$print()

#### Returns

The SessionBatch object invisibly

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SessionBatch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
