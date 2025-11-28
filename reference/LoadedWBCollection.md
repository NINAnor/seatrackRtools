# LoadedWBCollection Class

A convenience class for handling collections of LoadedWB objects.

## See also

Other classes:
[`DBImportCollection`](https://ninanor.github.io/seatrack-metadata/reference/DBImportCollection.md),
[`LoadedWB`](https://ninanor.github.io/seatrack-metadata/reference/LoadedWB.md),
[`SessionBatch`](https://ninanor.github.io/seatrack-metadata/reference/SessionBatch.md)

## Public fields

- `sheets_list`:

  A list of LoadedWB objects

## Active bindings

- `all_paths`:

  The paths of all workbooks in the collection

- `all_names`:

  The names of all workbooks in the collection

- `modified`:

  sheets_list

## Methods

### Public methods

- [`LoadedWBCollection$new()`](#method-LoadedWBCollection-new)

- [`LoadedWBCollection$names()`](#method-LoadedWBCollection-names)

- [`LoadedWBCollection$print()`](#method-LoadedWBCollection-print)

- [`LoadedWBCollection$clone()`](#method-LoadedWBCollection-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new LoadedWBCollection object

#### Usage

    LoadedWBCollection$new(sheets_list = list())

#### Arguments

- `sheets_list`:

  A list of LoadedWB objects

#### Returns

A new LoadedWBCollection object

------------------------------------------------------------------------

### Method [`names()`](https://rdrr.io/r/base/names.html)

Get the names of the sheets in the collection

#### Usage

    LoadedWBCollection$names()

#### Returns

A character vector of sheet names

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for LoadedWBCollection

#### Usage

    LoadedWBCollection$print()

#### Returns

The LoadedWBCollection object invisibly

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LoadedWBCollection$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
