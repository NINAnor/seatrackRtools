# LoadedMetadata Class

A convenience class for handling metadata loaded from workbooks.

## See also

Other classes:
[`DBImportCollection`](https://ninanor.github.io/seatrackRtools/reference/DBImportCollection.md),
[`LoadedWBCollection`](https://ninanor.github.io/seatrackRtools/reference/LoadedWBCollection.md),
[`SessionBatch`](https://ninanor.github.io/seatrackRtools/reference/SessionBatch.md)

## Public fields

- `data`:

  A list of tibbles

- `wb`:

  A workbook object

- `modified`:

  Has the workbook been modified since import?

## Active bindings

- `path`:

  The path of the workbook

## Methods

### Public methods

- [`LoadedWB$new()`](#method-LoadedWB-new)

- [`LoadedWB$print()`](#method-LoadedWB-print)

- [`LoadedWB$clone()`](#method-LoadedWB-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new LoadedWB object

#### Usage

    LoadedWB$new(data = list(), wb = openxlsx2::wb_workbook())

#### Arguments

- `data`:

  A list of tibbles

- `wb`:

  A workbook object

#### Returns

A new LoadedWB object

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for LoadedWB

#### Usage

    LoadedWB$print()

#### Returns

The LoadedWB object invisibly

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LoadedWB$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
