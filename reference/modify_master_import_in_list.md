# Given a new master import, modify all master imports in a list sharing the same path

When loading all master import files with distinct = FALSE, we will end
up with duplicate master import files. This is because multiple colonies
can share the same master import file. If we modify one of these (for
example, updating from partner metadata for that colony), we need to
make sure the files stay in sync.

## Usage

``` r
modify_master_import_in_list(all_master_import, new_master_import)
```

## Arguments

- all_master_import:

  A list of master imports, as produced by
  load_all_master_import(combine = FALSE, distinct = FALSE)

- new_master_import:

  A modified master import sheet to be distributed throughout the list

## Value

An updated list of master imports, with the new master import replacing
any with the same path.
