# Appendix table

First, load the field planning excel file

``` r
field_plan_path <- "fieldplanning2025_fieldsuksess.xlsx"
field_plan_sheet <- get_field_plan(field_plan_path)
```

Check locations against the database, appending a lattitude and
longitude. For locations not yet present in the database, a simple
dataframe of name and lattitude can be provided.

``` r
new_locations <- data.frame(colony = c("Nord-Troms", "Ågotnes", "Oslo", "Bicquette Island", "Pommes Island"), lat <- c(70, 60.40, 59.91, 48.41, 48.11))

seatrackR::connectSeatrack()

field_plan_sheet <- field_plan_check_locations(field_plan_sheet, new_locations)
```

Clean the field plan sheet, generating deployment and retrieval
successes.

``` r
field_plan_clean <- get_clean_field_plan(field_plan_sheet)
```

Export an appendix table for target logger type, age class and species.

``` r
target_species <- c(
    "Atlantic puffin", "Brünnich's guillemot", "Common guillemot",
    "Little auk", "Razorbill", "Arctic tern", "Black-legged kittiwake", "Great skua",
    "Leach's Storm Petrel", "Northern fulmar", "Northern gannet", "Glaucous gull", "Herring gull",
    "Lesser black-backed gull", "European shag", "Common eider"
)

appendix_table <- create_appendix_table(field_plan_clean, "GLS", "A", target_species)

export_appendix_table_excel(appendix_table, "table_output.xlsx")
```
