# Generate field plan email templates

This function generates email templates for field plan communication
based on the provided field plan data. It creates personalized emails
for each responsible person, detailing the planned allocations of
loggers and equipment for different species and colonies. The emails
also request confirmation of receipt and any necessary data or metadata
from the previous year.

## Usage

``` r
generate_fieldplan_templates(
  field_plan_path = file.path(the$sea_track_folder, "Admin/Fieldplanning"),
  field_year = c(as.numeric(format(Sys.Date(), "%Y")) + 1),
  template_dir = file.path(the$sea_track_folder, "Admin", "Fieldplanning",
    paste0("Fieldplanning", field_year), "templates")
)
```

## Arguments

- field_plan_path:

  Path to the field plan Excel file

- field_year:

  Year to generate email templates for, default is next year

- template_dir:

  Directory to save the generated email templates. Default is
  "Admin/Fieldplanning/Fieldplanning\<field_year\>/templates" in the sea
  track folder.
