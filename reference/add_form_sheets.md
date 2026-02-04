# Ceate And Add Form Sheets To The Data Dictionary

Create a workbook sheet for every table in the
datadict_tables\$form_items list.

## Usage

``` r
add_form_sheets(wb, datadict_tables, var_select = FALSE)
```

## Arguments

- wb:

  a workbook

- datadict_tables:

  generated with [`create_datadict_tables()`](create_datadict_tables.md)

- var_select:

  (logical) Is this a variable selection document?

## Value

workbook with form item tables added (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
wb <- wb_workbook()
add_form_overview(wb, datadict_tables = datadict_tables)
add_form_sheets(wb, datadict_tables = datadict_tables)
} # }
```
