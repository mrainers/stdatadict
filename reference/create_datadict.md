# Create A data dictionary

Wrapper function for creating a data dictionary. This function calls:
[`openxlsx2::wb_workbook()`](https://janmarvin.github.io/openxlsx2/reference/wb_workbook.html),
[`add_form_overview()`](add_form_overview.md),
[`add_form_sheets()`](add_form_sheets.md) and
[`link_form_sheets()`](link_form_sheets.md)

## Usage

``` r
create_datadict(
  datadict_tables,
  title = NULL,
  subtitle = NULL,
  as_of_date = NULL,
  form_type_description = TRUE,
  doc_width = "G"
)
```

## Arguments

- datadict_tables:

  a list with tables for the data dictionary, created by
  [`create_datadict_tables()`](create_datadict_tables.md)

- title:

  (character) Title of the document. Default = NULL

- subtitle:

  (character) subtitle of the document, e.g. researcher - study name.
  Default = NULL'

- as_of_date:

  (date) when the data set was created. Default = NULL

- form_type_description:

  (logical), Should short explanation of the different form types added
  to the form overview? Default = TRUE

- doc_width:

  (single character or numeric) Up to which column should the titles and
  texts spread? Default = "G"

## Value

An excel workbook

## Examples

``` r
if (FALSE) create_datadict(datadict_tables) # \dontrun{}
```
