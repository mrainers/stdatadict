# Create And Add A Form Overview To The Data Dictionary

Create And Add A Form Overview To The Data Dictionary

## Usage

``` r
add_form_overview(
  wb,
  datadict_tables,
  title = NULL,
  subtitle = NULL,
  as_of_date = NULL,
  form_type_description = TRUE,
  doc_width = "G"
)
```

## Arguments

- wb:

  a workbook

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

invisible, the workbook with a form overview sheet added to it.
