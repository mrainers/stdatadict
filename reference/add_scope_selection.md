# Add Scope Selection Table to workbook

Add Scope Selection Table to workbook

## Usage

``` r
add_scope_selection(
  wb,
  scope,
  title = NULL,
  sheet = current_sheet(),
  row = 1,
  doc_width = "G",
  select_all = TRUE
)
```

## Arguments

- wb:

  A Workbook object containing a worksheet.

- scope:

  "Scope Table", including at least a scopeID column named "Scope".

- title:

  (character) A title for the Scope selection table. Default = NULL If
  title = NULL, no title will be created.

- sheet:

  The worksheet to write to. Can be the worksheet index or name.

- row:

  number where to start the current Scope Selection, default = 1

- doc_width:

  (single character or numeric) if a title is given, up to which column
  should the section title spread? Default = "G"

- select_all:

  (logical) Should a "select all?" Question added to the scope selection
  table? Default = TRUE ("Select all" question is added only if the
  table contains more than one scope)

## Value

wbWorkbook, invisibly

## Examples

``` r
if (FALSE) { # \dontrun{
add_scope_selection(
  wb,
  scope = datapkg_scopes,
  title = "Data Packages",
  row = 5,
  doc_width = "I"
)
} # }
```
