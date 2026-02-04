# Add Document Title

Add Document Title

## Usage

``` r
add_title(wb, x, sheet = current_sheet(), row = 1, doc_width = "G")
```

## Arguments

- wb:

  A Workbook object containing a worksheet.

- x:

  (character) The document title

- sheet:

  The worksheet to write to. Can be the worksheet index or name.

- row:

  (integer) in which row the title should be added. Default = 1

- doc_width:

  (single character or numeric) Up to which column should the texts
  spread? Default = "G"

## Value

A wbWorkbook, invisibly

## Examples

``` r
wb <- openxlsx2::wb_workbook()
style_datadict(wb)
#> Warning: Could not find style(s): title
#> Warning: Could not find style(s): title
#> Warning: Could not find style(s): title
#> Warning: Could not find style(s): subtitle
#> Warning: Could not find style(s): subtitle
#> Warning: Could not find style(s): subtitle
#> Warning: Could not find style(s): as_of_date
#> Warning: Could not find style(s): as_of_date
#> Warning: Could not find style(s): as_of_date
#> Warning: Could not find style(s): heading_1
#> Warning: Could not find style(s): heading_1
#> Warning: Could not find style(s): heading_1
#> Warning: Could not find style(s): heading_2
#> Warning: Could not find style(s): heading_2
#> Warning: Could not find style(s): heading_2
#> Warning: Could not find style(s): text_area
#> Warning: Could not find style(s): text_area
#> Warning: Could not find style(s): text_area
#> Warning: Could not find style(s): section
#> Warning: Could not find style(s): section
#> Warning: Could not find style(s): section
#> Warning: Could not find style(s): table_head
#> Warning: Could not find style(s): table_head
#> Warning: Could not find style(s): table_head
#> Warning: Could not find style(s): visit_names
#> Warning: Could not find style(s): visit_names
#> Warning: Could not find style(s): visit_names
#> Warning: Could not find style(s): table_names
#> Warning: Could not find style(s): table_names
#> Warning: Could not find style(s): table_names
#> Warning: Could not find style(s): form_sheet_head
#> Warning: Could not find style(s): form_sheet_head
#> Warning: Could not find style(s): form_sheet_head
#> Warning: Could not find style(s): form_sheet_tables
#> Warning: Could not find style(s): form_sheet_tables
#> Warning: Could not find style(s): form_sheet_tables
#> Warning: Could not find style(s): form_sheet_table_col
#> Warning: Could not find style(s): form_sheet_table_col
#> Warning: Could not find style(s): form_sheet_table_col
#> Warning: Could not find style(s): select_column_head
#> Warning: Could not find style(s): select_column_head
#> Warning: Could not find style(s): select_column_head
#> Warning: Could not find style(s): select_column
#> Warning: Could not find style(s): select_column
#> Warning: Could not find style(s): select_column
#> Warning: Could not find style(s): select_all_q
#> Warning: Could not find style(s): select_all_q
#> Warning: Could not find style(s): select_all_q
#> Warning: Could not find style(s): select_all_a
#> Warning: Could not find style(s): select_all_a
#> Warning: Could not find style(s): select_all_a
wb$add_worksheet()
add_title(wb, "My Workbook Title")
#> Warning: Could not find style(s): title
#> Error in if (is_dims(style)) {    styid <- self$get_cell_style(dims = style, sheet = sheet)} else {    styid <- self$styles_mgr$get_xf_id(style)}: argument is of length zero
```
