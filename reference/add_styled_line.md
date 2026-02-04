# Add Styled Text To The Workbook

Add some Text to a given row, merge the cells of that row and apply a
predefined style.

## Usage

``` r
add_styled_line(
  wb,
  x,
  style,
  sheet = current_sheet(),
  row = 1,
  start_col = "A",
  doc_width = "G"
)
```

## Arguments

- wb:

  A Workbook object containing a worksheet.

- x:

  The text to add to the workbook

- style:

  (character) name of the cell style defined to the workbook. If your
  worbook is named `wb` enter `wb$styles_mgr$xf$name` to get a list of
  style names registered to the workbook

- sheet:

  The worksheet to write to. Can be the worksheet index or name.

- row:

  (integer) in which row the title should be added. Default = 1

- start_col:

  (single character or numeric) From which column should the texts
  start? Default = "A"

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
add_styled_line(wb, "some text", style = "title")
#> Warning: Could not find style(s): title
#> Error in if (is_dims(style)) {    styid <- self$get_cell_style(dims = style, sheet = sheet)} else {    styid <- self$styles_mgr$get_xf_id(style)}: argument is of length zero
```
