# Create And Register Data Dictionary Workbook Styles

Create And Register Data Dictionary Workbook Styles

## Usage

``` r
style_datadict(wb, theme_name = stdatadictEnv$use_color_theme)
```

## Arguments

- wb:

  a workbook

- theme_name:

  Character Name of the color theme that should be used for the data
  dictionary

## Value

workbook with added styles, invisibly

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
```
