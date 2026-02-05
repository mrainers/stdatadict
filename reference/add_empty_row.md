# Format an empty Row in the worksheet

The row is styled as the style of "text_area", which is the default font
with a white background.

## Usage

``` r
add_empty_row(wb, sheet = current_sheet(), row = 1, doc_width = "G")
```

## Arguments

- wb:

  A Workbook object containing a worksheet.

- sheet:

  The worksheet to write to. Can be the worksheet index or name.

- row:

  in which row the As Of Date should be added. Default = 1

- doc_width:

  (single character or numeric) Up to which column should the texts
  spread? Default = "G"

## Value

A wbWorkbook, invisibly

## Examples

``` r
wb <- openxlsx2::wb_workbook()
style_datadict(wb)
wb$add_worksheet()

add_empty_row(wb, row = 3)
```
