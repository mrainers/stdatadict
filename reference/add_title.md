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
wb$add_worksheet()
add_title(wb, "My Workbook Title")
```
