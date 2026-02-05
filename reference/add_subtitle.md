# Add Document Subtitle

Add Document Subtitle

## Usage

``` r
add_subtitle(wb, x, sheet = current_sheet(), row = 2, doc_width = "G")
```

## Arguments

- wb:

  A Workbook object containing a worksheet.

- x:

  (character) The document subtitle

- sheet:

  The worksheet to write to. Can be the worksheet index or name.

- row:

  in which row the subtitle should be added. Default = 2

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
add_subtitle(wb, "My Subtitle")
```
