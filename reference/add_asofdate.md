# Add Export As of Date

Add Export As of Date

## Usage

``` r
add_asofdate(wb, x, sheet = current_sheet(), row = 3, doc_width = "G")
```

## Arguments

- wb:

  A Workbook object containing a worksheet.

- x:

  (character or Date time) The Date (time) when the Export data was
  created

- sheet:

  The worksheet to write to. Can be the worksheet index or name.

- row:

  in which row the As Of Date should be added. Default = 3

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
add_asofdate(wb, "2022-11-20")
```
