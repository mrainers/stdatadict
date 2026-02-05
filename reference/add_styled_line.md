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
wb$add_worksheet()
add_styled_line(wb, "some text", style = "title")
```
