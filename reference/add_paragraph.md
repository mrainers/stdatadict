# Add A text paragraph

This function merges cells of a given document with (e.g. A:G) and sets
the cell style to "text_area". Within this cell style the text is
wrapped. However Excel fails to set the correct row height, when text is
added into merged cells. This function tries to estimate the right
number of lines needed to display the text and set the row height (with
some extra space at the end) accordingly.

## Usage

``` r
add_paragraph(
  wb,
  x,
  sheet = current_sheet(),
  row = 1,
  doc_width = "G",
  font_weight = 0.86
)
```

## Arguments

- wb:

  A Workbook object containing a worksheet.

- x:

  (character string) the text of the paragraph

- sheet:

  The worksheet to write to. Can be the worksheet index or name.

- row:

  in which row the As Of Date should be added. Default = 1

- doc_width:

  (single character or numeric) Up to which column should the texts
  spread? Default = "G"

- font_weight:

  (numeric) correction factor if the estimated number of needed lines is
  off. Default: 0.86

## Value

A wbWorkbook, invisibly

## Examples

``` r
wb <- openxlsx2::wb_workbook()
style_datadict(wb)
wb$add_worksheet()

p <- paste(
  "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy",
  "eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam",
  "voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet",
  "clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.")

add_paragraph(wb, p)
```
