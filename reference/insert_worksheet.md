# Insert a Worksheet into the Workbook

Insert a Worksheet into the Workbook

## Usage

``` r
insert_worksheet(wb, .after = 0, ...)
```

## Arguments

- wb:

  A `wbWorkbook` object to attach the new worksheet

- .after:

  postion where the sheet should be inserted. This can be either a sheet
  name from the workbook or an integer that specifies the sheet
  position. Set to "0" (Default), if you want to insert the sheet as
  first.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  additional options to pass to
  [`openxlsx2::wb_add_worksheet()`](https://janmarvin.github.io/openxlsx2/reference/wb_add_worksheet.html)

## Value

A wbWorkbook, invisibly

## See also

[`openxlsx2::wb_add_worksheet()`](https://janmarvin.github.io/openxlsx2/reference/wb_add_worksheet.html)
[`openxlsx2::wb_set_order()`](https://janmarvin.github.io/openxlsx2/reference/wb_order.html)

## Examples

``` r
library(openxlsx2)

## setup a workbook with 2 worksheets
wb <- wb_workbook()
wb$add_worksheet("Sheet 1", gridLines = FALSE)
wb$add_data_table(sheet = 1, x = iris)

wb$add_worksheet("mtcars (Sheet 2)", gridLines = FALSE)
wb$add_data(sheet = 2, x = mtcars)

## insert a third worksheet after "Sheet 1"
insert_worksheet(wb, .after = "Sheet 1", sheet = "Sheet 3", gridLines = FALSE)

## add data to the inserted sheet
wb$add_data(x = Formaldehyde)
```
