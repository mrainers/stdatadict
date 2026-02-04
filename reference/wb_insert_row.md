# Insert a row in a sheet (emulated)

Emulate the function of inserting one or multiple rows in a data sheet.
This is done, by copying and pasting the rows that comes after the
inserted rows and then clearing the insertion rows from data.

Be careful with this function. It will likely mess up the formulas in
that sheet, or that refer to that sheet.

## Usage

``` r
wb_insert_row(
  wb,
  sheet = current_sheet(),
  insert_row = 1,
  nrows = 1,
  below = FALSE
)
```

## Arguments

- wb:

  A Workbook object containing a worksheet.

- sheet:

  The worksheet where to insert row(s). Can be the worksheet index or
  name. Default: current_sheet()

- insert_row:

  numeric, where to insert a row. Default = 1

- nrows:

  how many rows should be inserted. Default = 1

- below:

  Boolean indicating if the rows inserted below `insert_row`. Default =
  FALSE

## Value

A wbWorkbook, invisibly

## Examples

``` r
library(openxlsx2)

# create workbook
wb <- wb_workbook()$
add_worksheet()$
 add_data(x = LifeCycleSavings, row_names = TRUE)

# insert two lines above row 4
wb_insert_row(wb, insert_row = 4, nrows = 2)
```
