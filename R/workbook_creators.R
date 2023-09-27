#' Insert a row in a sheet (emulated)
#'
#' @description
#' Emulate the function of inserting one or multiple rows in a data sheet.
#' This is done, by copying and pasting the rows that comes after the inserted
#' rows and then clearing the insertion rows from data.
#'
#' Be careful with this function. It will likely mess up the formulas in that
#' sheet, or that refer to that sheet.
#'
#' @param wb A Workbook object containing a worksheet.
#' @param sheet The worksheet where to insert row(s). Can be the worksheet
#'     index or name. Default: current_sheet()
#' @param insert_row numeric, where to insert a row. Default = 1
#' @param nrows how many rows should be inserted. Default = 1
#' @param below Boolean indicating if the rows inserted below `insert_row`.
#'     Default = FALSE
#'
#' @return A wbWorkbook, invisibly
#' @export
#'
#' @examples
#' library(openxlsx2)
#'
#' # create workbook
#' wb <- wb_workbook()$
#' add_worksheet()$
#'  add_data(x = LifeCycleSavings, row_names = TRUE)
#'
#' # insert two lines above row 4
#' wb_insert_row(wb, insert_row = 4, nrows = 2)
wb_insert_row <- function(
    wb,
    sheet = current_sheet(),
    insert_row = 1,
    nrows = 1,
    below = FALSE) {
  # check if insert_rows and and nrows are natural numbers.
  if(!is.natural(insert_row, incl_zero = FALSE)) {
    stop("'insert_row' must be a positive integer.")
  }
  if(!is.natural(nrows, incl_zero = FALSE)) {
    stop("'nrows' must be a positive integer.")
  }

  last_cell <- get_last_sheet_cell(wb)
  last_col <- last_cell %>% str_extract("[[:upper:]]+")
  last_row <- last_cell %>% str_extract("[[:digit:]]+")

  # copy everything below the insert_row and move it downwards
  lower_dims <- str_glue("A{insert_row + below}:{last_col}{last_row}")
  lower_data <- wb_data(wb, dims = lower_dims, col_names = FALSE)
  wb$copy_cells(
    data = lower_data,
    dims = str_glue("A{insert_row + nrows + below}")
  )

  # clear the 'inserted' line(s)
  clear_dims <- str_glue(
    "A{insert_row + below}:{last_col}{insert_row + nrows - 1 + below}"
    )
  wb$clean_sheet(dims = clear_dims)

  invisible(wb)
}


#' Insert a Worksheet into the Workbook
#'
#' @inheritParams openxlsx2::wb_add_worksheet
#' @param .after postion where the sheet should be inserted. This can be either
#'     a sheet name from the workbook or an integer that specifies the sheet
#'     position. Set to "0" (Default), if you want to insert the sheet as first.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> additional options to pass to
#'     [openxlsx2::wb_add_worksheet()]
#'
#' @return A wbWorkbook, invisibly
#'
#' @seealso [openxlsx2::wb_add_worksheet()] [openxlsx2::wb_set_order()]
#' @export
#'
#' @examples
#' library(openxlsx2)
#'
#' ## setup a workbook with 2 worksheets
#' wb <- wb_workbook()
#' wb$add_worksheet("Sheet 1", gridLines = FALSE)
#' wb$add_data_table(sheet = 1, x = iris)
#'
#' wb$add_worksheet("mtcars (Sheet 2)", gridLines = FALSE)
#' wb$add_data(sheet = 2, x = mtcars)
#'
#' ## insert a third worksheet after "Sheet 1"
#' insert_worksheet(wb, .after = "Sheet 1", sheet = "Sheet 3", gridLines = FALSE)
#'
#' ## add data to the inserted sheet
#' wb$add_data(x = Formaldehyde)
insert_worksheet <- function(wb, .after = 0, ...) {
  old_sheet_order <- openxlsx2::wb_get_order(wb)
  old_sheet_names <- wb$get_sheet_names()

  if(is.character(.after)) {
    .after <- match(.after, old_sheet_names)
  }

  if (!(.after %in% 1:length(old_sheet_order))) {
    stop("'.after' must be the name of an already existing sheet or an existing
         sheet number.")
  }

  wb$add_worksheet(...)

  new_sheet_nr <- openxlsx2::wb_get_order(wb) %>% dplyr::last()

  new_sheet_order <- append(old_sheet_order, new_sheet_nr, after = .after)

  wb$set_order(new_sheet_order)

  # set current sheet to the just added sheet. After applying the set_order()
  # function current_sheet has two values, which doesn't work.
  wb$.__enclos_env__$private$current_sheet <- new_sheet_nr

  invisible(wb)
}

?wb_set_order()
