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

  if (!(.after %in% 0:length(old_sheet_order))) {
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


#' Create A data dictionary
#'
#' Wrapper function for creating a data dictionary. This function calls:
#' [openxlsx2::wb_workbook()], [add_form_overview()] and [add_form_sheets()]
#'
#' @inheritParams add_form_overview
#'
#' @return An excel workbook
#' @export
#'
#' @examples \dontrun{create_datadict(datadict_tables)}
create_datadict <- function(datadict_tables,
                            title = NULL,
                            subtitle = NULL,
                            as_of_date = NULL,
                            form_type_description = TRUE,
                            doc_width = "G") {
  wb <- openxlsx2::wb_workbook()

  add_form_overview(wb,
                    datadict_tables = datadict_tables,
                    title = title,
                    subtitle = subtitle,
                    as_of_date = as_of_date,
                    form_type_description = form_type_description,
                    doc_width = doc_width)

  # this may take some seconds
  add_form_sheets(wb, datadict_tables = datadict_tables)

  wb
}


#' Add Styled Text To The Workbook
#'
#' Add some Text to a given row, merge the cells of that row and apply a
#' predefined style.
#'
#' @param wb A Workbook object containing a worksheet.
#' @param x The text to add to the workbook
#' @param sheet The worksheet to write to. Can be the worksheet index or name.
#' @param row (integer) in which row the title should be added. Default = 1
#' @param start_col (single character or numeric) From which column should the
#'     texts start? Default = "A"
#' @param doc_width (single character or numeric) Up to which column should the
#'     texts spread? Default = "G"
#' @param style (character) name of the cell style defined to the workbook.
#'     If your worbook is named `wb` enter `wb$styles_mgr$xf$name` to get a list
#'     of style names registered to the workbook
#'
#' @return A wbWorkbook, invisibly
#' @export
#'
#' @examples
#' wb <- openxlsx2::wb_workbook()
#' style_datadict(wb)
#' wb$add_worksheet()
#' add_styled_line(wb, "some text", style = "title")
add_styled_line <- function(wb,
                            x,
                            style,
                            sheet = current_sheet(),
                            row = 1,
                            start_col = "A",
                            doc_width = "G") {
  # convert start_col
  if (is.numeric(start_col)) start_col <- openxlsx2::int2col(start_col)
  if (is.character(start_col)) start_col <- toupper(start_col)

  # convert doc_width
  if (is.numeric(doc_width)) doc_width <- openxlsx2::int2col(doc_width)
  if (is.character(doc_width)) doc_width <- toupper(doc_width)

  # add text
  # wrap x in paste function, in case x is a formatted text.
  # openxlsx2 has a bug, that wont print formated text.
  wb$add_data(x = paste(x), sheet = sheet, dims = str_glue("{start_col}{row}"))

  # merge cells in subtitle row
  wb$merge_cells(sheet = sheet,
                 dims = str_glue("{start_col}{row}:{doc_width}{row}"))

  # apply subtitle style
  # TODO write bug report: Border styles Styles are not transferred to merged cells
  wb$set_cell_style(
    sheet = sheet,
    dims = str_glue("{start_col}{row}:{doc_width}{row}"),
    style = wb$styles_mgr$get_xf_id(style)
  )

  invisible(wb)
}

#' Add Document Title
#'
#' @param x (character) The document title
#' @inheritParams add_styled_line
#'
#' @return A wbWorkbook, invisibly
#' @export
#'
#' @examples
#' wb <- openxlsx2::wb_workbook()
#' style_datadict(wb)
#' wb$add_worksheet()
#' add_title(wb, "My Workbook Title")
add_title <- function(wb,
                      x,
                      sheet = current_sheet(),
                      row = 1,
                      doc_width = "G") {
  wb <- add_styled_line(wb = wb,
                        x = x,
                        style = "title",
                        sheet = sheet,
                        row = row,
                        start_col = "A",
                        doc_width = doc_width)

  invisible(wb)
}


#' Add Document Subtitle
#'
#' @param x (character) The document subtitle
#' @param row in which row the subtitle should be added. Default = 2
#' @inheritParams add_styled_line
#'
#' @return A wbWorkbook, invisibly
#' @export
#'
#' @examples
#' wb <- openxlsx2::wb_workbook()
#' style_datadict(wb)
#' wb$add_worksheet()
#' add_subtitle(wb, "My Subtitle")
add_subtitle <- function(wb,
                         x,
                         sheet = current_sheet(),
                         row = 2,
                         doc_width = "G") {
  wb <- add_styled_line(wb = wb,
                        x = x,
                        style = "subtitle",
                        sheet = sheet,
                        row = row,
                        start_col = "A",
                        doc_width = doc_width)

  invisible(wb)
}


#' Add Export As of Date
#'
#' @param x (character or Date time) The Date (time) when the Export data
#'     was created
#' @param row in which row the As Of Date should be added. Default = 3
#' @inheritParams add_title
#'
#' @return A wbWorkbook, invisibly
#' @export
#'
#' @examples
#' wb <- openxlsx2::wb_workbook()
#' style_datadict(wb)
#' wb$add_worksheet()
#' add_asofdate(wb, "2022-11-20")
add_asofdate <- function(wb,
                         x,
                         sheet = current_sheet(),
                         row = 3,
                         doc_width = "G") {
  wb <- add_styled_line(wb = wb,
                        x = x,
                        style = "as_of_date",
                        sheet = sheet,
                        row = row,
                        start_col = "A",
                        doc_width = doc_width)

  invisible(wb)
}


#' Add Heading 1
#'
#' @param x (character) The Heading text
#' @param row in which row the heading should be added. Default = 1
#' @inheritParams add_title
#'
#' @return A wbWorkbook, invisibly
#' @export
#'
#' @examples
#' wb <- openxlsx2::wb_workbook()
#' style_datadict(wb)
#' wb$add_worksheet()
#' add_heading1(wb, "My Subtitle")
add_heading1 <- function(wb,
                         x,
                         sheet = current_sheet(),
                         row = 1,
                         doc_width = "G") {
  wb <- add_styled_line(wb = wb,
                        x = x,
                        style = "heading_1",
                        sheet = sheet,
                        row = row,
                        start_col = "A",
                        doc_width = doc_width)

  invisible(wb)
}


#' Add Section Title
#'
#' @param x (character) The Section Title Text
#' @param row in which row the section Titleshould be added. Default = 1
#' @inheritParams add_title
#'
#' @return A wbWorkbook, invisibly
#' @export
#'
#' @examples
#' wb <- openxlsx2::wb_workbook()
#' style_datadict(wb)
#' wb$add_worksheet()
#' add_section(wb, "My Section title", row = 3)
add_section <- function(wb,
                        x,
                        sheet = current_sheet(),
                        row = 1,
                        doc_width = "G") {
  wb <- add_styled_line(wb = wb,
                        x = x,
                        style = "section",
                        sheet = sheet,
                        row = row,
                        start_col = "A",
                        doc_width = doc_width)

  invisible(wb)
}

#' Add A text paragraph
#'
#' This function merges cells of a given document with (e.g. A:G) and sets the
#' cell style to "text_area". Within this cell style the text is wrapped.
#' However Excel fails to set the correct row height, when text is added into
#' merged cells. This function tries to estimate the right number of lines needed
#' to display the text and set the row height (with some extra space at the end)
#' accordingly.
#'
#' @param x (character string) the text of the paragraph
#' @param row in which row the As Of Date should be added. Default = 1
#' @param font_weight (numeric) correction factor if the estimated number of
#'     needed lines is off. Default: 0.86
#' @inheritParams add_title
#'
#' @return A wbWorkbook, invisibly
#' @export
#'
#' @examples
#' wb <- openxlsx2::wb_workbook()
#' style_datadict(wb)
#' wb$add_worksheet()
#'
#' p <- paste(
#'   "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy",
#'   "eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam",
#'   "voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet",
#'   "clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.")
#'
#' add_paragraph(wb, p)
add_paragraph <- function(wb,
                         x,
                         sheet = current_sheet(),
                         row = 1,
                         doc_width = "G",
                         font_weight = 0.86) {
  wb <- add_styled_line(wb = wb,
                        x = x,
                        style = "text_area",
                        sheet = sheet,
                        row = row,
                        start_col = "A",
                        doc_width = doc_width)

  # estimate number of needed lines of the text
  font <- get_font_attributes(wb, "text_area")

  fontname <- font$name %||% "Calibri"
  fsize <- as.numeric(font$sz) %||% 11

  pwidth <- calc_colswidth(wb, sheet = sheet, cols = str_glue("A:{doc_width}"))
  lines <- calc_lines(text = x, pwidth = pwidth, font = fontname, fsize = fsize,
                      font_weight = font_weight)

  # row_height = nr of lines * font size + margin + extra space
  row_height <- (lines * (fsize + 4)) + fsize * 0.5

  # set cell height based on the number of guessed lines
  wb$set_row_heights(rows = row, heights = row_height)

  invisible(wb)
}

#' Format an empty Row in the worksheet
#'
#' The row is styled as the style of "text_area", which is the default font
#' with a white background.
#'
#' @inheritParams add_paragraph
#' @return A wbWorkbook, invisibly
#' @export
#'
#' @examples
#' wb <- openxlsx2::wb_workbook()
#' style_datadict(wb)
#' wb$add_worksheet()
#'
#' format_empty_row(wb, row = 3)
format_empty_row <- function(wb,
                          sheet = current_sheet(),
                          row = 1,
                          doc_width = "G") {
  if (is.numeric(doc_width)) doc_width <- openxlsx2::int2col(doc_width)
  if (is.character(doc_width)) doc_width <- toupper(doc_width)

  # this is to avoid a bug/error, when trying to style multiple cells at once
  # in an empty worksheet
  # TODO: write bug report
  wb$set_cell_style(
    sheet = sheet,
    dims = str_glue("A{row}"),
    style = wb$styles_mgr$get_xf_id("text_area")
  )

  # now apply the actual row styling
  wb$set_cell_style(
    sheet = sheet,
    dims = str_glue("A{row}:{doc_width}{row}"),
    style = wb$styles_mgr$get_xf_id("text_area")
  )

  invisible(wb)
}


#' Format Matched Strings In a Text.
#'
#' @param text (character) text that contains strings that should be formatted
#' @param pattern regular expressions, or vector of regular expressions
#' @param fmt_ops list of format options pass to [openxlsx2::fmt_txt()]
#'
#' @return an openxlsx2 fmt_txt string
#' @export
#'
#' @examples
#' shopping_list <- "apples x4, bag of flour, bag of sugar, milk x2"
#'
#' # format the word 'bag' bold
#' fmt_txt_at(shopping_list, "\\bbag\\b", list(bold = TRUE))
#'
#' # format the x<nr> green and bold and the "bag of" in italic
#' fmt_txt_at(shopping_list,
#'            c("x\\d+", "\\bbag of\\b"),
#'            list(list(bold = TRUE, color = openxlsx2::wb_color("green")),
#'                 list(italic = TRUE)
#'                )
#'            )
fmt_txt_at <- function(text, pattern, fmt_ops) {
  # find all strings that match the patterns

  fmt_pos <- text %>% stringr::str_locate_all(pattern)

  # find all strings that don' match any pattern
  no_fmt_pos <- fmt_pos %>%
    purrr::reduce(rbind) %>%
    # sort by start column
    {.[order(.[, "start"]),]} %>%
    stringr::invert_match()

  # create table of all substrings and their styling options
  fmt_strings <- map2(
    fmt_pos, fmt_ops,
    ~ tibble::as_tibble(.x) %>%
      mutate(fmts = list(.y))
  ) %>%
    append(list(
      no_fmt_pos %>%
        tibble::as_tibble() %>%
        mutate(fmts = list(list()))
    )) %>%
    dplyr::bind_rows() %>%
    arrange(.data$start)

  # create formatted text from substrings and format options
  purrr::pmap(fmt_strings, function(start, end, fmts) {
    rlang::exec("fmt_txt", x = str_sub(text, start, end), !!!fmts)
  }) %>%
    purrr::reduce(`+`)
}

