#' Create And Add A Form Overview To The Data Dictionary
#'
#' @param wb a workbook
#' @param datadict_tables a list with tables for the data dictionary,
#'     created by [create_datadict_tables()]
#' @param title (character) Title of the document. Default = NULL
#' @param subtitle (character) subtitle of the document,
#'     e.g. researcher - study name.
#'     Default = NULL'
#' @param as_of_date (date) when the data set was created. Default = NULL
#' @param form_type_description (logical), Should short explanation of the
#'     different form types added to the form overview? Default = TRUE
#' @param doc_width (single character or numeric) Up to which column should the
#'     titles and texts spread? Default = "G"
#'
#' @return invisible, the workbook with a form overview sheet added to it.
#' @export
#'
# @examples
add_form_overview <- function(wb,
                              datadict_tables,
                              title = NULL,
                              subtitle = NULL,
                              as_of_date = NULL,
                              form_type_description = TRUE,
                              doc_width = "G") {
  # init form overview sheet
  wb$add_worksheet("Form Overview")

  # check if styles are already registered, if not do so
  required_styles <- c(
    "title", "subtitle", "as_of_date", "heading_1",
    "text_area", "form_overview_section", "table_head",
    "visit_names", "table_names"
  )
  if (!all(required_styles %in% wb$styles_mgr$xf$name)) {
    style_datadict(wb)
  }

  # set column widths
  wb$set_col_widths(cols = 1, widths = 18)
  wb$set_col_widths(cols = 2, widths = 80)
  wb$set_col_widths(cols = 3:20, widths = 18)

  # convert doc_width
  if (is.numeric(doc_width)) doc_width <- openxlsx2::int2col(doc_width)
  if (is.character(doc_width)) doc_width <- toupper(doc_width)

  # init row index at which row to add the element
  row_idx <- 1

  # add title
  if (!is.null(title)) {
    wb$add_data(x = title, dims = str_glue("A{row_idx}"))
    wb$merge_cells(dims = str_glue("A{row_idx}:{doc_width}{row_idx}"))
    wb$set_cell_style(
      dims = str_glue("A{row_idx}"),
      style = wb$styles_mgr$get_xf_id("title")
    )
    row_idx <- row_idx + 1
  }

  # add subtitle
  if (!is.null(subtitle)) {
    wb$add_data(x = subtitle, dims = str_glue("A{row_idx}"))
    wb$merge_cells(dims = str_glue("A{row_idx}:{doc_width}{row_idx}"))
    wb$set_cell_style(
      dims = str_glue("A{row_idx}"),
      style = wb$styles_mgr$get_xf_id("subtitle")
    )
    row_idx <- row_idx + 1
  }

  # add as_of_date
  if (!is.null(as_of_date)) {
    wb$add_data(
      x = paste(stdatadictEnv$i18n_dd$t("as_of"), as_of_date), # TODO: format date with shiny.i18n (?)
      dims = str_glue("A{row_idx}")
    )
    wb$merge_cells(dims = str_glue("A{row_idx}:{doc_width}{row_idx}"))
    wb$set_cell_style(
      dims = str_glue("A{row_idx}"),
      style = wb$styles_mgr$get_xf_id("as_of_date")
    )
    row_idx <- row_idx + 1
  }

  # add white row
  if (!is.null(c(title, subtitle, as_of_date))) {
    wb$merge_cells(dims = str_glue("A{row_idx}:{doc_width}{row_idx}"))
    wb$set_cell_style(
      dims = str_glue("A{row_idx}"),
      style = wb$styles_mgr$get_xf_id("text_area")
    )
    row_idx <- row_idx + 1
  }

  # add heading
  wb$add_data(
    x = stdatadictEnv$i18n_dd$t("form_overview_section"),
    dims = str_glue("A{row_idx}")
  )
  wb$merge_cells(dims = str_glue("A{row_idx}:{doc_width}{row_idx}"))
  wb$set_cell_style(
    dims = str_glue("A{row_idx}:{doc_width}{row_idx}"),
    style = wb$styles_mgr$get_xf_id("heading_1")
  )
  # TODO write bug report: Border styles Styles are not transferred to merged cells
  row_idx <- row_idx + 1

  # add white row
  wb$merge_cells(dims = str_glue("A{row_idx}:{doc_width}{row_idx}"))
  wb$set_cell_style(
    dims = str_glue("A{row_idx}"),
    style = wb$styles_mgr$get_xf_id("text_area")
  )
  row_idx <- row_idx + 1

  # add descriptions
  if (form_type_description) {
    # rows that needs two have a hight, that it can hold two lines
    l2rows <- c()

    # merge every line over the whole document width
    for (i in 0:8) {
      wb$merge_cells(dims = str_glue("A{row_idx+i}:{doc_width}{row_idx+i}"))
    }

    test_two_rows <- function(text) {
      if (round(nchar(text) * 0.86) > 18 + 80 + 18 * (col2int(doc_width) - 2)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }

    # form type intro
    text <- stdatadictEnv$i18n_dd$t("form_descr_intro")
    if (test_two_rows(text)) l2rows <- append(l2rows, row_idx)

    wb$add_data(
      x = paste(fmt_txt(text, italic = TRUE)),
      dims = str_glue("A{row_idx}")
    )

    # visit forms
    text1 <- stdatadictEnv$i18n_dd$t("visit_forms_descr_item")
    text2 <- stdatadictEnv$i18n_dd$t("visit_forms_descr")

    if (test_two_rows(paste(text1, text2))) l2rows <- append(l2rows, row_idx + 1)

    wb$add_data(
      x = paste(fmt_txt(text1, bold = TRUE), fmt_txt(" "), fmt_txt(text2)),
      dims = str_glue("A{row_idx+1}")
    )

    # casenode forms
    text1 <- stdatadictEnv$i18n_dd$t("casenode_forms_descr_item")
    text2 <- stdatadictEnv$i18n_dd$t("casenode_forms_descr")

    if (test_two_rows(paste(text1, text2))) l2rows <- append(l2rows, row_idx + 2)

    wb$add_data(
      x = paste(fmt_txt(text1, bold = TRUE), fmt_txt(" "), fmt_txt(text2)),
      dims = str_glue("A{row_idx+2}")
    )

    # subforms
    text1 <- stdatadictEnv$i18n_dd$t("subforms_descr_item")
    text2 <- stdatadictEnv$i18n_dd$t("subforms_descr")

    if (test_two_rows(paste(text1, text2))) l2rows <- append(l2rows, row_idx + 3)

    wb$add_data(
      x = paste(fmt_txt(text1, bold = TRUE), fmt_txt(" "), fmt_txt(text2)),
      dims = str_glue("A{row_idx+3}")
    )

    # form item sheets description
    text <- stdatadictEnv$i18n_dd$t("form_items_sheet_descr")

    if (test_two_rows(text)) l2rows <- append(l2rows, row_idx + 5)

    wb$add_data(
      x = paste(fmt_txt(text, italic = TRUE)),
      dims = str_glue("A{row_idx+5}")
    )

    # hidden description -------------------------------------------------------
    text <- stdatadictEnv$i18n_dd$t("hidden_descr")
    if (test_two_rows(text)) l2rows <- append(l2rows, row_idx + 7)

    wb$add_data(
      x = paste(fmt_txt(text, italic = TRUE)),
      dims = str_glue("A{row_idx+7}")
    )

    # does some text need two lines?
    if (length(l2rows > 0)) {
      wb$set_row_heights(rows = l2rows, heights = 29)
    }

    wb$set_cell_style(
      dims = str_glue("A{row_idx}:{doc_width}{row_idx+7}"),
      style = wb$styles_mgr$get_xf_id("text_area")
    )

    row_idx <- row_idx + 9
  }

  # "Forms at Visits" ----------------------------------------------------------
  # section title
  wb$add_data(
    x = stdatadictEnv$i18n_dd$t("visit_forms_subsection"),
    dims = str_glue("A{row_idx}")
  )

  wb$merge_cells(dims = str_glue("A{row_idx}:{doc_width}{row_idx}"))

  wb$set_cell_style(
    dims = str_glue("A{row_idx}:{doc_width}{row_idx}"),
    style = wb$styles_mgr$get_xf_id("form_overview_section")
  )

  row_idx <- row_idx + 2

  # visit forms table
  data <- datadict_tables$form_overview$visit_forms %>%
    select(-"hidden")

  wb$add_data(x = data, dims = str_glue("A{row_idx}"))

  wb$set_cell_style(
    dims = str_glue("A{row_idx}:B{row_idx}"),
    style = wb$styles_mgr$get_xf_id("table_head")
  )

  wb$set_cell_style(
    dims = str_glue("C{row_idx}:{int2col(length(data))}{row_idx}"),
    style = wb$styles_mgr$get_xf_id("visit_names")
  )

  wb$set_cell_style(
    dims = str_glue("A{row_idx+1}:A{row_idx + nrow(data)}"),
    style = wb$styles_mgr$get_xf_id("table_names")
  )

  wb$add_font(
    dims = wb_dims(x = data, cols = 3:(length(data)), from_row = row_idx),
    b = TRUE
  )
  wb$add_cell_style(
    dims = wb_dims(x = data, cols = 3:(length(data)), from_row = row_idx),
    horizontal = "center"
  )

  # grey out hidden forms
  hidden <- datadict_tables$form_overview$visit_forms %>%
    select("hidden") %>%
    mutate(row_nr = dplyr::row_number()) %>%
    filter(as.logical(.data$hidden)) %>%
    dplyr::pull(.data$row_nr)

  hidden_font_color <- stdatadictEnv$use_color_theme %>%
    get_color_theme() %>%
    .$font_color_hidden

  for (row in hidden) {
    wb$add_font(
      dims = wb_dims(x = data, rows = row, from_row = row_idx),
      color = wb_color(hidden_font_color)
    )
  }

  wb$add_border(
    dims = wb_dims(x = data, from_row = row_idx),
    inner_hgrid = "thin",
    inner_hcolor = wb_color("black"),
    inner_vgrid = "thin",
    inner_vcolor = wb_color("black")
  )

  row_idx <- row_idx + nrow(data) + 3

  # casenode forms -------------------------------------------------------------
  # section title
  wb$add_data(
    x = stdatadictEnv$i18n_dd$t("casenode_forms_subsection"),
    dims = str_glue("A{row_idx}")
  )

  wb$merge_cells(dims = str_glue("A{row_idx}:{doc_width}{row_idx}"))

  wb$set_cell_style(
    dims = str_glue("A{row_idx}:{doc_width}{row_idx}"),
    style = wb$styles_mgr$get_xf_id("form_overview_section")
  )

  row_idx <- row_idx + 2

  # # casenode forms table
  data <- datadict_tables$form_overview$casenode_forms %>%
    select(-"hidden")

  wb$add_data(x = data, dims = str_glue("A{row_idx}"))

  wb$set_cell_style(
    dims = wb_dims(x = data, select = "col_names", from_row = row_idx),
    style = wb$styles_mgr$get_xf_id("table_head")
  )

  wb$set_cell_style(
    dims = str_glue("A{row_idx+1}:A{row_idx + nrow(data)}"),
    style = wb$styles_mgr$get_xf_id("table_names")
  )

  hidden <- datadict_tables$form_overview$casenode_forms %>%
    select("hidden") %>%
    mutate(row_nr = dplyr::row_number()) %>%
    filter(as.logical(.data$hidden)) %>%
    dplyr::pull(.data$row_nr)

  hidden_font_color <- stdatadictEnv$use_color_theme %>%
    get_color_theme() %>%
    .$font_color_hidden

  for (row in hidden) {
    wb$add_font(
      dims = wb_dims(x = data, rows = row, from_row = row_idx),
      color = wb_color(hidden_font_color)
    )
  }

  wb$add_border(
    dims = wb_dims(x = data, from_row = row_idx),
    inner_hgrid = "thin",
    inner_hcolor = wb_color("black"),
    inner_vgrid = "thin",
    inner_vcolor = wb_color("black")
  )

  row_idx <- row_idx + nrow(data) + 3


  # subforms -------------------------------------------------------------------

  # section title
  wb$add_data(
    x = stdatadictEnv$i18n_dd$t("subforms_subsection"),
    dims = str_glue("A{row_idx}")
  )

  wb$merge_cells(dims = str_glue("A{row_idx}:{doc_width}{row_idx}"))

  wb$set_cell_style(
    dims = str_glue("A{row_idx}:{doc_width}{row_idx}"),
    style = wb$styles_mgr$get_xf_id("form_overview_section")
  )

  row_idx <- row_idx + 2

  # subforms table
  data <- datadict_tables$form_overview$sub_forms %>%
    select(-"hidden")

  wb$add_data(x = data, dims = str_glue("A{row_idx}"))

  wb$set_cell_style(
    dims = wb_dims(x = data, select = "col_names", from_row = row_idx),
    style = wb$styles_mgr$get_xf_id("table_head")
  )

  wb$set_cell_style(
    dims = str_glue("A{row_idx+1}:A{row_idx + nrow(data)}"),
    style = wb$styles_mgr$get_xf_id("table_names")
  )

  hidden <- datadict_tables$form_overview$sub_forms %>%
    select("hidden") %>%
    mutate(row_nr = dplyr::row_number()) %>%
    filter(as.logical(.data$hidden)) %>%
    dplyr::pull(.data$row_nr)

  hidden_font_color <- stdatadictEnv$use_color_theme %>%
    get_color_theme() %>%
    .$font_color_hidden

  for (row in hidden) {
    wb$add_font(
      dims = wb_dims(x = data, rows = row, from_row = row_idx),
      color = wb_color(hidden_font_color)
    )
  }

  for (row in 0:nrow(data)) {
    wb$merge_cells(dims = str_glue("D{row_idx+row}:{doc_width}{row_idx+row}"))
  }

  # merge same mainform cells together
  merge_rows <- data %>%
    select(1) %>%
    mutate(start_row = dplyr::row_number()) %>%
    summarise(
      n = dplyr::n(), .by = stdatadictEnv$i18n_dd$t("mainform_col"),
      start_row = dplyr::first(.data$start_row),
      end_row = .data$start_row + .data$n - 1
    ) %>%
    filter(.data$n > 1)

  for (row in 1:nrow(merge_rows)) {
    start <- merge_rows[row, ]$start_row
    end <- merge_rows[row, ]$end_row
    wb$merge_cells(dims = str_glue("A{row_idx+start}:A{row_idx+end}"))
    wb$merge_cells(dims = str_glue("B{row_idx+start}:B{row_idx+end}"))
  }

  wb$add_cell_style(
    dims = wb_dims(x = data, cols = 1:2, from_row = row_idx),
    vertical = "top"
  )

  wb$add_border(
    dims = str_glue("A{row_idx}:{doc_width}{nrow(data)+row_idx}"),
    inner_hgrid = "thin",
    inner_hcolor = wb_color("black"),
    inner_vgrid = "thin",
    inner_vcolor = wb_color("black")
  )

  row_idx <- row_idx + nrow(data) + 3

  invisible(wb)
}
