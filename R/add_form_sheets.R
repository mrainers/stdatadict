#' Ceate And Add Form Sheets To The Data Dictionary
#'
#' @description
#' Create a workbook sheet for every table in the datadict_tables$form_items
#' list.
#'
#' The parameter "var_select" is ignored so far but in future release a 'select'
#' column with selection rules will be added to the form sheet tables, if
#' 'var_select' = TRUE
#'
#' @param wb a workbook
#' @param datadict_tables generated with [create_datadict_tables()]
#' @param var_select (logical) Is this a variable selection document?
#'
#' @return workbook with form item tables added (invisibly)
#' @export
#'
#' @examples \dontrun{
#' wb <- wb_workbook()
#' add_form_overview(wb, datadict_tables = datadict_tables)
#' add_form_sheets(wb, datadict_tables = datadict_tables)
#' }
add_form_sheets <- function(wb, datadict_tables, var_select = FALSE) {

  # check if styles are already registered, if not do so
  required_styles <- c(
    "form_sheet_head", "form_sheet_tables",
    "text_area", "table_head", "form_sheet_table_col"
  )
  if (!all(required_styles %in% wb$styles_mgr$xf$name)) {
    style_datadict(wb)
  }

  form_items <- datadict_tables$form_items

  doc_width <- form_items[[1]] %>%
    names() %>%
    setdiff(c("hidden", "select_col", stdatadictEnv$i18n_dd$t("select_col"))) %>%
    length() %>%
    `+`(var_select) %>%
    int2col()

  form_names <- datadict_tables$form_overview[1:2] %>%
    map(~ .x %>% select(1:2)) %>%
    dplyr::bind_rows() %>%
    tibble::deframe()

  form_subtables <- datadict_tables$form_overview[[3]] %>%
    rename(formname = 3) %>%
    summarise(subtables = paste(.data$formname, collapse = ", "), .by = 1) %>%
    tibble::deframe() %>%
    as.list()

  form_tables <- names(form_items) %>%
    map(~ str_c(.x, form_subtables[[.x]], sep = ", ")) %>%
    purrr::set_names(names(form_items))

  # init "VarSelect Settings" sheet
  if (var_select & "VarSelect Settings" %notin% wb$get_sheet_names()) {
    add_varselect_settings(wb)
  }

  form_items %>%
    iwalk(~ {
      # create worksheet
      wb$add_worksheet(.y)

      # first row: form name
      wb$add_data(sheet = .y, x = form_names[[.y]], dims = "B1")
      wb$merge_cells(sheet = .y, dims = str_glue("B1:{doc_width}1"))
      wb$set_cell_style(
        sheet = .y, dims = str_glue("A1:{doc_width}1"),
        style = wb$styles_mgr$get_xf_id("form_sheet_head")
      )

      # second row: Table names belonging to the form
      wb$add_data(
        sheet = .y,
        x = stdatadictEnv$i18n_dd$t("tables_row"),
        dims = "A2"
      )
      wb$add_data(sheet = .y, x = form_tables[[.y]], dims = "B2")
      wb$merge_cells(sheet = .y, dims = str_glue("B2:{doc_width}2"))
      wb$set_cell_style(
        sheet = .y, dims = str_glue("A2"),
        style = wb$styles_mgr$get_xf_id("form_sheet_head")
      )
      wb$set_cell_style(
        sheet = .y, dims = str_glue("B2:{doc_width}2"),
        style = wb$styles_mgr$get_xf_id("form_sheet_tables")
      )

      ### add var_select stuff -------------------------------------------------

      if (var_select) {
        select_font_color <- get_color_theme()$font_select_column

        # add "select entire form?" question
        wb$add_data(x = stdatadictEnv$i18n_dd$t("select_entire_form"),
                    dims = "A4")
        wb$merge_cells(dims = str_glue("A4:{int2col(col2int(doc_width)-1)}4"))
        wb$add_font(dims = "A4",
                    color = wb_color(select_font_color),
                    size = 14, italic = TRUE)
        wb$add_cell_style(dims = "A4",
                          horizontal = "right",
                          vertical = "center",
                          )

        wb$add_data(x = stdatadictEnv$i18n_dd$t("select_no"),
                    dims = str_glue("{doc_width}4")
                    )
        wb$add_data_validation(dims = str_glue("{doc_width}4"),
                               type = "list",
                               value = "YesNo",
                               allow_blank = FALSE,
                               show_error_msg = FALSE
        )
        wb$add_font(dims = str_glue("{doc_width}4"),
                    color = wb_color(select_font_color),
                    bold = TRUE
                    )
        wb$add_cell_style(dims = str_glue("{doc_width}4"),
                          horizontal = "center",
                          vertical = "center",
        )

        # add named region for the select entire form answer
        wb$add_named_region(dims = str_glue("{doc_width}4"),
                            name = "all_form",
                            local_sheet = TRUE
        )

        # add a select column to data, if it doesn't exist.
        yes <- stdatadictEnv$i18n_dd$t("select_yes")
        select_col <- stdatadictEnv$i18n_dd$t("select_col")

        if ("select_col" %in% names(.x)) {
          .x <- .x %>% rename(!!select_col := "select_col")
        }

        if (select_col %notin% names(.x)) {

          # does the form item table has a "Scope" variable? If so, add Scope
          # rules to the selection formula.
          if ("Scope" %in% names(.x)) {
            .x <- .x %>%
              mutate(Scope = tidyr::replace_na(.data$Scope, "")) %>%
              rowwise() %>%
              mutate(!!select_col := if_else(
                stringr::str_trim(.data$Scope) != "",
                .data$Scope %>%
                  stringr::str_split_1(", ") %>%
                  clean_region_name() %>%
                  str_c(collapse = ",") %>%
                  { str_glue('IF(OR(all_form="{yes}",{.}),"X","")') },
                as.character(str_glue('IF(all_form="{yes}","X","")')),
              )) %>%
              ungroup()
          } else {
            .x <- .x %>%
              mutate(!!select_col := as.character(str_glue(
                'IF(all_form="{yes}","X","")'
              )))
          }
        }

        wb$set_row_heights(rows = 4, heights = 23)
      }

      ### add form item table --------------------------------------------------
      data <- .x %>% select(-"hidden")

      # convert select column entry to formula
      if (var_select) {
        class(data[[stdatadictEnv$i18n_dd$t("select_col")]]) <-
          c(class(data[[stdatadictEnv$i18n_dd$t("select_col")]]), "formula")
      }

      row_idx <- 4 + var_select

      wb$add_data(
        sheet = .y,
        x = data,
        dims = str_glue("A{row_idx}"),
        na.strings = ""
      )
      # add table head_cell style
      wb$set_cell_style(
        sheet = .y,
        dims = wb_dims(x = data, select = "col_names", from_row = 4 + var_select),
        style = wb$styles_mgr$get_xf_id("table_head")
      )
      wb$add_cell_style(
        sheet = .y,
        dims = wb_dims(x = data, select = "col_names", from_row = 4 + var_select),
        wrap_text = FALSE
      )
      # add tables column cell style
      wb$set_cell_style(
        sheet = .y,
        dims = wb_dims(x = data, cols = 1, from_row = 4 + var_select),
        style = wb$styles_mgr$get_xf_id("form_sheet_table_col")
      )

      # identify rows, that belong to the same question
      qs_groups <- .x %>%
        ungroup() %>%
        rename(qs = 2) %>%
        mutate(question_nr = cumsum(.data$qs != dplyr::lag(.data$qs) |
          dplyr::row_number() == 1))

      # merge same question cells together
      merge_rows <- qs_groups %>%
        mutate(start_row = dplyr::row_number()) %>%
        summarise(
          n = dplyr::n(),
          start_row = dplyr::first(.data$start_row),
          end_row = .data$start_row + .data$n - 1,
          .by = "question_nr"
        ) %>%
        filter(.data$n > 1)

      if (nrow(merge_rows) > 0) {
        for (row in 1:nrow(merge_rows)) {
          start <- merge_rows[row, ]$start_row
          end <- merge_rows[row, ]$end_row
          wb$merge_cells(dims = str_glue("B{row_idx+start}:B{row_idx+end}"))
        }
      }

      # grey out hidden forms
      hidden <- .x %>%
        select("hidden") %>%
        mutate(row_nr = dplyr::row_number()) %>%
        filter(as.logical(.data$hidden)) %>%
        dplyr::pull(.data$row_nr)

      hidden_font_color <- get_color_theme()$font_hidden

      for (row in hidden) {
        wb$add_font(
          dims = wb_dims(x = data, rows = row, from_row = row_idx),
          color = wb_color(hidden_font_color)
        )
      }

      # But not question cells that have none hidden items
      unhide_questions <- qs_groups %>%
        mutate(hidden = as.logical(replace_na(hidden, 0))) %>%
        mutate(row = dplyr::row_number()) %>%
        group_by(question_nr) %>%
        summarise(
          row = dplyr::first(row),
          unhide = dplyr::first(hidden) & !all(hidden)
        ) %>%
        filter(unhide) %>%
        pull(row)

      for (row in unhide_questions) {
        wb$add_font(dims = str_glue("B{row+row_idx}"), color = wb_color("auto"))
      }

      # wrap text in table
      wb$add_cell_style(
        sheet = .y,
        dims = wb_dims(x = data, cols = 1:3, from_row = row_idx),
        wrap_text = TRUE
      )
      wb$add_cell_style(
        sheet = .y,
        dims = wb_dims(x = data, cols = 6, from_row = row_idx),
        wrap_text = TRUE,
      )
      # Vertical text alignment = top in table
      wb$add_cell_style(
        sheet = .y,
        dims = wb_dims(x = data, from_row = row_idx),
        vertical = "top"
      )

      # set column width
      wb$set_col_widths(.y, cols = 1:col2int(doc_width), widths = "auto")
      wb$set_col_widths(.y, cols = 1, widths = 18)
      wb$set_col_widths(.y, cols = 2:3, widths = 40)
      # wb$set_col_widths(.y, cols = 4:5, widths = "auto")
      wb$set_col_widths(.y, cols = 6, widths = 60)

      ### Add styles for select column
      if (var_select) {
        select_col <- stdatadictEnv$i18n_dd$t("select_col")
        select_col_idx <- which(colnames(data) == select_col)

        # select_column_head
        wb$set_cell_style(
          dims = str_glue("{int2col(select_col_idx)}5"),
          style = wb$styles_mgr$get_xf_id("select_column_head")
        )
        # select_column
        wb$set_cell_style(
          dims = wb_dims(x = data, cols = select_col, from_row = 5),
          style = wb$styles_mgr$get_xf_id("select_column")
        )
        # column width
        wb$set_col_widths(.y, cols = select_col_idx, widths = 8.5)
      }
    })

  invisible(wb)
}
