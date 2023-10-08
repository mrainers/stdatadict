add_form_sheets <- function(wb, datadict_tables, var_select = FALSE) {

  # check if styles are already registered, if not do so
  required_styles <- c(
    "form_sheet_head", "form_sheet_tables",
    "text_area", "table_head", "table_names"
  )
  if (!all(required_styles %in% wb$styles_mgr$xf$name)) {
    style_datadict(wb)
  }

  form_items <- datadict_tables$form_items

  doc_width <- form_items[[1]] %>%
    select(-any_of(c("hidden", "select"))) %>%
    length() %>%
    `+`(var_select) %>%
    int2col()

  form_names <- datadict_tables$form_overview[1:2] %>%
    map(~ .x %>% select(1:2)) %>%
    dplyr::bind_rows() %>%
    tibble::deframe()

  form_subtables <- datadict_tables$form_overview[[3]] %>%
    rename(formname = 3) %>%
    summarise(subtables = paste(formname, collapse = ", "), .by = 1) %>%
    tibble::deframe() %>%
    as.list()

  form_tables <- names(form_items) %>%
    map(~ str_c(.x, form_subtables[[.x]], sep = ", ")) %>%
    purrr::set_names(names(form_items))

    form_items %>%
      iwalk(~ {
        # create worksheet
        wb$add_worksheet(.y)

        # first row: form name
        wb$add_data(sheet = .y, x = form_names[[.y]], dims = "B1")
        wb$merge_cells(sheet = .y, dims = str_glue("B1:{doc_width}1"))
        wb$set_cell_style(sheet = .y, dims = str_glue("A1:{doc_width}1"),
                          style = wb$styles_mgr$get_xf_id("form_sheet_head"))

        # second row: Table names belonging to the form
        wb$add_data(sheet = .y,
                    x = stdatadictEnv$i18n_dd$t("tables_row"),
                    dims = "A2")
        wb$add_data(sheet = .y, x = form_tables[[.y]], dims = "B2")
        wb$merge_cells(sheet = .y, dims = str_glue("B2:{doc_width}2"))
        wb$set_cell_style(sheet = .y, dims = str_glue("A2"),
                          style = wb$styles_mgr$get_xf_id("form_sheet_head"))
        wb$set_cell_style(sheet = .y, dims = str_glue("B2:{doc_width}2"),
                          style = wb$styles_mgr$get_xf_id("form_sheet_tables"))

        # add form item table
        data <- .x %>% select(-"hidden")

        row_idx <- 4+var_select

        wb$add_data(
          sheet = .y,
          x = data,
          dims = str_glue("A{row_idx}"),
          na.strings = "")
        # add table head_cell style
        wb$set_cell_style(
          sheet = .y,
          dims = wb_dims(x = data, select = "col_names", from_row = 4+var_select),
          style = wb$styles_mgr$get_xf_id("table_head")
        )
        wb$add_cell_style(
          sheet = .y,
          dims = wb_dims(x = data, select = "col_names", from_row = 4+var_select),
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

        hidden_font_color <- stdatadictEnv$use_color_theme %>%
          get_color_theme() %>%
          .$font_color_hidden

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
          summarise(row = dplyr::first(row),
                    unhide = dplyr::first(hidden) & !all(hidden)) %>%
          filter(unhide) %>%
          pull(row)

        for (row in unhide_questions) {
          wb$add_font(dims = str_glue("A{row+row_idx}"), color = wb_color("auto"))
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
        wb$set_col_widths(.y, cols = 1, widths = 18)
        wb$set_col_widths(.y, cols = 2:3, widths = 40)
        wb$set_col_widths(.y, cols = 4:5, widths = "auto")
        wb$set_col_widths(.y, cols = 6, widths = 60)
      })

}


