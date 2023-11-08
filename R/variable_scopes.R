#' Create a Scope Table
#'
#' Create a table that defines variable scopes. This table can be used for
#' adding a "Scope"-Column (and Selection Rules) to the form item tables and
#' Scope-Selection-Tables in the workbook.
#'
#' @param data a data frame containing all variable identifiers (join keys) that
#'    belong to the scope.
#' @param scopeid (character) name/ID of the Scope.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> named list containing
#'    additional information about the variable Scope. This will be used as
#'    columns of the Scope Selection Table in the workbook.
#'
#' @return nested tibble decribing the Scope
#' @export
#'
#' @examples
#' scope_data <- scope_data <- data.frame (
#'   mainform = c("form1", "form1", "form2"),
#'   varname_col  = c("var1", "var3", "var10")
#' )
#' create_scope_table(
#'   scope_data,
#'   "my_scope",
#'   list(Description = "This Scope covers this and that",
#'        visits = "Baseline")
#' )
create_scope_table <- function(data, scopeid, ...) {
  tibble(Scope = scopeid) %>%
    dplyr::bind_cols(...) %>%
    mutate(data = list(tibble(data)))
}


#' Add Variable Scopes To Form Item Tables
#'
#' This functions joins a "Scope" variable with the form item tables in
#' "data_dict_tables". If form item tables already have a Scope variable, the
#' content of already existing and new Scope variable be concatenated.
#'
#' @details
#' A Scope table is a nested table with the following format:
#' \describe{
#'    \item{Scope}{ID String of the variable Scope}
#'    \item{additional Scope Information}{Variables that describe the Scope, such
#'        as Scope Label/Name, Description, etc.}
#'    \item{data}{a table containing variable names and other identifying
#'        variables to which the Scope belongs (joining key variables)}
#' }
#'
#'
#' @param datadict_tables table list, generated with [create_datadict_tables()]
#' @param scope "Scope Table" (see Details) or list of Scope tables.
#'
#' @return datadict_tables list where a Scope variable is added to form_item
#'      tables.
#' @export
#'
#' @examples
#' \dontrun{
#' datadict_tables <- join_scopes(datadict_tables, datapkg_pop_scope)
#' }
join_scopes <- function(datadict_tables, scope) {
  if (is.data.frame(scope)) scope <- list(scope)

  scope_data <- scope %>%
    map(~ .x %>%
          select("Scope", "data") %>%
          tidyr::unnest(cols = "data"))

  while (!purrr::is_empty(scope_data)) {
    # get translated variable names that will be used for table join
    by_vars <- scope_data[[1]] %>%
      select(-"Scope") %>%
      dplyr::rename_with(~ stdatadictEnv$i18n_dd$t(.x) %>%
                           suppressWarnings(),
                         .cols = -any_of("mainform")
      ) %>%
      names()

    # join all data tables that have the same (join) variables as the first
    # element.
    cat("Join Scopes by", paste(shQuote(by_vars), collapse = ", "), "\n")
    datadict_tables <- scope_data %>%
      purrr::keep(~ setequal(names(.), names(scope_data[[1]]))) %>%
      join_with_form_items(datadict_tables = datadict_tables, data = ., by = by_vars)

    # Those tables who have different variables, repeat procedure in the next round.
    scope_data <- scope_data %>%
      purrr::discard(~ setequal(names(.), names(scope_data[[1]])))
  }

  datadict_tables
}


#' Add Scope Selection Table to workbook
#'
#' @inheritParams openxlsx2::wb_add_data
#' @param scope "Scope Table", including at least a scopeID column named "Scope".
#' @param title (character) A title for the Scope selection table. Default = NULL
#'    If title = NULL, no title will be created.
#' @param row number where to start the current Scope Selection, default = 1
#' @param doc_width (single character or numeric) if a title is given, up to which
#'    column should the section title spread? Default = "G"
#'
#' @return wbWorkbook, invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' add_scope_selection(
#'   wb,
#'   scope = datapkg_scopes,
#'   title = "Data Packages",
#'   row = 5,
#'   doc_width = "I"
#' )
#' }
add_scope_selection <- function(wb,
                                scope,
                                title = NULL,
                                sheet = current_sheet(),
                                row = 1,
                                doc_width = "G"){

  # init "VarSelect Settings" Sheet if it doesn't exist.
  if ("VarSelect Settings" %notin% wb$get_sheet_names()) {
    add_varselect_settings(wb)
  }

  select_col <- stdatadictEnv$i18n_dd$t("select_col")

  # name of the named region for scope table
  varscope_regions <- wb$get_named_regions() %>%
    .$name %>% str_subset("varscope_")

  if (is_empty(varscope_regions)) {
    region_name <- "varscope_1"
  } else {
    region_name <- varscope_regions %>%
      stringr::str_sort(numeric = TRUE) %>%
      last() %>%
      str_replace("([:digit:]+)$", ~ as.numeric(.x) + 1)
  }

  # table for scope selection, remove data tables and add/rename select column
  scope_sel <- scope %>%
    select(-any_of("data")) %>%
    rename(any_of(`names<-`("select_col", select_col))) %>%
    { if (select_col %notin% names(.)) mutate(., {{ select_col }} := "") else .}

  # add title
  if (!is.null(title)) {
    add_section(wb, x = title, sheet = sheet, row = row, doc_width = doc_width)
    row = row + 2
  }

  ### if the scope table has multiple scopes
  if (nrow(scope) > 1) {
    # add a select all option
    wb$add_data(sheet = sheet,
                x = stdatadictEnv$i18n_dd$t("select_all"),
                start_row = row)

    # question
    dims_question <- paste0("A", row, ":", int2col(length(scope_sel) - 1), row)
    wb$merge_cells(dims = dims_question)
    wb$set_cell_style(dims = dims_question,
                      style = wb$styles_mgr$get_xf_id("select_all_q"))

    # answer
    dims_answer <- paste0(int2col(length(scope_sel)), row)

    wb$add_data(x = stdatadictEnv$i18n_dd$t("select_no"), dims = dims_answer)
    wb$add_data_validation(dims = dims_answer,
                           type = "list",
                           value = "YesNo",
                           allow_blank = FALSE,
                           show_error_msg = FALSE
    )
    wb$set_cell_style(dims = dims_answer,
                      style = wb$styles_mgr$get_xf_id("select_all_a"))

    row = row + 1

    # create select_formula, unless the scope table doesn't provides it's own
    # select formula
    if (!any(c("select_col", select_col) %in% names(scope))) {

      scope_sel <- scope_sel %>%
        mutate({{ select_col }} := as.character(str_glue(
          'IF({dims_answer} = "{stdatadictEnv$i18n_dd$t("select_yes")}","X","")'
        )))
    }

    # convert select column entry to formula
    class(scope_sel[[select_col]]) <-
      c(class(scope_sel[[select_col]]), "formula")

    # add named region for answer cell (it may be used by the user for custom
    # selection rules)
    wb$add_named_region(dims = dims_answer, name = paste0("all_", region_name))
  }

  # add scope table
  wb$add_data(sheet = sheet, x = scope_sel, dims = wb_dims(from_row = row))

  dims_data <- wb_dims(x = scope_sel, select = "data", from_row = row)

  # add table head_cell style
  wb$set_cell_style(
    dims = wb_dims(x = scope_sel, select = "col_names", from_row = row),
    style = wb$styles_mgr$get_xf_id("table_head")
  )
  # add scopes column cell style
  wb$set_cell_style(
    dims = wb_dims(x = scope_sel, cols = 1, from_row = row),
    style = wb$styles_mgr$get_xf_id("form_sheet_table_col")
  )

  # wrap text in table
  wb$add_cell_style(
    dims = dims_data,
    wrap_text = TRUE,
    vertical = "top"
  )

  # Add styles for select column
    select_col_idx <- which(colnames(scope_sel) == select_col)

    # select_column_head
    wb$set_cell_style(
      dims = paste0(int2col(select_col_idx),row),
      style = wb$styles_mgr$get_xf_id("select_column_head")
    )
    # select_column
    wb$set_cell_style(
      dims = wb_dims(x = scope_sel, cols = select_col, from_row = row),
      style = wb$styles_mgr$get_xf_id("select_column")
    )

  # add named region
  wb$add_named_region(sheet = sheet,
                      dims = dims_data,
                      name = region_name)


  ### -------- Add Scopes to VarSelect Settings --------

  # adding data to the "VarSelect Settings" sheet, changes the state, of
  # current_sheet(). So we need to save it and restore it later.
  current_sheet <- wb$.__enclos_env__$private$current_sheet

  # Add a named field for every Scope that indicates with a boolean, if
  # the related Scope, or option is marked/chosen or not.

  # Identify Last row that contains content in "VarSelect Settings" sheet
  settings_row_idx <- get_last_sheet_cell(wb, "VarSelect Settings") %>%
    str_extract("[[:digit:]]+") %>%
    as.numeric() + 1

  # If it is the first Scope setting, add a title
  if ("scope_settings_title" %notin% wb$get_named_regions()[["name"]]) {
    settings_row_idx <- settings_row_idx + 1

    wb$add_data(
      sheet = "VarSelect Settings",
      x = "Variable Scope Settings",
      start_row = settings_row_idx
    )

    # bug: If adding a name for the region, the region is added to current_sheet()
    wb$add_named_region(sheet = "VarSelect Settings",
                        name = "scope_settings_title",
                        dims = str_glue("A{settings_row_idx}"))

    wb$add_named_style(dims = str_glue("A{settings_row_idx}"), name = "Title")

    settings_row_idx <- settings_row_idx + 1
  }

  # Add table with Scope name and selection indicator
  scope_settings <- scope_sel %>%
    mutate(selected = as.character(str_glue(
      'TRIM(INDEX({region_name},{row_number()},{match(select_col,names(.))}))<>""'
    ))) %>%
    select("Scope", "selected")

  # convert select column entry to formula
  class(scope_settings[["selected"]]) <-
    c(class(scope_settings[["selected"]]), "formula")

  wb$add_data(sheet = "VarSelect Settings",
              x = scope_settings,
              start_row = settings_row_idx,
              col_names = FALSE)

  # add named regions for each Scope Setting / Boolean value
  scope_settings$Scope %>%
    iwalk(~ wb$add_named_region(
      sheet = "VarSelect Settings",
      name = clean_region_name(.x),
      dims = str_glue("B{settings_row_idx - 1 + .y}")
    ))

  # restore current sheet state
  wb$.__enclos_env__$private$current_sheet <- current_sheet

  invisible(wb)
}
