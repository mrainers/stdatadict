#' Create a Scope Table
#'
#' Create a table that defines variable scopes. This table can be used for
#' adding a "Scope"-Column (and Selection Rules) to the form item tables and
#' Scope-Selection-Tables in the workbook.
#'
#' @param data a data frame containing all variable identifiers (join keys) that
#'    belong to the scope.
#' @param scope (character) name/ID of the Scope.
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
create_scope_table <- function(data, scope, ...) {
  tibble(Scope = scope) %>%
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
