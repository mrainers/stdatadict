#' stdatadict Environment For Intermediate Calculation Results
#'
#' List the objects currently stored inside this environment:
#'     `s(stdatadict::intermediates, all.names = TRUE)`
#' Access object from this environment:
#'     `stdatadict::intermediates$<ObjName>`
#' @export
intermediates <- new.env(parent = emptyenv())


### UTILS ----------------------------------------------------------------------

#' Get Study ID
#'
#' Extract the study id used in the meta data from the formtable names column.
#' The study id is needed in further steps, in order to clear the form table
#' names from the "mnp"name clutter various meta data columns.
#'
#' @param .data a meta data table that contains the variable "formtablename"
#'     usually this would be "fs" (forms).
#'
#' @return single character string of the studyid that is used in the meta data.
#' @noRd
get_studyid <- function(data) {
  # check if the variable formtablename is in the data
  if (!("formtablename" %in% names(data))) {
    stop("No Variable named \"formtablename\" in data")
  }

  studyid_formtablenames <- data %>%
    distinct(.data$formtablename) %>%
    pull("formtablename") %>%
    str_subset("mnp") %>%
    str_remove("^e?mnp")

  # extract studyid
  str_end <- 1
  id_not_found <- TRUE
  studyid <- NULL

  while (id_not_found) {
    studyid <- str_sub(studyid_formtablenames[[1]], 1, str_end)
    str_end <- str_end + 1
    id_new <- str_sub(studyid_formtablenames[[1]], 1, str_end)
    # only allow letters and digits
    if (str_detect(id_new, "[:^alnum:]$")) break
    id_not_found <- all(str_starts(studyid_formtablenames, id_new))
  }

  message(paste("detected studyid:", studyid))

  studyid
}

#' Get Secutrials Forms Order
#'
#' Get the order of the main forms how they appear in the "Datensatztabelle" and
#' should also should kept for the DataDictionary. This can be applied after
#' some data wrangling that destroys the original formorder.
#'
#' @param forms the refined fs dataset from secutrials meta data
#'
#' @return tibble with column "formtablename" sorted in it's original order.
#' @noRd
get_formorder <- function(forms) {
  forms %>%
    filter(!.data$is_subform) %>%
    distinct(.data$formtablename)
}

### REFINE META DATA TABLES ----------------------------------------------------

#'  Prepare vp/visitplan Table For Further Data Dictionary Calculation Steps.
#'
#' The preparation procedure includes the following steps:
#' - Entries from older study versions are removed.
#' - not needed variables are removed.
#'
#' @param vp table from secutrial metadata
#'
#' @return tibble with cleaned visit data
#' @noRd
refine_vp <- function(vp) {
  # Retrieve visit structure information
  vp %>%
    mutate(mnpvsno = replace_na(as.numeric(.data$mnpvsno), -1)) %>%
    filter(.data$mnpvsno == max(.data$mnpvsno)) %>%
    select("mnpvisid", "mnpvislabel", "hidden")
}


#' Prepare forms/fs Table For Further Data Dictionary Calculation Steps.
#'
#' The preparation procedure includes the following steps:
#' - Entries from older study versions are removed.
#' - a boolean indicator variable if a form is a subform is added.
#' - a variable that estimates the form type (visit, casenode, subform) is added.
#' - formtablename entries are cleared from the '(e)mnp<studyid>' string.
#'
#' @param fs table from secutrial metadata
#' @param vpfs table from secutrial metadata
#' @param studyid character string extracted from 'formtablename' variable that
#'     is the study name in the secutrial setup and a common string in all
#'     formtablenames that needs to be removed.
#'
#' @return tibble with cleaned fs data
#' @noRd
refine_fs <- function(fs, vpfs, studyid) {
  fs %>%
    mutate(mnpvsno = replace_na(as.numeric(.data$mnpvsno), -1)) %>%
    filter(.data$mnpvsno == max(.data$mnpvsno)) %>%
    mutate(is_subform = str_detect(.data$formtablename, "^emnp")) %>%
    mutate(formtablename = str_remove(.data$formtablename, str_c("mnp", studyid))) %>%
    mutate(formtype = dplyr::case_when(
      formid %in% vpfs$formid ~ "visit",
      .data$is_subform ~ "subform",
      TRUE ~ "casenode"
    )) %>%
    # hidden visit forms are not in the visitplanforms (vpfs), so they are falsely
    # identified as casenode forms. However regular casenode forms appear at the
    # very end of the forms list. So if a hidden 'casenode' form is followed by
    # other visit forms, then we know it is truly a hidden visit form.
    mutate(
      last_formvisit_row = max(if_else(.data$formtype == "visit", dplyr::row_number(), 0)),
      formtype = if_else(
        .data$formtype == "casenode" & dplyr::row_number() < .data$last_formvisit_row,
        "visit", .data$formtype
      )
    ) %>%
    select(-"last_formvisit_row")
}

#' Prepare questions/qs Table For Further Data Dictionary Calculation Steps.
#'
#' The preparation procedure includes the following steps:
#' - Entries from older study versions are removed.
#' - a variable that holds the original question order is added.
#' - (sub)formtablename entries are cleared from the '(e)mnp<studyid>' string.
#'
#' @param qs table from secutrial metadata
#' @param studyid character string extracted from 'formtablename' variable that
#'     is the study name in the secutrial setup and a common string in all
#'     formtablenames that needs to be removed.
#' @param invert_hidden Boolean is the hidden variable inverted? In 2023
#'    secutrial created a bug that inverts the hidden variable in the
#'    qs/question table. If invert_hidden = NULL (default), then the function
#'    will assume that the hidden variable is inverted if more than 70% of all
#'    entries are 1 = hidden.
#'
#' @return tibble with cleaned qs data
#' @noRd
refine_qs <- function(qs, studyid, invert_hidden = NULL) {
  if (is.null(invert_hidden)) {
    # assume that the hidden variable is inverted, if more than 70% of all
    # questions are hidden.
    hidden_pct <- 100 * sum(qs$hidden, na.rm = TRUE) / length(qs$hidden)
    invert_hidden <- hidden_pct > 70
    if (invert_hidden) {
      message(paste0(
        round(hidden_pct, digits = 1), "% of the questions are marked as hidden.\n",
        "It seems that the hidden variable is inverted by a secutrial bug.\n",
        "-> The hidden variable will be reverted for the data dictionary."
        ))
    }
  }

  # invert hidden variable if necessary.
  if (invert_hidden) qs$hidden <- dplyr::if_else(qs$hidden == 1, NA, 1, 1)

  qs %>%
    mutate(order = dplyr::row_number()) %>%
    group_by(.data$formtablename) %>%
    dplyr::slice_min(.data$formid) %>%
    ungroup() %>%
    arrange(.data$order) %>%
    mutate(
      is_subform = str_detect(.data$formtablename, "^emnp"),
      formtablename = str_remove(.data$formtablename, str_c("mnp", studyid)),
      subformtablename = str_remove(.data$subformtablename, str_c("mnp", studyid))
    )
}

#' Prepare items/is Table For Further Data Dictionary Calculation Steps.
#'
#' The preparation procedure includes the following steps:
#' - Entries from older study versions are removed.
#' - a variable that holds the original question order is added.
#'
#' @param is table from secutrial metadata
#' @param questions refined qs table from secutrial metadata
#'
#' @return tibble with cleaned items data
#' @noRd
refine_is <- function(is, questions) {
  is %>%
    filter(!is.na(.data$ffcolname)) %>%
    mutate(order = dplyr::row_number()) %>%
    arrange(.data$order) %>%
    # this removes items from older study versions
    dplyr::semi_join(questions, by = "fgid")

}


#' Prepare cl (Variable Labels) Table For Further Data Dictionary Calculation Steps.
#'
#' The preparation procedure includes the following steps:
#' - Entries that are not actual variable labels are removed.
#' - formtable names are cleaned from "mnp<studyid>" strings.
#' - item identifiers are split into tablename and varname variables.
#' - all value codes and labels of one item are collapsed into a single string (row)
#'
#' @param cl table from secutrial metadata
#' @param studyid character string extracted from 'formtablename' variable that
#'     is the study name in the secutrial setup and a common string in all
#'     formtablenames that needs to be removed.
#'
#' @return tibble with item identifiers and combined item labels
#' @noRd
refine_cl <- function(cl, studyid) {
  cl %>%
    filter(str_detect(.data$column, str_c("mnp", studyid))) %>%
    mutate(column = str_remove(.data$column, str_c("mnp", studyid))) %>%
    mutate(
      tablename = str_extract(.data$column, "^[^\\.]*") %>%
        str_remove(str_c("mnp", studyid)),
      varname = str_extract(.data$column, "[^\\.]*$")
    ) %>%
    mutate(vallab = str_c(.data$code, " = ", .data$value)) %>%
    # order values (negative values at the end)
    mutate(
      positive = .data$code >= 0,
      abscode = abs(.data$code)
    ) %>%
    arrange(
      .data$tablename, .data$varname, dplyr::desc(.data$positive),
      .data$abscode
    ) %>%
    group_by(.data$tablename, .data$varname) %>%
    summarise(vallabs = str_c(.data$vallab, collapse = ", ")) %>%
    ungroup()
}


### CREATE DATADICT TABLES -----------------------------------------------------

#' Create Visit Form Table
#'
#' @param forms the refined "fs" data table from the secutrial export
#' @param visits the refined "vp" data table from the secutrial export
#' @param vpfs the "vpfs" data table from the secutrial export
#'
#' @return tibble that consists of all visit forms/tables (rows) and
#' visit names (colums).
#' An 'X' in the cells marks, which forms are available at which visits.
#' @noRd
create_visitforms <- function(forms, visits, vpfs) {
  forms %>%
    filter(.data$formtype == "visit") %>%
    left_join(vpfs %>% select(-any_of("hidden")), by = "formid") %>%
    left_join(visits %>% select(-"hidden"), by = "mnpvisid") %>%
    mutate(dummy = "X") %>%
    tidyr::pivot_wider(
      id_cols = c("formtablename", "formname", "hidden"),
      names_from = "mnpvislabel",
      values_from = "dummy"
    ) %>%
    select(-any_of("NA")) %>%
    mutate(across(4:last_col(), replace_na, "-")) %>%
    rename(!!stdatadictEnv$i18n_dd$t("table_col")   := "formtablename",
           !!stdatadictEnv$i18n_dd$t("formname_col") := "formname"
           )
}

#' Create Table of Casenode Forms
#'
#' @param forms the refined "fs" data table from the secutrial export
#'
#' @return tibble that lists all casenode forms
#' An 'X' in the cells marks, which forms are available at which visits.
#' @noRd
create_casenodeforms <- function(forms) {
  forms %>%
    filter(.data$formtype == "casenode") %>%
    select("formtablename", "formname", "hidden") %>%
    rename(!!stdatadictEnv$i18n_dd$t("table_col")    := "formtablename",
           !!stdatadictEnv$i18n_dd$t("formname_col") := "formname"
    )
}

#' Create Table of Subforms
#'
#' @param questions the refined "qs" data table from secutrial export
#'
#' @return tibble that lists all main forms, that contain sub forms and which
#' sub form those main forms include.
#' @noRd
create_subforms <- function(questions) {
  questions %>%
    filter(!is.na(.data$subformtablename)) %>%
    select(
      !!stdatadictEnv$i18n_dd$t("mainform_col")      := "formtablename",
      !!stdatadictEnv$i18n_dd$t("formname_col")      := "formname",
      !!stdatadictEnv$i18n_dd$t("subform_table_col") := "subformtablename",
      !!stdatadictEnv$i18n_dd$t("subform_name_col")  := "fglabel",
           "hidden"
    )
}


#' Create Tables of form items
#'
#' @param questions the refined "qs" data table from secutrial export
#' @param items the refined "is" data table from secutrial export
#' @param vallabs the refined "cl" data table from secutrial export
#'
#' @return list of tibbles that represent a form with their containing
#'     variables, each.
#' @noRd
create_formitems <- function(questions, items, vallabs) {
  # only mainform questions
  mainform_questions <- questions %>%
    filter(!.data$is_subform)

  # only subform questions
  subform_questions <- questions %>%
    filter(.data$is_subform) %>%
    arrange(.data$formtablename, .data$sequence)

  # substitute rows that marks the placement of a subtable (the column
  # subformtablename holds the name of the subform), with the corresponding
  # subform questions

  questions_substited <- mainform_questions %>%
    left_join(subform_questions,
      by = c("subformtablename" = "formtablename"),
      relationship = "many-to-many"
    ) %>%
    mutate(
      across(
        ends_with(".x"),
        ~ coalesce(get(str_replace(dplyr::cur_column(), ".x", ".y")), .),
        .names = "{str_remove(.col, '.x')}"
      )
    ) %>%
    select(-ends_with(".x"), -ends_with(".y")) %>%
    mutate(
      mainform = .data$formtablename,
      formtablename = coalesce(.data$subformtablename, .data$formtablename)
    ) %>%
    relocate("mainform")


  # table with all questions and their items and properties
  formitems_combined <- questions_substited %>%
    select("fgid", "mainform", "formtablename", "fglabel") %>%
    inner_join(
      items %>%
        select("fgid", "fflabel", "ffcolname", "itemtype", "hidden"),
      by = "fgid",
      relationship = "many-to-many"
    ) %>%
    select(-"fgid") %>%
    left_join(vallabs,
      by = c("formtablename" = "tablename", "ffcolname" = "varname")
    ) %>%
    rename(
      !!stdatadictEnv$i18n_dd$t("table_col") := "formtablename",
      !!stdatadictEnv$i18n_dd$t("question_col") := "fglabel",
      !!stdatadictEnv$i18n_dd$t("varlab_col") := "fflabel",
      !!stdatadictEnv$i18n_dd$t("varname_col") := "ffcolname",
      !!stdatadictEnv$i18n_dd$t("vartype_col") := "itemtype",
      !!stdatadictEnv$i18n_dd$t("values_col") := "vallabs"
    )

  # split by mainform
  formitems_combined %>%
    tidyr::nest(.by = .data$mainform) %>%
    tibble::deframe()
}

### MAIN -----------------------------------------------------------------------


#' Create Data Dictionary Tables
#'
#' Create the following tables for use in the data dictionary file:
#' - visit form overview
#' - casenode forms overview
#' - subform (repetition table) overview
#' - itemtables for each form
#'
#' All interim calculations such as the refined meta data tables or the
#' extracted study id are saved into the [intermediates] environment of this
#' package. Type:
#'    `s(stdatadict::intermediates, all.names = TRUE)`
#' to get a list of all objects stored in this environment.
#' Access object from this environment:
#' `stdatadict::intermediates$<ObjName>`.
#'
#' @param st_metadata list of dataset that contains the secutrial meta data
#'     tables.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> additional options to pass to
#'     internal functions.<br>
#'     - `invert_hidden` Boolean, define if the "hidden" variable in the
#'       question/qs file is inverted by a secutrial bug.<br>
#'       If this option is omitted, this function assumes that the bug is active
#'       when more than 70% of all questions are marked as hidden.
#'
#' @return nested list of tibbles with the following structure:
#'
#'     $ form_overview
#'      ...$ visit_forms
#'      ...$ casenode_forms
#'      ...$ sub_forms
#'     $ form_items
#'      ...$ <form1>
#'      ...$ <form2>
#'      ...
#' @export
#'
#' @examplesIf interactive()
#' datadict_raw <- create_datadict_tables(st_metadata)
create_datadict_tables <- function(st_metadata, ...) {
  invert_hidden = list(...)$invert_hidden

  datadict_tables <- list()

  # TODO: check if all needed metadata is in the tables if data is exported without the
  # "Dupliziere Formular-Metadaten in alle Tabellen" option.

  # extract study id from forms
  studyid <- get_studyid(st_metadata$fs)
  intermediates$studyid <- studyid

  # refine metadata
  visits <- refine_vp(st_metadata$vp)
  intermediates$visits <- visits

  forms <- refine_fs(st_metadata$fs, st_metadata$vpfs, studyid)
  intermediates$forms <- forms

  questions <- refine_qs(st_metadata$qs, studyid, invert_hidden)
  intermediates$questions <- questions

  items <- refine_is(st_metadata$is, questions)
  intermediates$items <- items

  vallabs <- refine_cl(st_metadata$cl, studyid)
  intermediates$vallabs <- vallabs


  # store the order of the major forms (without the subforms)
  form_order <- get_formorder(forms)
  intermediates$form_order <- form_order

  # create form overview tables
  visit_forms <- create_visitforms(forms, visits, st_metadata$vpfs)
  intermediates$visit_forms <- visit_forms
  datadict_tables$form_overview$visit_forms<- visit_forms

  casenode_forms <- create_casenodeforms(forms)
  intermediates$casenode_forms <- casenode_forms
  datadict_tables$form_overview$casenode_forms <- casenode_forms

  sub_forms <- create_subforms(questions)
  intermediates$sub_forms <- sub_forms
  datadict_tables$form_overview$sub_forms <- sub_forms

  form_items <- create_formitems(questions, items, vallabs)
  datadict_tables$form_items <- form_items

  datadict_tables
}

#' Add Data To Form Item Tables
#'
#' @description
#' Wrapper function to join a data set or a list of data sets with the form item
#' tables. It combines all tables in datadict_tables$form_items, and adds the
#' additional data with dplyrs `inner_join()` function.
#'
#' `mainform` is the key variable to match by form name.
#'
#' Keep in mind that the variable names in the form item tables depend on the
#' language settings of this package (Default is "en"). To be independent from
#' the language setting rename your matching key variables as following before
#' performing the join:
#' - `table_col` = Table,
#' - `question_col` = Question,
#' - `varlab_col` = "Variable Label",
#' - `varname_col` = "Variable Name",
#' - `vartype_col` = Type
#'
#' @param datadict_tables table list, generated with [create_datadict_tables()]
#' @param data data frame or list of data framed to be joined with form_item
#'    tables of datadict_tables
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> additional options to pass to
#'     [dplyr::left_join()]
#'
#' @return datadict_tables with joined data to the form item tables
#' @export
#'
#' @examples \dontrun{
#' ## create example data for matching
#' extra_data <- form_items %>%
#'   tibble::enframe(name = "mainform") %>%
#'   tidyr::unnest(cols = c(value)) %>%
#'   select(
#'     mainform,
#'     table_col = Table,
#'     varname_col = "Variable Name") %>%
#'   # add random vargroups
#'   mutate(var_group = sample(
#'     x = c("one", "two", NA),
#'     size = nrow(.),
#'     replace = TRUE,
#'     prob = c(0.2, 0.2, 0.6)
#'   )) %>%
#'   filter(!is.na(var_group))
#'
#' ## add "var_group" to form_items, match by mainform, table and varname
#' datadict_tables <- join_with_form_items(datadict_tables, extra_data)
#' }
join_with_form_items <- function(datadict_tables, data, ...) {
  # if the 'relationship' parameter is not set, set it to "many-to-one"
  dots <- rlang::list2(...)
  if (!("relationship" %in% names(dots))) {
    dots$relationship <- "many-to-one"
  }

  # if data is a single data frame, put it in a list
  if(is.data.frame(data)) data <- list(data)

  # translate columns of data frame
  data_transl <- data %>%
    map(~ .x %>%
          dplyr::rename_with(~ stdatadictEnv$i18n_dd$t(.x) %>%
                               suppressWarnings(),
                             .cols = -any_of("mainform")
          )
    )

  # put all form_item tables together
  form_items_combined <- datadict_tables$form_items %>%
    tibble::enframe(name = "mainform") %>%
    tidyr::unnest(cols = c("value"))

  # join data tables to form items
  form_items_joined <- purrr::reduce(
    .x = data_transl,
    .f = ~ .x %>% rlang::exec("left_join", x = ., y = .y, !!!dots),
    .init = form_items_combined
  )

  # if some data tables have the same column names the join has produced new
  # column names with the appendixes ".x", ".y" etc. Unite those columns into one
  # common variables in data_transl but not in form_items

  # don't unite the names that already exist in form_items, except from "Scope"
  form_item_names <- names(form_items_combined)

  unite_vars <- data_transl %>%
    map(~ names(.) %>% setdiff(form_item_names)) %>%
    unlist() %>%
    tibble::tibble(var = .) %>%
    dplyr::count(.data$var) %>%
    filter(.data$n > 1) %>%
    pull("var") %>%
    append(intersect(form_item_names, "Scope"))

  form_items_joinedclean <- purrr::reduce(
    .x = unite_vars,
    .f = ~ .x %>% tidyr::unite({{ .y }}, starts_with(.y), sep = ", ", na.rm = TRUE),
    .init = form_items_joined
  )

  datadict_tables$form_items <- form_items_joinedclean %>%
    tidyr::nest(.by = .data$mainform) %>%
    tibble::deframe()

  datadict_tables
}
