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
get_study_id <- function(data) {
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
    id_not_found <- all(str_starts(studyid_formtablenames, id_new))
  }

  studyid
}
