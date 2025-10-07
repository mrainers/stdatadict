#' "Not in" binary operator
#'
#' \code{\%nin\%} is a binary operator, which returns a logical vector indicating
#' if there is a match or not for its left operand. A true vector element
#' indicates no match in left operand, false indicates a match.
#'
#' It is the negation of %in% so \code{x \%notin\% y} is equivalent to \code{!(x \%in\% y)}.
#'
#' @param x vector: the values to be matched.
#' @param y vector: the values to be matched against.
#'
#' @return logical vector, indicating if a match was not found or each element
#' of x: thus the values are TRUE or FALSE and never NA.
#'
#' @seealso \code{\link{match}} \code{\link{\%in\%}}
#'
#' @examples
#' c('a','b','c') %notin% c('a','b')
#'
#' @noRd
`%notin%` <- function(x, y) {
  !(x %in% y)
}


#' Check If An Object Is A Natural Number
#'
#' @param x single object to be checked
#' @param incl_zero logical: should "0" included as natural number?
#'     Default = TRUE
#' @return logical
#' @noRd
is.natural <- function(x, incl_zero = TRUE) {
  if(!is.numeric(x))     return(FALSE)
  if(x %% 1 != 0)        return(FALSE)
  if(x < 0)              return(FALSE)
  if(!incl_zero & x < 1) return(FALSE)
  TRUE
}

#' Get Project ID
#'
#' Extract the project id used in the meta data from the formtable names column.
#' The project id is needed in further steps, in order to clear the form table
#' names from the "mnp"name clutter various meta data columns.
#'
#' @param .data a meta data table that contains the variable "formtablename"
#'     usually this would be "fs" (forms).
#'
#' @return single character string of the studyid that is used in the meta data.
#' @noRd
get_project_id <- function(data) {
  # check if the variable formtablename is in the data
  if (!("formtablename" %in% names(data))) {
    stop("No Variable named \"formtablename\" in data")
  }

  project_id_formtablenames <- data %>%
    distinct(.data$formtablename) %>%
    pull("formtablename") %>%
    str_subset("mnp") %>%
    str_remove("^e?mnp")

  # extract project_id
  str_end <- 1
  id_not_found <- TRUE
  project_id <- NULL

  while (id_not_found) {
    project_id <- str_sub(project_id_formtablenames[[1]], 1, str_end)
    str_end <- str_end + 1
    id_new <- str_sub(project_id_formtablenames[[1]], 1, str_end)
    # only allow letters and digits
    if (str_detect(id_new, "[:^alnum:]$")) break
    id_not_found <- all(str_starts(project_id_formtablenames, id_new))
  }

  message(paste("detected project_id:", project_id))

  project_id
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


# Utils copied from toTools ----

#' Guess the table separator from a table header
#'
#' @param header First line/header of a table as character string
#' @param quote Single character used to quote strings. If NULL (default),
#' the first character of the string is concidered as the quote symbol.
#' @return character string used as separator
#' @noRd
guess_delim <- function(header, quote = NULL) {
  quote <- quote %||% substr(header, 1, 1)

  # remove everything within quotes
  header_unquote <- stringr::str_remove_all(header, paste0(quote, ".*?", quote))

  possible_sep <- c(",", "'", ";", "\t", "@", "\"")

  possible_sep[str_detect(header_unquote, possible_sep)]
}

#' Repair names with modified "unique" method
#'
#' Under the hood this function calls [vctrs::vec_as_names()] with the
#' repair parameter set to "unique_quiet" but when a name occurs multiple times,
#' the first mention isn't changed.
#' @param names character vector of column names
#' @return character vector with unique and non-empty entries.
#' @noRd
totools_name_repair <- function (names) {
  if (length(names) == 0) {
    return(character())
  }
  # missings/empty and duplicated names
  repair_me <- names %in% c("", NA) | duplicated(names)

  names[repair_me] <- vctrs::vec_as_names(names, repair = "unique_quiet")[repair_me]
  names
}
