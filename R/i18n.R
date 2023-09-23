#' Get available Language Setting for Data Dictionary
#'
#' @return character vector with all language codes for the available language
#'     settings of the data dictionary.
#' @export
#'
#' @examples
#' get_datadict_language_codes()
get_datadict_language_codes <- function() {
  stdatadictEnv$available_languages
}

#' Get Language Setting for Data Dictionary
#'
#' @return character. A language code that represents the current language
#'     setting that is used for the data  dictionary.
#' @export
#'
#' @examples
#' get_datadict_language()
get_datadict_language <- function() {
  stdatadictEnv$language
}


#' Set Language of the Data Dictionary
#'
#' @param lang a character string that specifies the language code for the
#'     language of the data dictionary. Get a list of available language codes
#'     with [get_datadict_language_codes()].
#'
#' @return old language option invisibly.
#' @export
#'
#' @examples
#' ## Set the language of the data dictionary to german
#' set_datadict_language("de")
set_datadict_language <- function(lang = c("en", "de", stdatadictEnv$available_languages)) {
  lang <- rlang::arg_match(lang, values = unique(eval(rlang::fn_fmls()[["lang"]])))

  old <- stdatadictEnv$language
  stdatadictEnv$language <- lang
  # update Translator
  stdatadictEnv$i18n_dd$set_translation_language(lang)

  invisible(old)
}

