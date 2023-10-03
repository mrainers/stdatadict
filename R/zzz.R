# environment for package options
stdatadictEnv <- new_environment(list(
  language = "en"
))

.onLoad <- function(libname, pkgname) {
  ## init translation data

  stdatadictEnv$transl_table <- read_csv(
    system.file("extdata", "translations_vartables.csv", package = "stdatadict"),
    col_types = readr::cols()
  )

  stdatadictEnv$available_languages <- names(stdatadictEnv$transl_table) %>%
    setdiff("key")

  stdatadictEnv$transl_jsonfile <- tempfile("translations", fileext = ".json")

  # create temporary json file
  list(languages = names(stdatadictEnv$transl_table),
       translation = stdatadictEnv$transl_table) %>%
    jsonlite::write_json(stdatadictEnv$transl_jsonfile)

  ## init shiny.i18n Translator
  stdatadictEnv$i18n_dd <- shiny.i18n::Translator$new(
    translation_json_path = stdatadictEnv$transl_jsonfile
  )
  stdatadictEnv$i18n_dd$set_translation_language(stdatadictEnv$language)

  # create list of all package color themes
  stdatadictEnv$color_themes <- list(
    purpur = purpur,
    no_colors = no_colors,
    no_colours = no_colors
  )

  stdatadictEnv$use_color_theme <- "purpur"
}
