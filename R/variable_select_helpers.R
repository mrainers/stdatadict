add_varselect_settings <- function(wb) {
  insert_worksheet(wb, sheet = "VarSelect Settings")

  row_idx = 1

  # add Yes or No option list
  wb$add_data(x = "Yes/No Options", dims = "A1")
  wb$add_named_style(dims = "A1", name = "Heading 4")

  wb$add_data(x = stdatadictEnv$i18n_dd$t("select_yes"), dims = "A2")
  wb$add_data(x = stdatadictEnv$i18n_dd$t("select_no"), dims = "A3")

  wb$add_named_region(dims = "A2:A3", name = "YesNo")


  invisible(wb)
}
