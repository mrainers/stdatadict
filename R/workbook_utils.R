get_last_sheet_cell <- function(wb, sheet = current_sheet()) {

  sheetno <- wb$validate_sheet(sheet)

  dims <- unname(unlist(openxlsx2::xml_attr(
    wb$worksheets[[sheetno]]$dimension,
    "dimension"
  )))

  stringr::str_split_1(dims, ":")[2]
}
