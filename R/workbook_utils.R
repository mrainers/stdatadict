get_last_sheet_cell <- function(wb, sheet = current_sheet()) {

  sheetno <- wb$validate_sheet(sheet)

  dims <- unname(unlist(openxlsx2::xml_attr(
    wb$worksheets[[sheetno]]$dimension,
    "dimension"
  )))

  stringr::str_split_1(dims, ":")[2]
}

# wrapper function for wb_color in openxlsx2
wb_color <- function(name) {
  if (name == "auto") {
    return(openxlsx2::wb_color(auto = TRUE))
  } else {
    return(openxlsx2::wb_color(name))
  }
}
