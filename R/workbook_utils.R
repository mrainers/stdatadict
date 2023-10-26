#' Get Index Of The Last Cell Of A Sheet
#'
#' Get the most right down cell in a worksheet sheet with content.
#'
#' @param wb a wbWorkbook object.
#' @param sheet name or index of the Worksheet
#'
#' @return vector with column and row of the last cell containing content.
#' @noRd
get_last_sheet_cell <- function(wb, sheet = current_sheet()) {

  sheetno <- wb$validate_sheet(sheet)

  dims <- unname(unlist(openxlsx2::xml_attr(
    wb$worksheets[[sheetno]]$dimension,
    "dimension"
  )))

  stringr::str_split_1(dims, ":")[2]
}

#' Creates a wbColour Object
#'
#' Wrapper function for wb_color in openxlsx2
#'
#' @param name A name of a color known to R or RGB-Hexcode or "auto"
#'
#' @return a wbColour object
#' @noRd
wb_color <- function(name) {
  if (name == "auto") {
    return(openxlsx2::wb_color(auto = TRUE))
  } else {
    return(openxlsx2::wb_color(name))
  }
}



# Verfübare Breite für Text area
# t_area_width <- wb$worksheets[[1]]$unfold_cols() %>%
#   filter(customWidth == 1) %>%
#   filter(min <= last_col) %>%
#   pull(width) %>%
#   {sum(as.numeric(.)) + (last_col - length(.)) * default_colwidth}

#' Calculate Width Of Column (Range)
#'
#' @param wb a wbWorkbook object
#' @param sheet worksheet name or index, default = current_sheet()
#' @param cols single column or column range. Examples: 2, 2:5, "B", "B:D"
#'
#' @return (numeric) total column with of columns in column range (cols param).
#' @noRd
calc_colswidth <- function(wb, sheet = current_sheet(), cols = 1) {
  sheet_order <- openxlsx2::wb_get_order(wb)
  sheet_names <- wb$get_sheet_names()

  if (isa(sheet, "openxlsx2_waiver")) {
    sheet_idx <- wb$.__enclos_env__$private$current_sheet
  } else if (is.character(sheet)) {
    sheet_idx <- match(sheet, sheet_names)
  }

  if (!(sheet_idx %in% 1:length(sheet_order))) {
    stop("'sheet' must be the name of an already existing sheet or an existing
         sheet number.")
  }

  cols <- col2int(cols)

  col_info <- wb$worksheets[[sheet_idx]]$unfold_cols()

  custom_col_widths <- col_info %>%
    filter(.data$customWidth == 1,
           .data$min %in% cols) %>%
    pull("width") %>%
    as.numeric()

  col_n <- length(cols)

  default_colwidth <- wb$worksheets[[sheet_idx]]$sheetFormatPr %>%
    openxlsx2::xml_attr("sheetFormatPr") %>%
    { .[[1]][["baseColWidth"]] } %>%
    as.numeric()

  colwidth_combined <- sum(custom_col_widths) +
    default_colwidth * (col_n - length(custom_col_widths))

  colwidth_combined
}


#' Calculate number of lines needed to display a text
#'
#' @param text (character) that will be added to the workbook
#' @param pwidth (numeric) Width of the paragraph. This might be the width of a
#'     cell or the with of merged cells in which the text will be added.
#' @param font (character) font name, Default: Calibri
#' @param fsize (integer) The font size: Default: 11
#' @param font_weight (numeric) correction factor if the estimated number of
#'     needed lines is off. Default: 0.86
#'
#' @return (numeric) the number of lines needed to display the text
#' @noRd
calc_lines <- function(text,
                       pwidth,
                       font = "Calibri",
                       fsize = 11,
                       font_weight = 0.86) {
  fw <- system.file("extdata", "fontwidth/FontWidth.csv", package = "openxlsx2")
  font_width_tab <- utils::read.csv(fw)

  # get the width of one digit for the font type ans size
  digit_width <- font_width_tab %>%
    filter(.data$FontFamilyName == font,
           .data$FontSize == fsize) %>%
    pull("Width")

  # Width of the text if printed in one line
  # got the formular from:
  # https://learn.microsoft.com/en-us/dotnet/api/documentformat.openxml.spreadsheet.column?view=openxml-2.8.1
  text_width <- trunc((stringr::str_width(text) * digit_width * font_weight + 5) /
                        digit_width * 256) / 256

  # calculate and return the number of lines needed to display the text.
  ceiling(text_width / pwidth)
}


#' Get Attributes of a font style
#'
#' @param wb a wbWorkbook object
#' @param name (character) the name of the font style
#'
#' @return list of named characters, containing the font attributes
#' @noRd
get_font_attributes <- function(wb, name) {
  if (name %notin% wb$styles_mgr$font$name) stop(paste(name, "font style not found."))

  style_idx <- which(wb$styles_mgr$font$name == name)

  font_style_xml <- wb$styles_mgr$styles$fonts[[style_idx]]
  font_style_nodes <- openxlsx2::xml_node_name(font_style_xml, "font")

  font_style_nodes %>%
    map(~ openxlsx2::xml_attr(font_style_xml, "font", .x)[[1]]) %>%
    purrr::set_names(font_style_nodes)
}
