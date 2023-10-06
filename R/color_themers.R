#' Determine Font Color Depending On Brightness Of Background Color
#'
#' @param bg_color String, either a color name known by R, or the hex rgb code
#'     of the background color.
#'
#' @return String of the font color name. This will be "white" for dark
#'     background colors and "black" for light background colors.
#' @noRd
determine_font_color <- function(bg_color) {
 bg_color %>%
    map( ~ str_replace(.x, "auto", "white")) %>%
    grDevices::col2rgb() %>%
    asplit(MARGIN = 2) %>%
    map(
      ~ {
        luminance = (.x[["red"]] * 0.299 +
                       .x[["green"]] * 0.587  +
                       .x[["blue"]] * 0.114
        ) / 255
        if (luminance > 0.57) return("black") else return("white")
      }
    )
}


#' Tint or shade a color
#'
#' Calculate a brighter or darker version of the original color.
#'
#' @param color String, either a color name known by R, or the hex rgb code
#'     of the background color.
#' @param tint numeric value between -1 and +1. Negative values darkens and
#'     positive values brightens the original color.
#'
#' @return a hex rgb code as string.
#' @export
#'
#' @examples
#' tint_color("purple", 0.4)
#' tint_color("#FFD530", -0.7)
tint_color <- function(color, tint) {
  if(color == "auto") return(color)

  if(!(dplyr::between(tint, -1, 1))) {
    stop("tint must be a value between -1 and +1")
  }

  color %>%
    grDevices::col2rgb() %>%
    .[,1] %>%
    map(~ round(.x + abs((tint > 0) * 255 - .x) * tint)) %>%
    rlang::exec(grDevices::rgb, !!!., maxColorValue = 255)
}


#' List All Available Color Theme Names for Data Dictionary Creation.
#'
#' @return vector of available color theme names, invisible
#' @export
#'
#' @examples ls_color_themes()
ls_color_themes <- function() {
  cat(paste0("Available Data Dictionary color themes: \n",
             "Use 'show_color_theme()' to see the themes colors.\n\n"))

  names(stdatadictEnv$color_themes) %>%
    purrr::walk(~ cat(paste("*", .x, "\n")))

  invisible(names(stdatadictEnv$color_themes))
}

#' Get Data Dictionary Color Theme
#'
#' @param name of color theme. Use [ls_color_themes()] to list all currently
#'     available themes.
#'
#' @return named list with the colors defined in the color theme
#' @export
#'
#' @examples get_color_theme("purpur")
get_color_theme <- function(name) {
  if(!(name %in% names(stdatadictEnv$color_themes))) {
    stop(paste0("There is no color theme with the name ", name, "."))
  }

  stdatadictEnv$color_themes[[name]]
}

#' Show Color Theme
#'
#' @param name of color theme. Use [ls_color_themes()] to list all currently
#'     available themes.
#'
#' @return name parameter of the function call invisibly
#' @export
#'
#' @examples
#' show_color_theme("purpur")
show_color_theme <- function(name) {
  if(!(name %in% names(stdatadictEnv$color_themes))) {
    stop(paste0("There is no color theme with the name ", name, "."))
  }

  theme_description <- list(
    bg_color_primary             = "background for document and form titles ",
    font_color_primary           = "font color for document and form titles",
    bg_color_subtitle            = "background subtitle",
    font_color_subtitle          = "fontcolor subtitle",
    bg_color_h1                  = "background heading",
    font_color_h1                = "fontcolor heading",
    bg_color_tablehead           = "background column names",
    font_color_tablehead         = "fontcolor column names",
    bg_color_visit               = "background visit names",
    font_color_visit             = "fontcolor visit names",
    bg_color_tablecol_overview   = "background table names in form overview",
    font_color_tablecol_overview = "fontcolor table names in form overview",
    bg_color_tablecol_items      = "background table names in form items",
    font_color_tablecol_items    = "fontcolor table names in form items",
    border_color_tablecol_items  = "border table names in form items",
    font_color_hidden            = "fontcolor hidden forms and items"
  )

  color_theme <- stdatadictEnv$color_themes[[name]]

  colortable <- tibble::tibble(name = names(color_theme),
                               description = unlist(theme_description),
                               color = unlist(color_theme))

  if (rlang::is_installed("kableExtra")) {
    colortable %>%
      kableExtra::kbl(caption = paste0("Color Theme: ", name)) %>%
      kableExtra::kable_paper(
        full_width = F,
        html_font = "\"Source Sans Pro\", helvetica, sans-serif"
      ) %>%
      kableExtra::column_spec(
        column = 2:3, background = colortable$color,
        color = determine_font_color(colortable$color)
      ) %>%
      print()
  } else {
    message(paste0(
      'Install the "kableExtra" package, to see a colored table in the view pane.\n',
      "If you use Rstudio version >= 2023.09.0, you can copy paste the following ",
      "table into the editor, to see a preview of the colors."))

    cat(paste0("\nColor Theme: ", name, "\n\n"))

    colortable %>%
      data.frame() %>%
      print(row.names = FALSE, quote = TRUE, right = FALSE)
  }

  invisible(name)
}


#' Set Color Theme That's Gonna Be Used For The Data Dictionary
#'
#' @param name of color theme. Use [ls_color_themes()] to list all currently
#'     available themes.
#'
#' @return invisibly the name parameter of this function call
#' @export
#'
#' @examples use_color_theme("purpur")
use_color_theme <- function(name) {
  if(!(name %in% names(stdatadictEnv$color_themes))) {
    stop(paste0("There is no color theme with the name ", name, "."))
  }

  stdatadictEnv$use_color_theme <- name

  invisible(name)
}

#' Create a New Data Dictionary Color Theme
#'
#' @description
#' Define a new color theme for the data dictionary workbook, based on the
#' color definition from a template theme. You only have to define those
#' parameters that deviate from that template.
#'
#' The newly defined color theme will be added to the list of available themes
#' and if `use_theme = TRUE` use the new theme for creating the data dictionary
#' excel workbook.
#'
#'  Use [ls_color_themes()] to list all currently available themes.
#'
#' @param name of the new color theme
#' @param template name of the color theme from which the new theme inherits
#'     color values. The default is "purpur".
#' @param use_theme logical. Should the newly defined theme used for the workbook?
#'     Default = TRUE
#' @param bg_color_primary character color name or RGB hex code:
#'     background for document and form titles
#' @param font_color_primary character color name or RGB hex code:
#'     font color for document and form titles
#' @param bg_color_subtitle character color name or RGB hex code:
#'     background subtitle
#' @param font_color_subtitle character color name or RGB hex code:
#'     font color subtitle
#' @param bg_color_h1 character color name or RGB hex code:
#'     background heading
#' @param font_color_h1 character color name or RGB hex code:
#'     font color heading
#' @param bg_color_tablehead character color name or RGB hex code:
#'     background column names
#' @param font_color_tablehead character color name or RGB hex code:
#'     font color column names
#' @param bg_color_visit character color name or RGB hex code:
#'     background visit names
#' @param font_color_visit character color name or RGB hex code:
#'     font color visit names
#' @param bg_color_tablecol_overview character color name or RGB hex code:
#'     background table names in form overview
#' @param font_color_tablecol_overview character color name or RGB hex code:
#'     font color table names in form overview
#' @param bg_color_tablecol_items character color name or RGB hex code:
#'     background table names in form items
#' @param font_color_tablecol_items character color name or RGB hex code:
#'     font color table names in form items
#' @param border_color_tablecol_items character color name or RGB hex code:
#'     border table names in form items
#' @param font_color_hidden character color name or RGB hex code:
#'     font color hidden forms and items
#'
#' @return new color theme as named list, invisibly
#'
#' @seealso [use_color_theme()] [ls_color_themes()]
#' @export
#'
#' @examples
#' add_color_theme("my_theme",
#'                 template = "purpur",
#'                 bg_color_primary = "steelblue4",
#'                 font_color_subtitle = "steelblue4",
#'                 bg_color_h1 = "steelblue2",
#'                 font_color_h1 = "#111188")
add_color_theme <- function(
    name,
    template = "purpur",
    use_theme = TRUE,
    bg_color_primary = NULL,
    font_color_primary = NULL,
    bg_color_subtitle = NULL,
    font_color_subtitle = NULL,
    bg_color_h1 = NULL,
    font_color_h1 = NULL,
    bg_color_tablehead = NULL,
    font_color_tablehead = NULL,
    bg_color_visit = NULL,
    font_color_visit = NULL,
    bg_color_tablecol_overview = NULL,
    font_color_tablecol_overview = NULL,
    bg_color_tablecol_items = NULL,
    font_color_tablecol_items = NULL,
    border_color_tablecol_items = NULL,
    font_color_hidden = NULL
    ) {
  template_theme <- get_color_theme(template)
  # get color parameters from function call
  color_args <- as.list(match.call(expand.dots=FALSE)) %>%
    purrr::keep_at(names(template_theme))

  # replace colors in template with colors from function call
  new_theme <- utils::modifyList(template_theme, color_args)

  # add new color theme to list of available themes
  stdatadictEnv$color_themes[[name]] <- new_theme

  if (use_theme) use_color_theme(name)

  invisible(new_theme)
}

