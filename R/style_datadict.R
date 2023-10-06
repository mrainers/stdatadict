#' Create And Register Data Dictionary Workbook Styles
#'
#' @param wb a workbook
#' @param theme_name Character Name of the color theme that should be used for the data dictionary
#'
#' @return workbook with added styles, invisibly
#' @export
#'
#' @examples
#' wb <- openxlsx2::wb_workbook()
#' style_datadict(wb)
style_datadict <- function(wb, theme_name = stdatadictEnv$use_color_theme) {
  color_theme <- get_color_theme(theme_name)

  wb_styles <- list(
    title = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_color_primary)
      ),
      font_style = create_font(
        sz = 36,
        b = TRUE,
        color = wb_color(color_theme$font_color_primary),
      ),
      border_style = create_border()
    ),
    subtitle = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_color_subtitle)
      ),
      font_style = create_font(
        sz = 24,
        b = TRUE,
        color = wb_color(color_theme$font_color_subtitle)
      ),
      border_style = create_border()
    ),
    as_of_date = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_color_subtitle)
      ),
      font_style = create_font(
        b = TRUE,
        color = wb_color(color_theme$font_color_subtitle),
      ),
      border_style = create_border()
    ),
    heading_1 = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_color_h1)
      ),
      font_style = create_font(
        sz = 24,
        b = TRUE,
        color = wb_color(color_theme$font_color_h1)
      ),
      border_style = create_border(
        top = "thick", top_color = wb_color(color_theme$bg_color_primary), # maybe the border color should get it's own color definition
        bottom = "thick", bottom_color = wb_color(color_theme$bg_color_primary)
      )
    ),
    text_area = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color("white")
      ),
      font_style = create_font(),
      border_style = create_border()
    ),
    form_overview_section = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_color_primary)
      ),
      font_style = create_font(
        sz = 14,
        b = TRUE,
        color = wb_color(color_theme$font_color_primary)
      ),
      border_style = create_border() # maybe one day add color theme option for border
    ),
    table_head = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_color_tablehead)
      ),
      font_style = create_font(
        b = TRUE,
        color = wb_color(color_theme$font_color_tablehead)
      ),
      border_style = create_border()
    ),
    visit_names = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_color_visit)
      ),
      font_style = create_font(
        b = TRUE,
        color = wb_color(color_theme$font_color_visit)
      ),
      border_style = create_border()
    ),
    table_names = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_color_tablecol_overview)
      ),
      font_style = create_font(
        color = wb_color(color_theme$font_color_tablecol_overview)
      ),
      border_style = create_border()
    )
  )

  # add all fill, font and border styles to the workbook
  wb_styles %>%
    purrr::iwalk(~ map(
      .x,
      ~ wb_add_style(wb, .x, .y),
      name = .y
    ))

  # define cell styles, based on fill, font, border styles and additional
  # style properties
  cell_style_options_a <- wb_styles %>%
    purrr::imap(~ list(
      fill_id = wb$styles_mgr$get_fill_id(.y),
      font_id = wb$styles_mgr$get_font_id(.y),
      border_id = wb$styles_mgr$get_border_id(.y)
    ))

  cell_style_options_b <- list(
    title = list(indent = 1, vertical = "center"),
    subtitle = list(indent = 1),
    as_of_date = list(indent = 1),
    heading_1 = list(horizontal = "center"),
    text_area = list(wrap_text = TRUE),
    form_overview_section = list(horizontal = "center"),
    table_head = list(wrap_text = TRUE, horizontal = "center"),
    visit_names = list(wrap_text = TRUE, horizontal = "center")
  )

  cell_style_options <- cell_style_options_a %>%
    purrr::imap(~ append(.x, cell_style_options_b[[.y]]))

  cell_styles <- cell_style_options %>%
    map(~ rlang::exec(openxlsx2::create_cell_style, !!!.x))

  # add cell styles to the workbook
  cell_styles %>%
    purrr::iwalk(~ wb_add_style(wb, .x, .y))

  invisible(wb)
}
