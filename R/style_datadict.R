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
    # --- Title ---
    title = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_primary)
      ),
      font_style = create_font(
        sz = 36,
        b = TRUE,
        color = wb_color(color_theme$font_primary),
      ),
      border_style = create_border()
    ),
    # --- Subtitle ---
    subtitle = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_subtitle)
      ),
      font_style = create_font(
        sz = 24,
        b = TRUE,
        color = wb_color(color_theme$font_subtitle)
      ),
      border_style = create_border()
    ),
    # --- as of date ---
    as_of_date = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_subtitle)
      ),
      font_style = create_font(
        b = TRUE,
        color = wb_color(color_theme$font_subtitle),
      ),
      border_style = create_border()
    ),
    # --- heading 1 ---
    heading_1 = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_h1)
      ),
      font_style = create_font(
        sz = 24,
        b = TRUE,
        color = wb_color(color_theme$font_h1)
      ),
      border_style = create_border(
        top    = "thick", top_color    = wb_color(color_theme$bg_primary), # maybe the border color should get it's own color definition
        bottom = "thick", bottom_color = wb_color(color_theme$bg_primary)
      )
    ),
    # --- text area ---
    text_area = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color("white")
      ),
      font_style = create_font(),
      border_style = create_border()
    ),
    # --- form overview section title ---
    form_overview_section = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_primary)
      ),
      font_style = create_font(
        sz = 14,
        b = TRUE,
        color = wb_color(color_theme$font_primary)
      ),
      border_style = create_border() # maybe one day add color theme option for border
    ),
    # --- table head ---
    table_head = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_tablehead)
      ),
      font_style = create_font(
        b = TRUE,
        color = wb_color(color_theme$font_tablehead)
      ),
      border_style = create_border(
        top    = "thin", top_color    = wb_color("black"),
        bottom = "thin", bottom_color = wb_color("black"),
        left   = "thin", left_color   = wb_color("black"),
        right  = "thin", right_color  = wb_color("black")
      )
    ),
    # --- visit column names in form visit table ---
    visit_names = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_visit)
      ),
      font_style = create_font(
        b = TRUE,
        color = wb_color(color_theme$font_visit)
      ),
      border_style = create_border()
    ),
    # --- 'Tables' column in form overview tables ---
    table_names = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_tablecol_overview)
      ),
      font_style = create_font(
        color = wb_color(color_theme$font_tablecol_overview)
      ),
      border_style = create_border()
    ),
    ### --- form sheet head (form name) ---
    form_sheet_head = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_primary)
      ),
      font_style = create_font(
        b = TRUE,
        color = wb_color(color_theme$font_primary)
      ),
      border_style = create_border()
    ),
    ### --- form sheet table list ---
    form_sheet_tables = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(tint_color(color_theme$bg_primary, 0.71))
      ),
      font_style = create_font(),
      border_style = create_border()
    ),
    ### --- form sheet 'Table' column ---
    form_sheet_table_col = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_tablecol_items)
      ),
      font_style = create_font(
        color = wb_color(color_theme$font_tablecol_items)
      ),
      border_style = create_border(
        top    = "thin", top_color    = wb_color(color_theme$border_tablecol_items),
        bottom = "thin", bottom_color = wb_color(color_theme$border_tablecol_items),
        left   = "thin", left_color   = wb_color(color_theme$border_tablecol_items),
        right  = "thin", right_color  = wb_color(color_theme$border_tablecol_items)
      )
    ),
    ### --- select column head ---
    select_column_head = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(tint_color(color_theme$border_select_column, 0.47))
      ),
      font_style = create_font(
        color = wb_color(color_theme$font_select),
        b = TRUE
      ),
      border_style = create_border(
        top    = "thin", top_color    = wb_color("black"),
        bottom = "thin", bottom_color = wb_color("black"),
        left   = "thin", left_color   = wb_color("black"),
        right  = "thin", right_color  = wb_color("black")
      )
    ),
    ### --- select column ---
    select_column = list(
      fill_style = create_fill(
        patternType = "solid",
        fgColor = wb_color(color_theme$bg_select_column)
      ),
      font_style = create_font(
        color = wb_color(color_theme$font_select)
      ),
      border_style = create_border(
        top    = "thin", top_color    = wb_color(color_theme$border_select_column),
        bottom = "thin", bottom_color = wb_color(color_theme$border_select_column),
        left   = "thin", left_color   = wb_color(color_theme$border_select_column),
        right  = "thin", right_color  = wb_color(color_theme$border_select_column)
      )
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
    text_area = list(wrap_text = TRUE, vertical = "top"),
    form_overview_section = list(horizontal = "center"),
    table_head = list(wrap_text = TRUE, horizontal = "center"),
    visit_names = list(wrap_text = TRUE, horizontal = "center"),
    select_column_head = list(horizontal = "center"),
    select_column = list(horizontal = "center")
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
