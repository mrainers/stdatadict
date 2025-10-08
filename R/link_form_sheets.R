#' Link Form Names in the Form Overview
#'
#' @description
#' Link the form names in the "Table"/"Embedding Form" columns in the
#' Form Overview sheet to their respective form sheets.
#'
#' Additionally add a link back to the Form Overview to each A1 cell of the
#' form item sheets.
#'
#' Note that the form overview and the form sheets must be added to the workook
#' before calling this function.
#'
#' @param wb a data dictionary workbook
#'
#' @return workbook with links to form item tables (invisibly)
#' @export
#'
#' @examples \dontrun{
#' wb <- wb_workbook()
#' add_form_overview(wb, datadict_tables = datadict_tables)
#' add_form_sheets(wb, datadict_tables = datadict_tables)
#' link_form_sheets(wb)
#' }
link_form_sheets <- function(wb) {
  # get first row of the Form Overview sheet
  form_overview_A <- wb$to_df("Form Overview", cols = 1, col_names = FALSE)

  # also store the styles from that row. They will be reapplied later to overwrite
  # the hyperlink font style in these cells.
  form_overview_A_styles <- wb$get_cell_style(
    "Form Overview", paste0("A1:A", nrow(form_overview_A))
  )

  # identify in which rows are the forms to be linked.
  link_rows <- form_overview_A |>
    mutate(row = dplyr::row_number()) |>
    filter(.data$A %in% wb$sheet_names) |>
    filter(.data$A != "Form Overview") |> # remove the header row
    tibble::deframe()

  # Link every form name in the first column of the Form Overview sheet to the
  # respective form sheet
  link_rows |>
    purrr::iwalk(
      \(row, formname)
      wb$add_hyperlink(
        sheet = "Form Overview",
        dims = paste0(row, "A"),
        target = paste0(formname, "!A1"),
        is_external = FALSE,
        tooltip = "Jump to Form"
      )
    )

  # restore the original cell style
  wb$set_cell_style(
    "Form Overview",
    paste0("A1:A", length(form_overview_A_styles)),
    form_overview_A_styles
  )

  # Add a link back to the Form Overview in the form sheets
 names(link_rows) |>
    purrr::walk(
      \(formname)
      wb$add_hyperlink(
        sheet = formname,
        dims = "1A",
        target = "'Form Overview'!A1",
        is_external = FALSE,
        tooltip = "Back to Form Overview"
      )
    )
}
