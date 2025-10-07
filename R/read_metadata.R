# This function is a copy from toTools package v1.0.3
#
#' Load an individual table from a Secutrial export.
#'
#' @description
#' This function loads an individual csv file from an Secutrial export
#' zip archive or directory.
#'
#' This is an internal function which is wrapped by read_secutrial_raw
#'
#' Secutrial attaches an empty column at the end of each scv table.Therefore this
#' function also removes the empty last column if present.
#'
#' @param data_dir (character) Path with directory or zip file of the
#'    SecuTrial export.
#' @param file_name (character) Name of the csv file which should be read.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots] arguments passed to [readr::read_delim()].
#'    If not specified this function passes the following parameters to `read_delim()`:
#'    - escape_backslash = TRUE,
#'    - escape_double = FALSE,
#'    - locale = readr::locale(decimal_mark = ",", grouping_mark = "", encoding = "UTF-8")
#'    - guess_max = Inf
#'    - show_col_types = FALSE
#' @param safe_read (logical) Should the data read linewise and repaired if
#'   necessary? Some exported table have an inconsistent number of fields in the
#'   lines. When safe_read is activated, the data is read linewise and
#'   inconsistencies are repaired before reading the data into tables. However,
#'   this option slows down the reading process, for big files. Default = FALSE.
#'
#' @return The function returns a tibble for the data in file_name.
#'
#' @seealso [read_secutrial_raw()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # SUEP test data location
#' suep_file <- system.file(
#'   "extdata", "NAPKON", "st_exports",
#'   "s_export_CSV_SUEP_20241009-141616.zip",
#'   package = "toTools"
#' )
#' scv <- read_secutrial_table(suep_file, "scv.csv")
#' head(scv)
#'}
read_secutrial_table <- function(data_dir, file_name, ..., safe_read = FALSE) {
  is_dir <- dir.exists(data_dir)
  is_zip <- str_detect(data_dir, "\\.zip$") & file.exists(data_dir)

  if (is_zip) {
    data_file <- unz(data_dir, file_name)
  } else if (is_dir) {
    data_file <- file.path(data_dir, file_name)
  } else {
    stop(paste("no zip file or directory found:", data_dir))
  }

  # combine user defined and functions default parameters for read_delim function.
  dots <- rlang::list2(...)

  # tables from a secutrial export may be non-rectangular due to a varying
  # number of (empty) cells at the end of each line.
  # Therefore read the files line-wise and fill missing cells if necessary
  # to make table rectangular.
  # (analogous to fill = TRUE in read.table())

  locale = dots$locale %||% readr::locale(
    decimal_mark = ",",
    grouping_mark = "",
    encoding = "UTF-8"
  )

  if (safe_read) {
    table_lines <- readr::read_lines(data_file, locale = locale)

    # table_lines is utf8 encoded, therefore change the encoding for the local
    # param
    locale$encoding <- "UTF-8"
    dots$locale <- NULL

    quote = dots$quote %||% "\""
    delim = dots$delim %||% guess_delim(table_lines[1], quote = quote)

    read_defaults <- list(
      delim = delim,
      quote = quote,
      escape_backslash = TRUE,
      escape_double = FALSE,
      locale = locale,
      guess_max = Inf,
      show_col_types = FALSE,
      name_repair = totools_name_repair #use internal package function for name repairing
    )

    if (is_empty(dots)) {
      read_params <- read_defaults
    } else {
      read_params <- dots %>%
        append(read_defaults %>% purrr::discard_at(names(dots)))
    }


    tokenizer_params <- read_params %>%
      purrr::keep_at(c("delim", "quote", "na", "quoted_na", "comment",
                       "trim_ws", "escape_double", "escape_backslash",
                       "skip_empty_rows"))

    # Number of fields in each line
    n_fields <- readr::count_fields(
      paste0(table_lines, "\n"),
      inject(readr::tokenizer_delim(!!!tokenizer_params))
    )

    # fill lines with empty cells if the number of fields isn't equal
    if (any(n_fields) < max(n_fields)) {
      table_lines <- paste0(
        table_lines,
        stringr::str_dup(paste0(delim, quote, quote), (max(n_fields) - n_fields))
      )
    }

    # escape special characters possibly read with wrong encoding (experimental)
    table_lines <- table_lines %>% stringr::str_replace_all("\xe(\\d+)", "\\xe\\1")

    # read the data
    data <- inject(readr::read_delim(I(table_lines), !!!read_params))

  } else { # safe_read == FALSE
    # combine user and default reading options
    read_defaults <- list(
      escape_backslash = TRUE,
      escape_double = FALSE,
      locale = locale,
      guess_max = Inf,
      show_col_types = FALSE,
      name_repair = totools_name_repair #use internal package function for name repairing
    )

    if (is_empty(dots)) {
      read_params <- read_defaults
    } else {
      read_params <- dots %>%
        append(read_defaults %>% purrr::discard_at(names(dots)))
    }

    # suppress warnings if filename is cl.csv, because this file is bugged. It has
    # different number of columns in different row, which causes non problematic
    # parsing issues.
    if (str_detect(file_name, "^cl.*\\.(csv|xls)")) {
      rlang::local_options(warn = -1)
    }

    # read the data
    data <- inject(readr::read_delim(data_file, !!!read_params))
  }

  # remove last empty column added by secutrial export (if it is there)
  lastcolname <- data %>%
    names() %>%
    dplyr::last()

  # Only do something if last column is (X|...)<number>
  if (stringr::str_detect(lastcolname, "(^\\.\\.\\.|^X)\\d*$")) {
    if (data[[lastcolname]] %>% is.na() %>% all()) { # if last column is empty, delete it.
      data <- data %>% dplyr::select(-dplyr::last_col())
    }
  }
  data
}


#' Read Setup Data Files From A SecuTrial or stExport Directory
#'
#' @description
#' Read setup data files from the export directory or zip file:
#'
#' - vp.csv/xls or visitplan*.csv/xls
#' - vpfs.csv or visitplanforms*.csv/xls
#' - fs.csv/xls or forms*.csv/xls
#' - qs.csv/xls or questions*.csv/xls
#' - is.csv/xls or items*.csv/xls
#' - cl.csv/xls or cl*.csv/xls
#'
#' If one of this files isn't present in the data directory, the function exits
#' with an error message and no files where read.
#'
#' You must choose "Store reference values - separate table" and include
#' "Project setup" export options when creating the export in order to have
#' these files included.
#'
#' @inheritParams read_secutrial_table
#' @inheritParams readr::locale
#' @param data_dir Name of the Directory or Zip File in which the SecuTrial or
#'     tsExport data is stored.
#'
#' @return List of tibbles with meta data read from the SecuTrial or tsExport
#'     directory or zip file.
#' @export
#'
#' @examplesIf interactive()
#' data_dir <- file.choose()
#' st_metadata <- read_metadata(data_dir)
read_metadata <- function(data_dir, ..., safe_read = FALSE) {
  # Check if data_dir is an existing zip file or directory.
  is_zip <- grepl(".zip$", data_dir) & file.exists(data_dir)
  is_dir <- dir.exists(data_dir)

  # filenames in secutrial folder
  if (is_dir) {
    files <- list.files(data_dir)
  } else if (is_zip) {
    files <- utils::unzip(data_dir, list = TRUE)$Name
  } else {
    abort(paste("no zip file or directory found:", data_dir))
  }

  # parse file names ----

  # data files
  data_files <- files |> str_subset("html$", negate = TRUE)

  # data file extension
  file_extension <- data_files |>
    tools::file_ext() |>
    unique()

  # abort when files are not CSV or Excel format
  if (
    length(file_extension) != 1 ||
    tolower(file_extension) %notin% c("csv", "xls", "xlsx")
  ) {
    abort("Your export must be exported as MS Excel or CSV format.")
  }

  meta_file_pattern_short <- c(
    vp   = "^vp",   # Visit Plan
    vpfs = "^vpfs", # Visit-Form-Connection
    fs   = "^fs",   # Form Information
    qs   = "^qs",   # Question Labels & Subform affiliation
    is   = "^is",   # Question and Variable Labels
    cl   = "^cl"    # Value Labels
  ) |>
    # use modify for pasting to keep names.
    modify(\(pattern) paste0(pattern, "\\.", file_extension, "$"))

  meta_file_pattern_long <- c(
    vp   = "^visitplan_.*",
    vpfs = "^visitplanforms_.*",
    fs   = "^forms_.*",
    qs   = "^questions_.*",
    is   = "^items_.*",
    cl   = "^cl_.*"
  ) |>
    # use modify for pasting to keep names.
    modify(\(pattern) paste0(pattern, "\\.", file_extension, "$"))

  read_files_short <- meta_file_pattern_short |>
    map(\(pattern) stringr::str_subset(data_files, pattern)) |>
    unlist()

  read_files_long <- meta_file_pattern_long |>
    map(\(pattern) stringr::str_subset(data_files, pattern)) |>
    unlist()

  # check if all needed files are included in the (zip) directory.
  if (!is_empty(read_files_short)) {
    read_files <- read_files_short

    missing_files <- meta_file_pattern_short |>
      purrr::discard_at(names(read_files_short)) |>
      str_remove_all("\\^|\\$|_\\.|\\\\")

  } else if (!is_empty(read_files_long)) {
    read_files <- read_files_long

    missing_files <- meta_file_pattern_long |>
      purrr::discard_at(names(read_files_long)) |>
      str_remove_all("\\^|\\$|_\\.|\\\\")

  } else {
    abort("The directory or zip does not contain any SecuTrial setup data files.")
  }

  if (length(missing_files > 0)) {
    abort(paste(
      "Can not load all required setup files. The following files are missing:",
      paste(missing_files, collapse = "\n"),
      sep = "\n"
    ))
  }

  # read setup files -----

  st_metadata <- read_files |>
    map(\(file) {
      rlang::inform(paste("Read:", file))
      read_secutrial_table(data_dir, file, ..., safe_read = safe_read)
    })

  st_metadata <- append(st_metadata,
                        list(export_date = get_export_date(data_dir, is_zip)),
                        after = 0
  )

  st_metadata
}


#' Get The Creation Date Time Of The Data Export Directory
#'
#' This function tries to extract the data creation date time from the
#' ExportOptions file that is exported with the data.
#' However if due to some processing steps or other reasons there is no
#' ExportOptions file found in the data directory, then return the
#' modification time of the data directory
#'
#' @param data_dir Name of the Directory or Zip File in which the SecuTrial or
#'     tsExport data is stored.
#' @param is_zip (logical) is files directory a zip file?
#'
#' @return (POSIXt)
#' @keywords internal
get_export_date <- function(data_dir, is_zip = FALSE) {
  export_options <- NULL

  # try to read a export options file
  if (is_zip) {
    export_option_file <- utils::unzip(data_dir, list = TRUE)$Name %>%
      str_subset("^ExportOptions_\\w+.html$")

    if (length(export_option_file) > 0) {
      export_options <- unz(data_dir, export_option_file) %>%
        readr::read_lines()
    }
  } else {
    export_option_file <- list.files(data_dir,
                                     pattern = "^ExportOptions_\\w+.html$",
                                     full.names = TRUE)

    if (length(export_option_file) > 0) {
      export_options <- readr::read_lines(export_option_file)
    }
  }

  # if an export option file is in the data directory get the creation date time
  # from that file.
  # if not, take the mdate from the data directory
  if (!is.null(export_options)) {
    export_options %>%
      stringr::str_which("Erstellungsdatum|Created on") %>%
      {export_options[. + 2]} %>%
      stringr::str_extract("(<b>)(.*)(</b>)", group = 2) %>%
      as.POSIXlt(format = "%d.%m.%Y - %H:%M:%S")
  } else {
     file.mtime(data_dir)
  }
}



# TODO an andere Funktion auslagern
#' The column "hidden" will be added to the files:
#' "fs.csv",
#' "qs.csv", and
#' "is.csv"
#' if it doesn't already exists, because further functions need this
#' column to be present.
#'
#' # Add 'hidden' variable if it does not exist to 'fs', 'qs' and 'is'.
#' files_with_hidden <- c("fs.csv", "qs.csv", "is.csv",
#'                        "^forms_", "^questions_", "^items_")
#' if (any(str_detect(file_name, files_with_hidden))) {
#'   if (!"hidden" %in% names(data)) data$hidden <- as.numeric(NA)
#' }
