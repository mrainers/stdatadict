#' Read A Single Meta Data File From A SecuTrial Export Directory
#'
#' @inheritParams read_metadata
#' @param file_name Name of the csv file that should be read.
#' @param is_zip Boolean if the files directory is actually a zip file.
#'
#' @return the csv data read from the file as tibble. If necessary a "hidden"
#'    column is added to certain files that are expected to have one.
#' @noRd
read_meta_file <- function(data_dir, file_name,
                           delim = ";", decimal_mark = ",",
                           encoding = "UTF-8",
                           is_zip = FALSE) {
  if (is_zip) {
    data_file <- unz(data_dir, file_name)
  } else {
    data_file <- file.path(data_dir, file_name)
  }

  # suppress warnings if filename is cl.csv, because this file is bugged. It has
  # different number of columns in different row, which causes non problematic
  # parsing issues.
  if (str_detect(file_name, "^cl")) {
    defaultW <- getOption("warn")
    withr::local_options(list(warn = -1))
  }

  data <- read_delim(data_file,
                     delim = delim,
                     locale = readr::locale(
                       date_names = "de",
                       decimal_mark = decimal_mark,
                       grouping_mark = ""
                     ),
                     escape_backslash = TRUE,
                     escape_double = FALSE
  )

  # undo warning suppression
  if (file_name == "cl.csv") withr::local_options(warn = defaultW)


  # remove last empty column added by secutrial export (if it is there)
  lastcolname <- data %>%
    names() %>%
    last()
  # Only do something if last column is (X|...)<number>
  if (str_detect(lastcolname, "(^\\.\\.\\.|^X)\\d*$")) {
    if (data[[lastcolname]] %>% is.na() %>% all()) { # if last column is empty, delete it.
      data <- data %>% select(-last_col())
    }
  }

  # Add 'hidden' variable if it does not exist to 'fs', 'qs' and 'is'.
  files_with_hidden <- c("fs.csv", "qs.csv", "is.csv",
                         "^forms_", "^questions_", "^items_")
  if (any(str_detect(file_name, files_with_hidden))) {
    if (!"hidden" %in% names(data)) data$hidden <- as.numeric(NA)
  }

  data
}


#' Read Meta Data Files From A SecuTrial or stExport Directory
#'
#' @description
#' Read the following csv files from the export directory or zip file:
#'
#' - vp.csv or visitplan*.csv
#' - vpfs.csv or visitplanforms*.csv
#' - fs.csv or forms*.csv
#' - qs.csv or questions*.csv
#' - cl.csv or cl*.csv
#' - is.csv or items*.csv
#'
#' If one of this files isn't present in the data directory, the function exits
#' with an error message and no files where read.
#'
#' The column "hidden" will be added to the files: "fs.csv", "qs.csv", and
#' "is.csv" if it doesn't already exists, because further functions need this
#' column to be present.
#'
#' @inheritParams readr::read_delim
#' @inheritParams readr::locale
#' @param data_dir Name of the Directory or Zip File in which the SecuTrial or
#'     tsExport data is stored.
#' @param decimal_mark Symbol used to indicate the decimal place.
#'     Decimal mark can only be ⁠,⁠ or ..
#'
#' @return List of tibbles with meta data read from the SecuTrial or tsExport
#'     directory or zip file.
#' @export
#'
#' @examplesIf interactive()
#' data_dir <- file.choose()
#' st_metadata <- read_metadata(data_dir)
read_metadata <- function(data_dir,
                          delim = ";", decimal_mark = ",",
                          encoding = "UTF-8") {
  meta_files_short <- c(
    vp   = "^vp.csv$",   # Visit Plan
    vpfs = "^vpfs.csv$", # Visit-Form-Connection
    fs   = "^fs.csv$",   # Form Information
    qs   = "^qs.csv$",   # Question Labels & Subform affiliation
    cl   = "^cl.csv$",   # Value Labels
    is   = "^is.csv$"    # Question and Variable Labels
  )

  meta_files_long <- c(
    vp   = "^visitplan_.*\\.csv$",
    vpfs = "^visitplanforms_.*\\.csv$",
    fs   = "^forms_.*\\.csv$",
    qs   = "^questions_.*\\.csv$",
    cl   = "^cl_.*\\.csv$",
    is   = "^items_.*\\.csv$"
  )

  # Check if data_dir is an existing zip file or directory.
  is_zip <- grepl(".zip$", data_dir)
  is_dir <- dir.exists(data_dir)

  if (is_zip) {
    read_files <- utils::unzip(data_dir, list = TRUE)$Name %>%
      str_subset(paste0(c(meta_files_long, meta_files_short), collapse = "|"))
  } else if (is_dir) {
    read_files <- list.files(data_dir, pattern = "*.csv") %>%
      str_subset(paste0(c(meta_files_long, meta_files_short), collapse = "|"))
  } else {
    stop(paste("no zip file or directory found:", data_dir))
  }

  # check if the files are in the long or short name format
  if (some(meta_files_long, ~ any(str_detect(read_files, .)))) {
    meta_files <- meta_files_long
  } else if (some(meta_files_short, ~ any(str_detect(read_files, .)))) {
    meta_files <- meta_files_short
  } else {
    stop("The directory or zip does not contain any SecuTrial meta data files.")
  }

  # check if all needed files are included in the (zip) directory.
  missing_files <- meta_files %>%
    discard(~ any(str_detect(read_files, .))) %>%
    str_remove_all("\\^|\\$|_\\.|\\\\")

  if (length(missing_files > 0)) {
    stop(paste(
      "Can not load all required meta data files. The following files are missing:",
      paste(missing_files, collapse = "\n"),
      sep = "\n"
    ))
  }

  # Read meta data files from Secutrial or tsExport
  meta_files_read <- map(meta_files, ~ str_subset(read_files, .))

  meta_files_read %>%
    map(~ {
      cat(paste("\nRead:", .x))
      read_meta_file(data_dir, .x, delim, decimal_mark, encoding, is_zip)
    })
}

# TODO: add function to calculate data creation date, based on html file or
# cdate/mdate?
