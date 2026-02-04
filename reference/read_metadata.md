# Read Setup Data Files From A SecuTrial or stExport Directory

Read setup data files from the export directory or zip file:

- vp.csv/xls or visitplan\*.csv/xls

- vpfs.csv or visitplanforms\*.csv/xls

- fs.csv/xls or forms\*.csv/xls

- qs.csv/xls or questions\*.csv/xls

- is.csv/xls or items\*.csv/xls

- cl.csv/xls or cl\*.csv/xls

If one of this files isn't present in the data directory, the function
exits with an error message and no files where read.

You must choose "Store reference values - separate table" and include
"Project setup" export options when creating the export in order to have
these files included.

## Usage

``` r
read_metadata(data_dir, ..., safe_read = FALSE, detect_ctr_forms = FALSE)
```

## Arguments

- data_dir:

  Name of the Directory or Zip File in which the SecuTrial or tsExport
  data is stored.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)
  arguments passed to
  [`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html).
  If not specified this function passes the following parameters to
  `read_delim()`:

  - escape_backslash = TRUE,

  - escape_double = FALSE,

  - locale = readr::locale(decimal_mark = ",", grouping_mark = "",
    encoding = "UTF-8")

  - guess_max = Inf

  - show_col_types = FALSE

- safe_read:

  (logical) Should the data read linewise and repaired if necessary?
  Some exported table have an inconsistent number of fields in the
  lines. When safe_read is activated, the data is read linewise and
  inconsistencies are repaired before reading the data into tables.
  However, this option slows down the reading process, for big files.
  Default = FALSE.

- detect_ctr_forms:

  **\[experimental\]** (logical) should the function identify center
  forms? If TRUE, a formtype column will be added to the fs/forms table,
  which then identifies the center forms. Center forms are identified by
  reading all the headings from the data files. Therefore, setting this
  parameter to TRUE the reading process might be be somewhat slower.
  Default is FALSE.

## Value

List of tibbles with meta data read from the SecuTrial or tsExport
directory or zip file.

## Examples

``` r
if (FALSE) { # interactive()
data_dir <- file.choose()
st_metadata <- read_metadata(data_dir)
}
```
