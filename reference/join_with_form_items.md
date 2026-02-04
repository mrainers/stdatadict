# Add Data To Form Item Tables

Wrapper function to join a data set or a list of data sets with the form
item tables. It combines all tables in datadict_tables\$form_items, and
adds the additional data with dplyrs `inner_join()` function.

`mainform` is the key variable to match by form name.

Keep in mind that the variable names in the form item tables depend on
the language settings of this package (Default is "en"). To be
independent from the language setting rename your matching key variables
as following before performing the join:

- `table_col` = Table,

- `question_col` = Question,

- `varlab_col` = "Variable Label",

- `varname_col` = "Variable Name",

- `vartype_col` = Type

## Usage

``` r
join_with_form_items(datadict_tables, data, ...)
```

## Arguments

- datadict_tables:

  table list, generated with
  [`create_datadict_tables()`](create_datadict_tables.md)

- data:

  data frame or list of data framed to be joined with form_item tables
  of datadict_tables

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  additional options to pass to
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)

## Value

datadict_tables with joined data to the form item tables

## Examples

``` r
if (FALSE) { # \dontrun{
## create example data for matching
extra_data <- form_items |>
  tibble::enframe(name = "mainform") |>
  tidyr::unnest(cols = c(value)) |>
  select(
    mainform,
    table_col = Table,
    varname_col = "Variable Name") |>
  # add random vargroups
  mutate(var_group = sample(
    x = c("one", "two", NA),
    size = nrow(.),
    replace = TRUE,
    prob = c(0.2, 0.2, 0.6)
  )) |>
  filter(!is.na(var_group))

## add "var_group" to form_items, match by mainform, table and varname
datadict_tables <- join_with_form_items(datadict_tables, extra_data)
} # }
```
