# Create a Scope Table

Create a table that defines variable scopes. This table can be used for
adding a "Scope"-Column (and Selection Rules) to the form item tables
and Scope-Selection-Tables in the workbook.

## Usage

``` r
create_scope_table(data, scopeid, ...)
```

## Arguments

- data:

  a data frame containing all variable identifiers (join keys) that
  belong to the scope.

- scopeid:

  (character) name/ID of the Scope.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  named list containing additional information about the variable Scope.
  This will be used as columns of the Scope Selection Table in the
  workbook.

## Value

nested tibble decribing the Scope

## Examples

``` r
scope_data <- scope_data <- data.frame (
  mainform = c("form1", "form1", "form2"),
  varname_col  = c("var1", "var3", "var10")
)
create_scope_table(
  scope_data,
  "my_scope",
  list(Description = "This Scope covers this and that",
       visits = "Baseline")
)
#> # A tibble: 1 × 4
#>   Scope    Description                     visits   data            
#>   <chr>    <chr>                           <chr>    <list>          
#> 1 my_scope This Scope covers this and that Baseline <tibble [3 × 2]>
```
