# Add Variable Scopes To Form Item Tables

This functions joins a "Scope" variable with the form item tables in
"data_dict_tables". If form item tables already have a Scope variable,
the content of already existing and new Scope variable be concatenated.

## Usage

``` r
join_scopes(datadict_tables, scope)
```

## Arguments

- datadict_tables:

  table list, generated with
  [`create_datadict_tables()`](create_datadict_tables.md)

- scope:

  "Scope Table" (see Details) or list of Scope tables.

## Value

datadict_tables list where a Scope variable is added to form_item
tables.

## Details

A Scope table is a nested table with the following format:

- Scope:

  ID String of the variable Scope

- additional Scope Information:

  Variables that describe the Scope, such as Scope Label/Name,
  Description, etc.

- data:

  a table containing variable names and other identifying variables to
  which the Scope belongs (joining key variables)

## Examples

``` r
if (FALSE) { # \dontrun{
datadict_tables <- join_scopes(datadict_tables, datapkg_pop_scope)
} # }
```
