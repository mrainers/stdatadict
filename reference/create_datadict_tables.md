# Create Data Dictionary Tables

Create the following tables for use in the data dictionary file:

- visit form overview

- casenode forms overview

- centreform overview

- subform (repetition table) overview

- itemtables for each form

## Usage

``` r
create_datadict_tables(st_metadata, ...)
```

## Arguments

- st_metadata:

  list of dataset that contains the secutrial meta data tables.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  additional options to pass to internal functions.  
  - `invert_hidden` Boolean, define if the "hidden" variable in the
  question/qs file is inverted by a secutrial bug.  
  If this option is omitted, this function assumes that the bug is
  active when more than 70% of all questions are marked as hidden.

## Value

nested list of tibbles with the following structure:

    $ form_overview
     ...$ visit_forms
     ...$ casenode_forms
     ...$ sub_forms
     ...$ visitarms (only if the study contains multiple visit arms)
    $ form_items
     ...$ <form1>
     ...$ <form2>
     ...

## Details

All interim calculations such as the refined meta data tables or the
extracted study id are saved into the [intermediates](intermediates.md)
environment of this package. Type:
`s(stdatadict::intermediates, all.names = TRUE)` to get a list of all
objects stored in this environment. Access object from this environment:
`stdatadict::intermediates$<ObjName>`.

## Examples

``` r
if (FALSE) { # interactive()
datadict_raw <- create_datadict_tables(st_metadata)
}
```
