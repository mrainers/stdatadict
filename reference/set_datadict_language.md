# Set Language of the Data Dictionary

Set Language of the Data Dictionary

## Usage

``` r
set_datadict_language(lang = c("en", "de", stdatadictEnv$available_languages))
```

## Arguments

- lang:

  a character string that specifies the language code for the language
  of the data dictionary. Get a list of available language codes with
  [`get_datadict_language_codes()`](get_datadict_language_codes.md).

## Value

old language option invisibly.

## Examples

``` r
## Set the language of the data dictionary to german
set_datadict_language("de")
```
