# Clean Excel Region Names

Names for named regions have the following restrictions:

- The first character of a name must be a letter or an underscore
  character (\_).

- Remaining characters in the name can be letters, numbers, periods, and
  underscore characters.

- Spaces are not allowed.

- Names cannot be the same as a cell reference, such as Z\$100, BIN9, or
  R1C1.

## Usage

``` r
clean_region_name(name)
```

## Arguments

- name:

  (character or character vector) the name(s) of the named region

## Value

a string (vector) of cleaned region names.

## Details

Therefore this function

- replaces all punctuation characters like "-" with "."

- replaces all white spaces with "\_"

- inserts an underscore at the beginning if

  - a name starts with anything else than a letter or underscore

  - a name starts with "c"

  - a name is a cell reference, such as "C1", "AB6R12"

## Examples

``` r
clean_region_name(c("foo-bar", "c19severity", "ad+b 4"))
#> [1] "foo.bar"      "_c19severity" "ad.b_4"      
```
