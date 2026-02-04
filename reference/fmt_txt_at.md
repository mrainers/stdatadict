# Format Matched Strings In a Text.

Format Matched Strings In a Text.

## Usage

``` r
fmt_txt_at(text, pattern, ...)
```

## Arguments

- text:

  (character) text that contains strings that should be formatted

- pattern:

  regular expressions, or vector of regular expressions

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  format options pass to
  [`openxlsx2::fmt_txt()`](https://janmarvin.github.io/openxlsx2/reference/fmt_txt.html).
  Can also be a lists of format options that has the same length as
  `pattern`.

## Value

an openxlsx2 fmt_txt string

## Examples

``` r
shopping_list <- "apples x4, bag of flour, bag of sugar, milk x2"

# format the word 'bag' bold
fmt_txt_at(shopping_list, "\\bbag\\b", bold = TRUE)
#> fmt_txt string: 
#> [1] "apples x4, bag of flour, bag of sugar, milk x2"

# format the x<nr> green and bold and the "bag of" in italic
fmt_txt_at(shopping_list,
           c("x\\d+", "\\bbag of\\b"),
           list(bold = TRUE, color = openxlsx2::wb_color("green")),
           list(italic = TRUE)
           )
#> fmt_txt string: 
#> [1] "apples x4, bag of flour, bag of sugar, milk x2"
```
