# Get Data Dictionary Color Theme

Get Data Dictionary Color Theme

## Usage

``` r
get_color_theme(name = NULL)
```

## Arguments

- name:

  of color theme. Use [`ls_color_themes()`](ls_color_themes.md) to list
  all currently available themes. If name = NULL (Default), the function
  returns the currently active color theme. Use
  [`active_color_theme()`](active_color_theme.md) to get the name of the
  active color theme.

## Value

named list with the colors defined in the color theme

## Examples

``` r
get_color_theme()
#> $bg_primary
#> [1] "steelblue4"
#> 
#> $font_primary
#> [1] "#FFFFFF"
#> 
#> $bg_subtitle
#> [1] "#FFFFFF"
#> 
#> $font_subtitle
#> [1] "steelblue4"
#> 
#> $bg_h1
#> [1] "steelblue2"
#> 
#> $font_h1
#> [1] "#111188"
#> 
#> $bg_tablehead
#> [1] "#909090"
#> 
#> $font_tablehead
#> [1] "#FFFFFF"
#> 
#> $bg_visit
#> [1] "#0066CC"
#> 
#> $font_visit
#> [1] "#FFFFFF"
#> 
#> $bg_tablecol_overview
#> [1] "#FFD530"
#> 
#> $font_tablecol_overview
#> [1] "#000000"
#> 
#> $bg_tablecol_items
#> [1] "#CCCCCC"
#> 
#> $font_tablecol_items
#> [1] "#000000"
#> 
#> $border_tablecol_items
#> [1] "#909090"
#> 
#> $font_hidden
#> [1] "#9696aa"
#> 
#> $bg_select_column
#> [1] "#E1F7EF"
#> 
#> $font_select_column
#> [1] "#006141"
#> 
#> $border_select_column
#> [1] "#90E0C5"
#> 
#> $font_select_all
#> [1] "#006141"
#> 
#> $bg_select_all
#> [1] "#FFFFFF"
#> 
get_color_theme("purpur")
#> $bg_primary
#> [1] "#990099"
#> 
#> $font_primary
#> [1] "#FFFFFF"
#> 
#> $bg_subtitle
#> [1] "#FFFFFF"
#> 
#> $font_subtitle
#> [1] "#990099"
#> 
#> $bg_h1
#> [1] "#E1B5E1"
#> 
#> $font_h1
#> [1] "#990099"
#> 
#> $bg_tablehead
#> [1] "#909090"
#> 
#> $font_tablehead
#> [1] "#FFFFFF"
#> 
#> $bg_visit
#> [1] "#0066CC"
#> 
#> $font_visit
#> [1] "#FFFFFF"
#> 
#> $bg_tablecol_overview
#> [1] "#FFD530"
#> 
#> $font_tablecol_overview
#> [1] "#000000"
#> 
#> $bg_tablecol_items
#> [1] "#CCCCCC"
#> 
#> $font_tablecol_items
#> [1] "#000000"
#> 
#> $border_tablecol_items
#> [1] "#909090"
#> 
#> $font_hidden
#> [1] "#9696aa"
#> 
#> $bg_select_column
#> [1] "#E1F7EF"
#> 
#> $font_select_column
#> [1] "#006141"
#> 
#> $border_select_column
#> [1] "#90E0C5"
#> 
#> $font_select_all
#> [1] "#006141"
#> 
#> $bg_select_all
#> [1] "#FFFFFF"
#> 
```
