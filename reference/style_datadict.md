# Create And Register Data Dictionary Workbook Styles

Create And Register Data Dictionary Workbook Styles

## Usage

``` r
style_datadict(wb, theme_name = stdatadictEnv$use_color_theme)
```

## Arguments

- wb:

  a workbook

- theme_name:

  Character Name of the color theme that should be used for the data
  dictionary

## Value

workbook with added styles, invisibly

## Examples

``` r
wb <- openxlsx2::wb_workbook()
style_datadict(wb)
```
