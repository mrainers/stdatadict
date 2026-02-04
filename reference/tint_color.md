# Tint or shade a color

Calculate a brighter or darker version of the original color.

## Usage

``` r
tint_color(color, tint)
```

## Arguments

- color:

  String, either a color name known by R, or the hex rgb code of the
  background color.

- tint:

  numeric value between -1 and +1. Negative values darkens and positive
  values brightens the original color.

## Value

a hex rgb code as string.

## Examples

``` r
tint_color("purple", 0.4)
#> [1] "#C679F6"
tint_color("#FFD530", -0.7)
#> [1] "#4C400E"
```
