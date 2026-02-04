# Show Color Theme

Show Color Theme

## Usage

``` r
show_color_theme(name)
```

## Arguments

- name:

  of color theme. Use [`ls_color_themes()`](ls_color_themes.md) to list
  all currently available themes.

## Value

name parameter of the function call invisibly

## Examples

``` r
show_color_theme("purpur")
#> <table class=" lightable-paper" style='font-family: "Source Sans Pro", helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>
#> <caption>Color Theme: purpur</caption>
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> name </th>
#>    <th style="text-align:left;"> description </th>
#>    <th style="text-align:left;"> color </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"> bg_primary </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(153, 0, 153, 255) !important;"> background for document and form titles </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(153, 0, 153, 255) !important;"> #990099 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> font_primary </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(255, 255, 255, 255) !important;"> font color for document and form titles </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(255, 255, 255, 255) !important;"> #FFFFFF </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> bg_subtitle </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(255, 255, 255, 255) !important;"> background subtitle </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(255, 255, 255, 255) !important;"> #FFFFFF </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> font_subtitle </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(153, 0, 153, 255) !important;"> fontcolor subtitle </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(153, 0, 153, 255) !important;"> #990099 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> bg_h1 </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(225, 181, 225, 255) !important;"> background heading </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(225, 181, 225, 255) !important;"> #E1B5E1 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> font_h1 </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(153, 0, 153, 255) !important;"> fontcolor heading </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(153, 0, 153, 255) !important;"> #990099 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> bg_tablehead </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(144, 144, 144, 255) !important;"> background column names </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(144, 144, 144, 255) !important;"> #909090 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> font_tablehead </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(255, 255, 255, 255) !important;"> fontcolor column names </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(255, 255, 255, 255) !important;"> #FFFFFF </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> bg_visit </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(0, 102, 204, 255) !important;"> background visit names </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(0, 102, 204, 255) !important;"> #0066CC </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> font_visit </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(255, 255, 255, 255) !important;"> fontcolor visit names </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(255, 255, 255, 255) !important;"> #FFFFFF </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> bg_tablecol_overview </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(255, 213, 48, 255) !important;"> background table names in form overview </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(255, 213, 48, 255) !important;"> #FFD530 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> font_tablecol_overview </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(0, 0, 0, 255) !important;"> fontcolor table names in form overview </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(0, 0, 0, 255) !important;"> #000000 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> bg_tablecol_items </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(204, 204, 204, 255) !important;"> background table names in form items </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(204, 204, 204, 255) !important;"> #CCCCCC </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> font_tablecol_items </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(0, 0, 0, 255) !important;"> fontcolor table names in form items </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(0, 0, 0, 255) !important;"> #000000 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> border_tablecol_items </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(144, 144, 144, 255) !important;"> border table names in form items </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(144, 144, 144, 255) !important;"> #909090 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> font_hidden </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(150, 150, 170, 255) !important;"> fontcolor hidden forms and items </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(150, 150, 170, 255) !important;"> #9696aa </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> bg_select_column </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(225, 247, 239, 255) !important;"> background 'select' column </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(225, 247, 239, 255) !important;"> #E1F7EF </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> font_select_column </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(0, 97, 65, 255) !important;"> font color 'select' column </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(0, 97, 65, 255) !important;"> #006141 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> border_select_column </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(144, 224, 197, 255) !important;"> border color 'select' column </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(144, 224, 197, 255) !important;"> #90E0C5 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> font_select_all </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(0, 97, 65, 255) !important;"> font color 'select all' question </td>
#>    <td style="text-align:left;color: white !important;background-color: rgba(0, 97, 65, 255) !important;"> #006141 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> bg_select_all </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(255, 255, 255, 255) !important;"> background color 'select all' question </td>
#>    <td style="text-align:left;color: black !important;background-color: rgba(255, 255, 255, 255) !important;"> #FFFFFF </td>
#>   </tr>
#> </tbody>
#> </table>
```
