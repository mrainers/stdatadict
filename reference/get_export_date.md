# Get The Creation Date Time Of The Data Export Directory

This function tries to extract the data creation date time from the
ExportOptions file that is exported with the data. However if due to
some processing steps or other reasons there is no ExportOptions file
found in the data directory, then return the modification time of the
data directory

## Usage

``` r
get_export_date(data_dir, is_zip = FALSE)
```

## Arguments

- data_dir:

  Name of the Directory or Zip File in which the SecuTrial or tsExport
  data is stored.

- is_zip:

  (logical) is files directory a zip file?

## Value

(POSIXt)
