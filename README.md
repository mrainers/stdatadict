
# stdatadict - Secutrial Data Dictionary & Variable Selection

This R package provides functions for creating a data dictionary or variable selection excel file based on the metadata files of an Secutrial data export and additional files provided by the user.

A **data dictionary** is basically a compact form of the "Datensatztabelle/Data Set Table" from Secutrial. Since it is created from the exported meta data it holds only information about the variables and forms that are included in the export. Furthermore it has no information about data capture rules (e.g. under which circumstances a variable is shown in the data capture) or unit conversion factors, since this data is also not available in the export metadata. You have to extract these informations from other sources and add it to the form item tables, during the creation process.

A **variables selection table** is broadly speaking, a *data dictionay* based on a full export, with a `select` column attached to the form item tables. This 'select' column however can hold more or less complex check rules, that makes it possible for the user to select entire variable groups. 

This package builds upon the the following R packages:

- [openxlsx2](https://janmarvin.github.io/openxlsx2/) for creating the excel file.
- [shiny.i18n](https://appsilon.github.io/shiny.i18n/) for language switching. 

*Automatic translation of questions, item labels etc. is not implemented. But I would happily do so given enough time and provided an api key for google.translate, DeepL or any other translation tool.*


## Installation

### Prerequisites

Install the `remotes` package.

``` r
if (!rlang::is_installed("remotes")) install.packages("remotes")
```

### Installation from gitLab or gitHub (mirror repository)

Install `stdatadict` from the [gitlab repository](https://gitlab.gwdg.de/medinfpub/stdatadict) 
repository with dependent packages.

``` r
library(remotes)

install_git("https://gitlab.gwdg.de/medinfpub/stdatadict", dependencies = TRUE)
```

Install from the [github mirror repository](https://github.com/mrainers/stdatadict) 
repository with dependent packages.

``` r
library(remotes)

install_github("mrainers/stdatadict", dependencies = TRUE)
```



<!--

## Secutrial ExportOption Requirements
test
-->


## License

MIT License Copyright (c) 2025 Department of Medical Informatics - University Medical Center Göttingen
