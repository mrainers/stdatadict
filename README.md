
# stdatadict - Secutrial Data Dictionary & Variable Selection

This R package provides functions for creating a data dictionary or variable selection excel file based on the metadata files of an Secutrial data export and additional files provided by the user.

It builds upon the packages
- [openxlsx2](https://janmarvin.github.io/openxlsx2/) for creating the excel file.
- [shiny.i18n](https://appsilon.github.io/shiny.i18n/) for language switching. 

*Automatic translation of questions, item labels etc. is not implemented. But I would happily do so given enough time and provided an api key for google.translate, DeepL or any other translation tool.*


## Installation

### Prerequisites

[Create a Personal Access Token in Gitlab](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html) with the scope: `read_api`.
Save the token for later use.

Install the `remotes` package.

``` r
if (!rlang::is_installed("remotes")) install.packages("remotes")
```

### Installation with install_gitlab()

Install `stdatadict` from gitlab and dependent packages.
``` r
library(remotes)

install_gitlab(
  repo = "medinf/kvf/kardio/dzhk/num-transferstelle/stdatadict",
  host = "https://gitlab-pe.gwdg.de",
  auth_token = "<YOUR_AUTH_TOKEN>",
  dependencies = TRUE
)
```

You can also store your access token for later use as R's system environment variable.
`install_gitlab()` then automatically uses the stored token for access to gitlab.
```r
Sys.setenv(GITLAB_PAT = "<YOUR_AUTH_TOKEN>")

install_gitlab(
  repo = "medinf/kvf/kardio/dzhk/num-transferstelle/stdatadict",
  host = "https://gitlab-pe.gwdg.de",
  dependencies = TRUE
)
```


<!--  

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(stdatadict)
## basic example code
```

-->
<!--

### Secutrial ExportOption Requirements
-->


## License

MIT License Copyright (c) 2023 stdatadict authors
