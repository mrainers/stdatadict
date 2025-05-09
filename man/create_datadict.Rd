% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workbook_creators.R
\name{create_datadict}
\alias{create_datadict}
\title{Create A data dictionary}
\usage{
create_datadict(
  datadict_tables,
  title = NULL,
  subtitle = NULL,
  as_of_date = NULL,
  form_type_description = TRUE,
  doc_width = "G"
)
}
\arguments{
\item{datadict_tables}{a list with tables for the data dictionary,
created by \code{\link[=create_datadict_tables]{create_datadict_tables()}}}

\item{title}{(character) Title of the document. Default = NULL}

\item{subtitle}{(character) subtitle of the document,
e.g. researcher - study name.
Default = NULL'}

\item{as_of_date}{(date) when the data set was created. Default = NULL}

\item{form_type_description}{(logical), Should short explanation of the
different form types added to the form overview? Default = TRUE}

\item{doc_width}{(single character or numeric) Up to which column should the
titles and texts spread? Default = "G"}
}
\value{
An excel workbook
}
\description{
Wrapper function for creating a data dictionary. This function calls:
\code{\link[openxlsx2:wb_workbook]{openxlsx2::wb_workbook()}}, \code{\link[=add_form_overview]{add_form_overview()}} and \code{\link[=add_form_sheets]{add_form_sheets()}}
}
\examples{
\dontrun{create_datadict(datadict_tables)}
}
