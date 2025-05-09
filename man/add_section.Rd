% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workbook_creators.R
\name{add_section}
\alias{add_section}
\title{Add Section Title}
\usage{
add_section(wb, x, sheet = current_sheet(), row = 1, doc_width = "G")
}
\arguments{
\item{wb}{A Workbook object containing a worksheet.}

\item{x}{(character) The Section Title Text}

\item{sheet}{The worksheet to write to. Can be the worksheet index or name.}

\item{row}{in which row the section Titleshould be added. Default = 1}

\item{doc_width}{(single character or numeric) Up to which column should the
texts spread? Default = "G"}
}
\value{
A wbWorkbook, invisibly
}
\description{
Add Section Title
}
\examples{
wb <- openxlsx2::wb_workbook()
style_datadict(wb)
wb$add_worksheet()
add_section(wb, "My Section title", row = 3)
}
