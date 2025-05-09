% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datadict_tables.R
\name{create_datadict_tables}
\alias{create_datadict_tables}
\title{Create Data Dictionary Tables}
\usage{
create_datadict_tables(st_metadata, ...)
}
\arguments{
\item{st_metadata}{list of dataset that contains the secutrial meta data
tables.}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> additional options to pass to
internal functions.\if{html}{\out{<br>}}
- \code{invert_hidden} Boolean, define if the "hidden" variable in the
question/qs file is inverted by a secutrial bug.\if{html}{\out{<br>}}
If this option is omitted, this function assumes that the bug is active
when more than 70\% of all questions are marked as hidden.}
}
\value{
nested list of tibbles with the following structure:

\if{html}{\out{<div class="sourceCode">}}\preformatted{$ form_overview
 ...$ visit_forms
 ...$ casenode_forms
 ...$ sub_forms
 ...$ visitarms (only if the study contains multiple visit arms)
$ form_items
 ...$ <form1>
 ...$ <form2>
 ...
}\if{html}{\out{</div>}}
}
\description{
Create the following tables for use in the data dictionary file:
\itemize{
\item visit form overview
\item casenode forms overview
\item subform (repetition table) overview
\item itemtables for each form
}
}
\details{
All interim calculations such as the refined meta data tables or the
extracted study id are saved into the \link{intermediates} environment of this
package. Type:
\code{s(stdatadict::intermediates, all.names = TRUE)}
to get a list of all objects stored in this environment.
Access object from this environment:
\verb{stdatadict::intermediates$<ObjName>}.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
datadict_raw <- create_datadict_tables(st_metadata)
\dontshow{\}) # examplesIf}
}
