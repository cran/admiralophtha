% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_etdrs_to_logmar.R
\name{convert_etdrs_to_logmar}
\alias{convert_etdrs_to_logmar}
\title{ETDRS --> LogMAR conversion}
\usage{
convert_etdrs_to_logmar(value)
}
\arguments{
\item{value}{object containing ETDRS score to convert to logMAR}
}
\value{
The input value converted converted to logMAR units
}
\description{
Convert ETDRS score to LogMAR units
}
\details{
ETDRS value converted to logMAR as logMAR = -0.02 * ETDRS + 1.7
}
\examples{
library(tibble)
library(dplyr)
library(admiral)
library(admiraldev)

adbcva <- tribble(
  ~STUDYID, ~USUBJID, ~AVAL,
  "XXX001", "P01", 5,
  "XXX001", "P02", 10,
  "XXX001", "P03", 15,
  "XXX001", "P04", 20,
  "XXX001", "P05", 25
)

adbcva <- adbcva \%>\% mutate(AVAL = convert_etdrs_to_logmar(AVAL))
}
\author{
Rachel Linacre
}
\keyword{utils_fmt}
