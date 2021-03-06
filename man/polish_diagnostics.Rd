% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polish_diagnostics.R
\name{polish_diagnostics}
\alias{polish_diagnostics}
\title{Reorganize Diagnostics data}
\usage{
polish_diagnostics(diagnostics_tbl, run_date_fmt = c("mdy"))
}
\arguments{
\item{diagnostics_tbl}{Diagnostics data in tibble format}

\item{run_date_fmt}{Date format used to specify run date
(for example, 08-01-2020 for August 1, 2020)}
}
\value{
A (better) reorganized diagnostics_tbl
}
\description{
Reorganize Diagnostics data
}
