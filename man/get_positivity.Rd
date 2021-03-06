% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_positivity.R
\name{get_positivity}
\alias{get_positivity}
\title{Compute Weekly Test Positivity Rate (TPR)}
\usage{
get_positivity(
  sc_tbl,
  start_date = "2021-01-01",
  end_date = as.character(lubridate::date(lubridate::now())),
  n_days = 30,
  time_zone = "America/New_York"
)
}
\arguments{
\item{sc_tbl}{Sample Collection table from deidentifiedDB database}

\item{start_date}{Filter sample collection table for samples collected after
this date. Provide date as in "2021-08-01" for August 1, 2021}

\item{end_date}{Filter sample collection table for samples collected on or
before this date. Provide date as in "2021-08-01" for August 1, 2021}

\item{n_days}{Duration of time (in days) for which only the first collected
COVID-19 positive sample  for a patient is retained}

\item{time_zone}{Time zone for collection time (Default: "America/New_York")}
}
\value{
Tibble containing Weekly Test Positivity Rate (TPR) data after
excluding redundant COVID-19 positive samples
}
\description{
Compute Weekly Test Positivity Rate (TPR)
}
