% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_up_birth_year.R
\name{tidy_up_birth_year}
\alias{tidy_up_birth_year}
\title{Format the column containing birth year to remove invalid entries}
\usage{
tidy_up_birth_year(birth_year, max_year)
}
\arguments{
\item{birth_year}{Value from birth_year column}

\item{max_year}{Birth year above which the entry is considered invalid.
(Default: the year in which the sample was collected)}
}
\value{
Birth year as a numeric data type or NA
}
\description{
Format the column containing birth year to remove invalid entries
}
