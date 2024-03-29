% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_grants.R
\name{.create_grant_table}
\alias{.create_grant_table}
\title{Create a table specific for grant information.}
\usage{
.create_grant_table(gl)
}
\arguments{
\item{data}{Raw source data}
}
\value{
A tibble corresponding to grants.
}
\description{
Grant information is the bulk of the raw data, with investigators
as extra rows (to be deleted here).
}
\details{
The data is formatted for data entry, not reporting, so there are
many different columns representing the same type of information.
Therefore, a significant amount of this function involves coalescing this
data into single columns.
}
