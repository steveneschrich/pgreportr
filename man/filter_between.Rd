% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R, R/filter_grants.R
\name{filter_between}
\alias{filter_between}
\alias{filter_grants_submitted_between}
\alias{filter_grants_funded_between}
\title{Filter records between dates}
\usage{
filter_between(.x, var, start, end = format(Sys.time(), "\%Y\%m\%d"))

filter_grants_submitted_between(.x, ...)

filter_grants_funded_between(.x, ...)
}
\arguments{
\item{.x}{The table to filter (can be any data frame)}

\item{var}{The variable to filter dates on (changes between types).}

\item{start}{Start date (YYYY-MM-DD)}

\item{end}{End date (YYYY-MM-DD) default: current date}
}
\value{
A table filtered on start/end dates for \code{var}.
}
\description{
This function filters a reporting table, based on the
variable \code{var}, to keep records with dates between \code{start} and \code{end}.
}
\details{
This function filters a reporting table (grants, publications, presentations)
to only those between the specified start and end date. The dates must be
input as YYYY-MM-DD format.

If you are not sure what fields to use when filtering dates, consider using
the \code{filter_grants_}, \code{filter_publications_}, \code{filter_presentations_} functions
which provide that information directly.
}
\section{Functions}{
\itemize{
\item \code{filter_grants_submitted_between()}: Filter grants submitted between dates

\item \code{filter_grants_funded_between()}: Filter grants funded between dates

}}
\note{
If you do not specify an end date then today's date is used.

The dates are inclusive (meaning they include the date specified).
}
\seealso{
\link{filter}
}
