% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_publications.R
\name{filter_publications_between}
\alias{filter_publications_between}
\alias{filter_pubs_between}
\title{Filter publications published between two dates}
\usage{
filter_publications_between(.x, ...)

filter_pubs_between(.x, ...)
}
\description{
Filtering functions for presentations
\itemize{
\item \code{\link[=filter_presentations_between]{filter_presentations_between()}}
\item \code{\link[=filter_presentations_in_year]{filter_presentations_in_year()}}
\item \code{\link[=filter_presentations_current_esi_related]{filter_presentations_current_esi_related()}}
\item \code{\link[=filter_presentations_former_esi_related]{filter_presentations_former_esi_related()}}
}
}
\details{
The \code{pgreportr} package consists of three object types:
grants, publications and presentations. These object types are tables with
specific columns that have been created/formatted by \code{pgimportr}. When it
comes to reporting on these objects, we typically want to filter on the
status, the relevant dates, etc.

The functions that being with \code{filter_presentations} in this library are designed as
syntactic sugar, but often valuable sugar. Rather than having to remember
which specific fields to filter on, or the specific logic built into the
filtering process, these functions provide it. Importantly, over time
there will be an evolutionary process of incorporating logic that reflects the
needs of program grants in a way that is not immediately obvious from the
tables alone. This, we believe, is justification for the large number of
function options available for use.

Of note, many of the functions that filter presentations call more generic filtering
routines (such as \code{\link[=filter_between]{filter_between()}}), but again are included for completeness.
It does perhaps make it confusing to figure out which functions to use, so
perhaps this should be readdressed.
}
\seealso{
\itemize{
\item \code{\link[=filter_between]{filter_between()}} for generic date filtering.
\item \code{\link[=filter_publications]{filter_publications()}} for publication-based filtering.
}
}
