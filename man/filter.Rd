% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R
\name{filter}
\alias{filter}
\title{Filtering grants, publications and presentations}
\description{
Filtering functions for different objects. Generics include:
\itemize{
\item \code{\link[=filter_between]{filter_between()}}
\item \code{\link[=filter_in_year]{filter_in_year()}}
\item \code{\link[=filter_supported]{filter_supported()}}
\item \code{\link[=filter_core_supported]{filter_core_supported()}}
\item \code{\link[=filter_by_tag]{filter_by_tag()}}
}
}
\details{
The \code{pgreportr} package consists of three object types:
grants, publications and presentations. These object types are tables with
specific columns that have been created/formatted by \code{pgimportr}. When it
comes to reporting on these objects, we typically want to filter on the
status, the relevant dates, etc.

The functions that being with \code{filter_} in this library are designed as
syntactic sugar, but often valuable sugar. Rather than having to remember
which specific fields to filter on, or the specific logic built into the
filtering process, these functions provide it. Importantly, over time
there will be an evolutionary process of incorporating logic that reflects the
needs of program grants in a way that is not immediately obvious from the
tables alone. This, we believe, is justification for the large number of
function options available for use.
}
