% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_investigators.R
\name{collapse_investigators}
\alias{collapse_investigators}
\title{Collapse investigators to a single string}
\usage{
collapse_investigators(grants)
}
\arguments{
\item{grants}{the grants tibble}
}
\value{
A grants tibble with a new column (Investigators).
}
\description{
There are (potentially) multiple investigators per grant. This function will
reduce this to a single text string per grant, adding it to the grants table as the
\code{Investigators} field.
}
\details{
Given the grants table, the current implementation is to have investigators (a tibble)
nested inside of the grant tibble. This is convenient to retain the dplyr semantics,
but at some point the investigators have to be collapsed to a single string. This is that
function.

The job here is to take the investigators out, format the investigators per the style
guidelines, then store the resulting strings back into the grants table.
}
