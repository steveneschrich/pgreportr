% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_presentations.R
\name{format_presentations_as_flextable}
\alias{format_presentations_as_flextable}
\title{Format presentations as flextable output}
\usage{
format_presentations_as_flextable(x)
}
\arguments{
\item{x}{A data frame consisting of presentation fields (and possibly other things).}
}
\value{
A list of formatted presentations.
}
\description{
Parses a data frame of presentation information into a
formatted presentation suitable for displaying in a \code{\link[flextable:flextable]{flextable::flextable()}}.
}
\details{
This function is actually just a wrapper for the formatting routine,
see \code{\link[=format_presentation_as_flextable]{format_presentation_as_flextable()}} for details. However, there is a bit
of boxing/unboxing to deal with, which this function manages. Use this
function when you have a data frame full of presentations to format.
}
