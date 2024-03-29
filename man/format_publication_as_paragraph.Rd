% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_pubs.R
\name{format_publication_as_paragraph}
\alias{format_publication_as_paragraph}
\title{Format a publication in flextable style}
\usage{
format_publication_as_paragraph(...)
}
\arguments{
\item{x}{A list with the necessary named elements.}
}
\description{
Formats a single publication as a flextable paragraph.
}
\details{
This function will create a \code{\link[flextable:as_paragraph]{flextable::as_paragraph()}} structure for
use in a flextable. It achieves this by creating a list of individual elements
and combining them together. These elements can be complex, formatted, etc.
}
