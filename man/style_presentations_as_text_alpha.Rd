% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/style_presentations.R
\name{style_presentations_as_text_alpha}
\alias{style_presentations_as_text_alpha}
\title{Format presentations/authors as text-printable output}
\usage{
style_presentations_as_text_alpha(.x, ...)
}
\arguments{
\item{.x}{The presentations table}

\item{...}{Other parameters to pass to format_presenters.}
}
\value{
A formatted table with a subset of columns for printing.
}
\description{
Format presentations/authors as text-printable output
}
\details{
The presentations consist of many fields that
can be compressed and rearranged for nice output. This function
attempts to format the presentations/authors in a way that is
suitable for text-based output (e.g., a spreadsheet).
}
