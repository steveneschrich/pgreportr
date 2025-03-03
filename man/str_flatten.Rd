% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{str_flatten}
\alias{str_flatten}
\alias{str_flatten_comma}
\title{Extended str_flatten function}
\usage{
str_flatten(
  s,
  collapse = "",
  last = NULL,
  na.rm = FALSE,
  rm.empty = TRUE,
  unique = TRUE
)

str_flatten_comma(
  s,
  last = NULL,
  na.rm = FALSE,
  rm.empty = TRUE,
  unique = TRUE
)
}
\arguments{
\item{s}{Input vector. Either a character vector or something coercible into one.}

\item{collapse}{String to insert between each piece. Defaults to "".}

\item{last}{Optional string to use in place of the final separator.}

\item{na.rm}{Remove missing values?}

\item{rm.empty}{Remove empty values? If TRUE, empty strings "" are removed. Sets na.rm=TRUE as well.}

\item{unique}{Uniquify string vector before collapsing.}
}
\value{
A string, i.e. a character vector of length 1.
}
\description{
A version of \code{\link[stringr:str_flatten]{stringr::str_flatten()}} with support for removing empty (zero-length)
strings prior to flattening.
}
\section{Functions}{
\itemize{
\item \code{str_flatten_comma()}: variation designed to mirror \code{\link[stringr:str_flatten]{stringr::str_flatten_comma()}} but with
removing empty strings.

}}
\seealso{
\code{\link[stringr:str_flatten]{stringr::str_flatten()}}

\code{\link[=str_flatten_comma]{str_flatten_comma()}}
}
