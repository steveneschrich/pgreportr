% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{how_many}
\alias{how_many}
\title{Number of true values in logical array}
\usage{
how_many(.x, negate = FALSE)
}
\arguments{
\item{.x}{A logical vector to count up true values from}

\item{negate}{(FALSE) Should FALSE values be counted instead of TRUE}
}
\value{
The number of TRUE (or FALSE if \code{negate}) values in the vector \code{.x}
}
\description{
Return the count of the number of TRUE values from a vector
as a convenience function.
}
\details{
Often we use a logical test across a vector to find out
how many elements meet a specific criteria. For instance,

\if{html}{\out{<div class="sourceCode">}}\preformatted{length(which(x>4))
}\if{html}{\out{</div>}}

This function is a convenience function to slightly
simplify that syntax since it comes up so much. Using the example
above,

\if{html}{\out{<div class="sourceCode">}}\preformatted{how_many(x>4)
}\if{html}{\out{</div>}}
}
\examples{
\dontrun{
how_many(iris$Sepal.Length>5)
}
}
