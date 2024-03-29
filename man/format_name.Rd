% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_investigators.R
\name{format_name}
\alias{format_name}
\alias{format_investigator_name}
\alias{format_author_name}
\title{Format investigator name}
\usage{
format_name(
  n,
  use_degree = TRUE,
  use_first_name_only = FALSE,
  use_initials = TRUE,
  use_period_after_initials = TRUE,
  use_last_name_first = FALSE,
  ...
)

format_investigator_name(...)

format_author_name(...)
}
\arguments{
\item{n}{String representing the investigator name}

\item{use_degree}{Should the degree be printed in the name?}

\item{use_first_name_only}{Should only the first given name be used?}

\item{use_initials}{Should only initials of the given name(s) be used?}

\item{use_period_after_initials}{Should a period follow given name initials?}

\item{use_last_name_first}{Should it be last name, first?}
}
\value{
A shortened name. If a vector is provided, a vector of shortened names is returned.
}
\description{
For reporting, the investigator full name is often not needed because it takes up too much space.
To uniquely identify a person, we use the convention of \verb{GivenNameInitials. LastName, PhD}
for instance.
}
\details{
The data for pgreportr uses full names for investigators. Shortening these names to first initials
is tricky since people may have multiple given names (i.e., middle names). This code takes all names
except the last name (tokenized by whitespace) and abbreviates them, followed by a period. The last
name is then added and returned.

The degree (in the form of ,degree) is another variation. This code handles it's presence
or absence. You can use the use_degree flag to print it out (or not).

So, for instance
in the case of

\if{html}{\out{<div class="sourceCode">}}\preformatted{     John James Smith
}\if{html}{\out{</div>}}

his formatted name would be

\if{html}{\out{<div class="sourceCode">}}\preformatted{     JJ. Smith
}\if{html}{\out{</div>}}

Note that this function supports either a single name or a vector of names, and will transform
accordingly.

Of note, while this code is reasonably competent it is by no means bullet-proof. You would need
to check the results to verify it's worked as desired.

TODO: There is bug if someone's name is John (Joe) Smith. The parens screw up a regex or
something.
}
\section{Functions}{
\itemize{
\item \code{format_investigator_name()}: Format investigator

\item \code{format_author_name()}: Format author

}}
