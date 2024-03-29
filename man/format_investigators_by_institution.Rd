% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_investigators.R
\name{format_investigators_by_institution}
\alias{format_investigators_by_institution}
\title{Format investigators by group}
\usage{
format_investigators_by_institution(
  investigators_list,
  group,
  format_investigator_group,
  combine_investigator_groups,
  ...
)
}
\arguments{
\item{summary_field}{What field to use for the investigator (defaults to \verb{Investigator Summary})}
}
\description{
Given a list of investigator tibbles, format the investigators of each tibble by grouping.
}
\details{
This function assumes there is a list of tibbles provided, as one might expect to have
by extracting the \code{investigators} field from a \code{grants} tibble. This list of investigators
will be processed individually.

Each investigator tibble will be grouped by the \code{group} variable, then summarized according
to the \code{format_investigator_group} function (see format_investigator_group_as_* for ideas).
Each investigator group will then be combined together as a final output using the
\code{format_investigator_group_as_paragraph} function.

The final result of this function call should be a list of the same length as the input
(\code{investigators_list}) with each value being an appropriate representation of the investigators,
grouped by \code{group} and formatted by \code{format_investigator_group} and
\code{format_investigator_group_as_paragraph}.

NB: This might be better if we use a flag like "flextable" which would then find the appropriate
functions, assuming they exist. Then the twin functions don't have to be explicitly named.

NB: There is a bug in group_map that it doesn't allow for groupings by variables (e.g.,
the "group" variable, which when you group by would be !!group). It for some reason uses
the literal value as opposed to interpreting. So for now, ignore the group variable and
have explicit functions for each grouping variable of interest (probably not many anyway).
}
