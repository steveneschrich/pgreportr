#' Filtering presentations
#'
#' @description Filtering functions for presentations
#'  * [filter_presentations_between()]
#'  * [filter_presentations_in_year()]
#'  * [filter_presentations_current_esi_related()]
#'  * [filter_presentations_former_esi_related()]
#'
#' @details The `pgreportr` package consists of three object types:
#' grants, publications and presentations. These object types are tables with
#' specific columns that have been created/formatted by `pgimportr`. When it
#' comes to reporting on these objects, we typically want to filter on the
#' status, the relevant dates, etc.
#'
#' The functions that being with `filter_presentations` in this library are designed as
#' syntactic sugar, but often valuable sugar. Rather than having to remember
#' which specific fields to filter on, or the specific logic built into the
#' filtering process, these functions provide it. Importantly, over time
#' there will be an evolutionary process of incorporating logic that reflects the
#' needs of program grants in a way that is not immediately obvious from the
#' tables alone. This, we believe, is justification for the large number of
#' function options available for use.
#'
#' Of note, many of the functions that filter presentations call more generic filtering
#' routines (such as [filter_between()]), but again are included for completeness.
#' It does perhaps make it confusing to figure out which functions to use, so
#' perhaps this should be readdressed.
#'
#' @name filter_presentations
NULL



#' Filter presentations between two dates
#' @seealso
#'  * [filter_between()] for generic date filtering.
#'  * [filter_presentations()] for presentation-based filtering.
#' @inherit filter_presentations
#' @export
filter_presentations_between <- function(.x, ...) {
  filter_between(.x, var = presentation_date, ...)
}

#' Filter presentations in Program Grant year
#' @seealso
#'  * [filter_presentations()] for presentations-based filtering.
#'  * [filter_in_year()] for generic year-based filtering.
#' @inherit filter_presentations
#' @export
filter_presentations_in_year <- function(.x, ...) {
  filter_in_year(var = pg_year_presented, ...)
}



#' Filter presentations to 'current ESI' related status
#' @seealso
#'  * [filter_presentations()] for presentations-based filtering.
#'  * [is_presentation_current_esi_related()] for ESI-related status.
#'  * [filter_presentations_former_esi_related()] for former ESI-related status.
#' @inherit filter_presentations
#' @export
filter_presentations_current_esi_related <- function(.x) {
  dplyr::filter(.x, !!is_presentation_current_esi_related())
}

#' Filter presentations to 'former ESI' related status
#' @seealso
#'  * [filter_presentations()] for presentations-based filtering.
#'  * [is_presentation_former_esi_related()] for ESI-related status.
#'  * [filter_presentations_current_esi_related()] for current ESI-related status.
#' @inherit filter_presentations
#' @export
filter_presentations_former_esi_related <- function(.x) {
  dplyr::filter(.x, !!is_presentation_former_esi_related())
}
