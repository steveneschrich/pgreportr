#' Filtering publications
#'
#' @description Filtering functions for publications
#'  * [filter_publications_between()]
#'  * [filter_publications_in_year()]
#'  * [filter_publications_current_esi_related()]
#'  * [filter_publications_former_esi_related()]
#'
#' @details The `pgreportr` package consists of three object types:
#' grants, publications and presentations. These object types are tables with
#' specific columns that have been created/formatted by `pgimportr`. When it
#' comes to reporting on these objects, we typically want to filter on the
#' status, the relevant dates, etc.
#'
#' The functions that being with `filter_publications` in this library are designed as
#' syntactic sugar, but often valuable sugar. Rather than having to remember
#' which specific fields to filter on, or the specific logic built into the
#' filtering process, these functions provide it. Importantly, over time
#' there will be an evolutionary process of incorporating logic that reflects the
#' needs of program grants in a way that is not immediately obvious from the
#' tables alone. This, we believe, is justification for the large number of
#' function options available for use.
#'
#' Of note, many of the functions that filter publications call more generic filtering
#' routines (such as [filter_between()]), but again are included for completeness.
#' It does perhaps make it confusing to figure out which functions to use, so
#' perhaps this should be readdressed.
#'
#' @seealso
#'  * [filter()] for generic filtering functions.
#'
#' @name filter_publications
NULL



#' Filter publications published between two dates
#' @seealso
#'  * [filter_between()] for generic date filtering.
#'  * [filter_publications()] for publication-based filtering.
#' @inherit filter_presentations
#' @export
filter_publications_between <- function(.x, ...) {
  filter_between(.x, var = publication_date, ...)
}

#' @rdname filter_publications_between
filter_pubs_between <- filter_publications_between

#' Filter publications in Program Grant year yr
#' @seealso
#'  * [filter_in_year()] for generic program grant filtering.
#'  * [filter_publications()] for other publication-based filtering.
#' @inherit filter_presentations
#' @export
filter_publications_in_year <- function(.x, ...) {
  filter_in_year(var = pg_year_published, ...)
}
#' @rdname filter_publications_in_year
filter_pubs_in_year <- filter_publications_in_year


#' Filter publications to 'current ESI' related status
#' @seealso
#'  * [filter_publications()] for publications-based filtering.
#'  * [is_publication_current_esi_related()] for ESI-related status.
#' @inherit filter_publications
#' @export
filter_publications_current_esi_related <- function(.x) {
  dplyr::filter(.x, !!is_publication_current_esi_related())
}

#' @rdname filter_publications_current_esi_related
filter_pubs_current_esi_related <- filter_publications_current_esi_related

#' Filter publications to 'former ESI' related status
#' @seealso
#'  * [filter_publications()] for publications-based filtering.
#'  * [is_publication_former_esi_related()] for ESI-related status.
#'  * [is_publication_current_esi_related()] for current ESI status.
#' @export
filter_publications_former_esi_related <- function(.x) {
  dplyr::filter(.x, !!is_publication_former_esi_related())
}

#' @rdname filter_publications_former_esi_related
filter_pubs_former_esi_related <- filter_publications_former_esi_related

