#' Filtering grants
#'
#' @description Filtering functions for grants
#'
#' @details The `pgreportr` package consists of three object types:
#' grants, publications and presentations. These object types are tables with
#' specific columns that have been created/formatted by `pgimportr`. When it
#' comes to reporting on these objects, we typically want to filter on the
#' status, the relevant dates, etc.
#'
#' The functions that being with `filter_grants` in this library are designed as
#' syntactic sugar, but often valuable sugar. Rather than having to remember
#' which specific fields to filter on, or the specific logic built into the
#' filtering process, these functions provide it. Importantly, over time
#' there will be an evolutionary process of incorporating logic that reflects the
#' needs of program grants in a way that is not immediately obvious from the
#' tables alone. This, we believe, is justification for the large number of
#' function options available for use.
#'
#' Of note, many of the functions that filter grants call more generic filtering
#' routines (such as [filter_between()]), but again are included for completeness.
#' It does perhaps make it confusing to figure out which functions to use, so
#' perhaps this should be readdressed.
#'
#' @name filter_grants
NULL



#' @describeIn filter_between Filter grants submitted between dates
#' @export
filter_grants_submitted_between<-function(.x, ...) {
  .x |>
    filter_grants_submitted() |>
    filter_between(var = submission_date, ...)
}

#' @describeIn filter_between Filter grants funded between dates
#' @export
filter_grants_funded_between <- function(.x, ...) {
  .x |>
    filter_grants_funded() |>
    filter_between(var = funding_start_date, ...)
}


#' @describeIn filter_in_year Filter grants submitted in year
#' @export
filter_grants_submitted_in_year <- function(.x, ...) {
  .x |>
    filter_grants_submitted() |>
    filter_in_year(var = pg_year_submitted, ...)
}

#' @describeIn filter_in_year Filter grants with funding starting in PG year yr.
#' @export
filter_grants_funded_in_year<-function(.x, ...) {
  .x |>
    filter_grants_funded() |>
    filter_in_year(var = pg_year_funded, ...)

}
