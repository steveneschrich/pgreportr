#' Custom PHSU-MCC U54 fields
#'
#' Routines customized for the PHSU-MCC U54 partnership.
#'
#' @rdname u54
#' @name u54
#'
#'
NULL


#' Filter table to PHSU Cancer-related projects
#'
#' This filter will reduce the table to those entries that are PHSU Cancer related
#'
#' @param .x A  table
#'
#' @return A filtered grants table that is PHSU Cancer Related
#' @export
#'
#' @examples
filter_phsu_cancer_related <- function(.x) {
  filter_by_tag(.x, !!is_phsu_cancer_related())
}

#' @describeIn filter_phsu_cancer_related Filter pubs on  PHSU Cancer-related
#' @export
filter_pub_phsu_cancer_related <- function(.x) {
  filter_phsu_cancer_related(.x)
}

#' @describeIn filter_phsu_cancer_related Filter grants on PHSU Cancer-related
#' @export
filter_grants_phsu_cancer_related<-function(.x) {
  filter_phsu_cancer_related(.x)
}

#' Filter grants based on Moffitt Health Disparities
#'
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
filter_moffitt_health_disparities <- function(.x) {
  dplyr::filter(.x, !!is_moffitt_health_disparities())
}

#' @describeIn filter_moffitt_health_disparities Filter grants on Moffitt Health Disparities
#' @export
filter_pub_moffitt_health_disparities <- function(.x) {
  filter_moffitt_health_disparities(.x)
}

#' @describeIn filter_moffitt_health_disparities Filter grants on Moffitt Health Disparities
#' @export
filter_grants_moffitt_health_disparities <- function(.x) {
  filter_moffitt_health_disparities(.x)
}


#' Is the pub PHSU Cancer Related?
#'
#' This is an indicator if the pub is related to PHSU and Cancer (a criteria
#' for reporting). Return a vector of logicals indicating if the grants represented by that
#' field are PHSU Cancer-related or not (or the field name if .x  is not included).
#'
#'
#' @param .x A publication table or nothing (in which case the field name is returned).
#'
#' @return A vector of logical values indicating if the pub is PHSU Cancer Related or field name.
#' @export
#'
#' @example
is_phsu_cancer_related <- function(.x) {
  has_tag(.x, tag = "PHSU Cancer")
}

#' @describeIn is_phsu_cancer_related Is publication PHSU Cancer-related
#' @export
is_pub_phsu_cancer_related <- function(.x) {
  is_phsu_cancer_related(.x)
}

#' @describeIn is_phsu_cancer_related Is grant PHSU Cancer-related
#' @export
is_grant_phsu_cancer_related <- function(.x) {
  is_phsu_cancer_related(.x)
}



#' Title
#'
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
is_moffitt_health_disparities <- function(.x) {
  has_tag(.x, tag = "Moffitt Health Disparities")
}

#' @describeIn is_moffitt_health_disparities Is publication Moffitt Health-Disparities related
#' @export
is_pub_moffitt_health_disparities <- function(.x) {
  is_moffitt_health_disparities(.x)
}


#' @describeIn #' @describeIn is_moffitt_health_disparities Is grant Moffitt Health-Disparities related
#' @export
is_grant_moffitt_health_disparities <- function(.x) {
  is_moffitt_health_disparities(.x)
}



#' Return the last year of the Partnership Grant
#'
#' @return An string representing the last (max) grant year.
#' @export
#'
#' @examples
pg_last_year<-function() {
  max(pg_grant_years$year)
}


#' Return the text version of Parntership Grant years (e.g., Y01) for all years.
#'
#' @return A list of grant years in order.
#' @export
#'
print_pg_years<-function() {
  pg_grant_years$year
}

#' Convert a calendar date to a Partnership Grant year.
#'
#' Grant years are often not aligned to calendar years or to fiscal years.
#' They represent their own internal clock. Here, the Partnership Grant
#' is the reason for reporting on activity within the Partnership. Therefore,
#' it is important to track the Partnership Grant (pg) year.
#'
#' The logic implemented in this function is straight-forward. If the date is
#' less than the start date of a given pg year, then it is the previous
#' year. This works since each year is tested in increasing order.
#'
#' Note that dates starting earlier than Y01 start date are considered
#' Y00.
#'
#' @param d A lubridate-style date.
#'
#' @return A string representing the pg year, or NA if not defined.
#' @export
#'
#' @examples
#'
convert_date_to_pg_year<-function(d) {
  purrr::map_chr(d, year_from_date)
}

#' Title
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
year_from_date <- function(d) {
  if ( is.na(d) ) return(NA)
  y <- pg_grant_years$year[lubridate::`%within%`(lubridate::ymd(d), pg_grant_years$interval)]
  if (length(y) == 0 ) return(NA)

  y
}

#' Given a Partnership Grant funding year, return the start date of that year.
#'
#' @param yr The funding year - default is the initial grant year ("Y01")
#'
#' @return The start date (as a lubridate) of the funding year.
#' @export
#'
#'
pg_start_date<-function(yr="Y01") {
  assertthat::assert_that(yr %in% pg_grant_years$year)

  dplyr::filter(pg_grant_years, .data$year == yr) |>
    dplyr::pull(.data$start_date)
}


pg_grant_years<-tibble::tribble(
  ~year, ~start_date,
  "U56", "2000-01-01",
  "Y01", "2012-09-01",
  "Y02", "2013-09-01",
  "Y03", "2014-09-01",
  "Y04", "2015-09-01",
  "Y05", "2016-09-01",
  # Originally 2017-09-25 but for consistency, we will use 2017-09-01
  "Y06", "2017-09-01",
  "Y07", "2018-09-01",
  "Y08", "2019-09-01",
  "Y09", "2020-09-01",
  "Y10", "2021-09-01",
  "Y10S1", "2022-09-01",
  "YXX", "2023-09-01"
) |>
  dplyr::mutate(
    start_date = lubridate::ymd(start_date),
    end_date = dplyr::lead(start_date-1),
    interval = lubridate::interval(start = start_date, end = end_date)
  )





