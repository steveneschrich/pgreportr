#' Return the last year of the Partnership Grant
#'
#' @return An integer represent the last grant year (e.g. 10).
#' @export
#'
#' @examples
pg_last_year<-function() {
  10
}


#' Return the text version of Parntership Grant years (e.g., Y01) for all years.
#'
#' @return A list of grant years in order.
#' @export
#'
print_pg_years<-function() {
  sprintf("Y%02d", 1:pg_last_year())
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
  dplyr::case_when(
    d < pg_start_date("Y01") ~ "Y00",
    d < pg_start_date("Y02") ~ "Y01",
    d < pg_start_date("Y03") ~ "Y02",
    d < pg_start_date("Y04") ~ "Y03",
    d < pg_start_date("Y05") ~ "Y04",
    d < pg_start_date("Y06") ~ "Y05",
    d < pg_start_date("Y07") ~ "Y06",
    d < pg_start_date("Y08") ~ "Y07",
    d < pg_start_date("Y09") ~ "Y08",
    d < pg_start_date("Y10") ~ "Y09",
    d < pg_start_date("Y11") ~ "Y10",
    TRUE ~ NA_character_
  )
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
  assertthat::assert_that(yr %in% pg_grant_years$pg_year)

  pg_grant_years %>%
    dplyr::filter(.data$pg_year == yr) %>%
    dplyr::pull(.data$pg_start_date)
}


pg_grant_years<-tibble::tribble(
  ~pg_year, ~pg_start_date,
  "Y01", lubridate::ymd("2012-09-01"),
  "Y02", lubridate::ymd("2013-09-01"),
  "Y03", lubridate::ymd("2014-09-01"),
  "Y04", lubridate::ymd("2015-09-01"),
  "Y05", lubridate::ymd("2016-09-01"),
  "Y06", lubridate::ymd("2017-09-25"),
  "Y07", lubridate::ymd("2018-09-01"),
  "Y08", lubridate::ymd("2019-09-01"),
  "Y09", lubridate::ymd("2020-09-01"),
  "Y10", lubridate::ymd("2021-09-01"),
  "Y11", lubridate::ymd("2022-09-01")
)
