#' Filtering grants and publications
#'
#'
#' @details This contains code for filtering the grant table based on
#' various criteria. It is easier
#' to implement these as functions, rather than remember the specific variables to filter on.
#' The logic can be implemented consistently this way.
#'
#' @name filter
NULL
#> NULL


#' Filter records between dates
#'
#' @description This function filters a reporting table, based on the
#' variable `var`, to keep records with dates between `start` and `end`.
#'
#' @details
#'
#' This function filters the grants table to only grants that are submitted between the
#' specified start and end date. The dates must be input as YYYY-MM-DD format.
#'
#' Note that
#' if you do not specify an end date then today's date is used.
#'
#' Note that the dates are inclusive (meaning they include the date specified).
#'
#' @param .x The table to filter
#' @param start Start date (YYYY-MM-DD)
#' @param end End date (YYYY-MM-DD) default: current date
#'
#' @return A table filtered on start/end dates for `var`.
#'
#' @importFrom lubridate %within%
#' @importFrom dplyr %>%
#'
#' @export
#' @examples
filter_between <- function(.x, var, start, end = format(Sys.time(), '%Y%m%d')) {
  dplyr::filter(.x, {{ var }} %within% lubridate::interval(start,end))
}

#' @describeIn filter_between Filter grants submitted between dates
#' @export
filter_grants_submitted_between<-function(.x, ...) {
  .x %>%
    filter_grants_submitted() %>%
    filter_between(var = `Submission Date`, ...)
}

#' @describeIn filter_between Filter grants funded between dates
#' @export
filter_grants_funded_between <- function(.x, ...) {
  .x %>%
    filter_grants_funded() %>%
    filter_between(var = `Funding Start Date`, ...)
}

#' @describeIn filter_between Filter publications published between dates
#' @export
filter_pubs_between <- function(.x, ...) {
  filter_between(var = `Publication Date`, ...)
}



#' Filter records in Partnership Grant year yr.
#'
#' Records are from all time periods. However, reporting
#' often occurs by specific Grant Year. The Grant Year is different from the
#' calendar year and fiscal year, and is specifically defined for the
#' partnership grant.
#'
#' Thus, this function filters records to those that were submitted
#' in Partnership Grant year yr.
#'
#' NOTE: This function is vector-aware, meaning that multiple grant years can be
#' included.
#'
#' @param d tibble of records
#' @param var Variable to use for grant year
#' @param yr Partnership Grant Year to filter by.
#'
#' @return A tibble filtered to submissions from year yr.
#' @export
#'
#' @importFrom rlang .data
#' @examples
filter_in_year <- function(.x, var, yr = print_pg_years() ) {
  dplyr::filter(.x, {{ var }} %in% yr)
}


#' @describeIn filter_in_year Filter grants submitted in year
#' @export
filter_grants_submitted_in_year <- function(.x, ...) {
  .x %>%
    filter_grants_submitted() %>%
    filter_in_year(var = `U54 Fiscal Year Submitted`, ...)
}

#' @describeIn filter_in_year Filter grants with funding starting in PG year yr.
#' @export
filter_grants_funded_in_year<-function(.x, ...) {
  .x %>%
    filter_grants_funded() %>%
    filter_in_year(var = `U54 Fiscal Year Funded`, ...)

}

#' @describeIn filter_in_year Filter publications in Partnership Grant year yr
#' @export
filter_pubs_in_year <- function(.x, ...) {
  filter_in_year(var = `U54 Year Published`, ...)
}



#' Filter to grants submitted between start and end.
#'
#' This function filters the grants table to only grants that are submitted between the
#' specified start and end date. The dates must be input as YYYY-MM-DD format.
#'
#' Note that
#' if you do not specify an end date then today's date is used.
#'
#' Note that the dates are inclusive (meaning they include the date specified).
#'
#' @param d The grants table
#' @param start Start date (YYYY-MM-DD)
#' @param end End date (YYYY-MM-DD)
#'
#' @return A filtered grants table.
#' @export
#'
#' @examples
filter_grants_submitted<-function(d) {
  dplyr::filter(d, !!is_grant_submitted())
}





#' Filter grant table to grants that are ESI-related.
#'
#' @param d A grant table
#'
#' @return A filtered grant table with only ESI-related grants.
#' @export
#'
#' @importFrom rlang .data
#' @examples
filter_grants_esi_related<-function(d) {
  dplyr::filter(d, !!is_grant_esi_related())
}

#' Filter grant list to grants not funded.
#'
#' @param d The grant table.
#'
#' @return A table of grants not funded.
#' @export
#'
#' @importFrom rlang .data
#' @examples
filter_grants_not_funded<-function(d) {
  dplyr::filter(d, !!is_grant_not_funded())
}

#' Title
#'
#' @param d
#'
#' @return
#' @export
#'
#' @importFrom rlang .data
#' @examples
filter_grants_funded<-function(d) {
  dplyr::filter(d, !!is_grant_funded())
}


#' Filter the grants table to those in preparation.
#'
#' The grant table includes all grants (unless previously filtered). Some
#' grants are in preparation, under review, funded, etc. This is a filter
#' to limit to those in preparation.
#'
#' @param d The grants table from pgreportr
#'
#' @return A filtered grants table, limited to those grants in preparation.
#' @export
#'
filter_grants_in_preparation<-function(d) {
  dplyr::filter(d, !!is_grant_in_preparation())
}

#' Filter the grants table to those Pending Review.
#'
#' The grant table includes all grants (unless previously filtered). Some
#' grants are in preparation, under review, funded, etc. This is a filter
#' to limit to those pending review.
#
#' @param d The grants table from pgreportr
#'
#' @return A filtered grants table, limited to those grants in pending review.
#' @export
#'
filter_grants_pending_review<-function(d) {
  dplyr::filter(d, !! is_grant_pending_review())
}



#' Title
#'
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
filter_supported <- function(.x, var) {
  dplyr::filter(.x, !!is_supported())
}

#' Filter table on those with any core support.
#'
#' @param d  The grant table
#'
#' @return A filtered grant table with only grants having grant support.
#' @export
#'
#' @examples
filter_core_supported <- function(.x) {
  dplyr::filter(.x, !!is_core_supported())
}

#' @describeIn filter_core_supported  Filter grants with core support
#' @export
filter_grants_core_supported<-function(.x) {
  filter_core_supported(.x)
}

#' @describeIn filter_core_supported Filter pubs with core support
#' @export
filter_pubs_core_supported <- function(.x) {
  filter_core_supported(.x)
}

#' Title
#'
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
filter_other_support <- function(.x) {
  dplyr::filter(.x, !!is_other_support())
}




#' Filter grants table to only grants with investigators from both MCC and PHSU.
#'
#' @param grants A grants table
#'
#' @return A filtered grants table that consists of joint grants
#' @export
#'
#' @examples
filter_grants_joint<-function(grants) {
  dplyr::filter(grants, !!is_grant_joint())
}



#' Filter grants table to only grants that are R-type.
#'
#' @param grants A grants table
#'
#' @return A filtered grants table that consists of R-type grants
#' @export
#'
#' @examples
filter_grants_rtype<-function(grants) {
  dplyr::filter(grants, !!is_grant_rtype())
}





#' Title
#'
#' @param .x
#' @param tag
#'
#' @return
#' @export
#'
#' @examples
filter_by_tag <- function(.x, tag) {
  dplyr::filter(.x, !!has_tag(tag))
}
#' Title
#'
#' @param .x
#' @param support
#'
#' @return
#' @export
#'
#' @examples
filter_by_support <- function(.x, support) {
  dplyr::filter(.x, !!has_support(support))
}





#' Filter grant table d to grants that start funding within the U54 fiscal years grant_years.
#'
#' Starting from the grant table, filter the table to only include
#' grants that started funding in specific Program Grant fiscal years (listed as
#' Y01, Y02, ...). Note: grant funding can occur in a different
#' fiscal year from grant submission.
#'
#' @param d The grant table
#' @param grant_years A list of PG fiscal years (e.g., Y01, Y02) to filter to.
#'
#' @return A filtered grant table with only grants whose funding started in grant_years.
#' @export
#'
#' @examples
filter_u54_fiscal_years_funded<-function(d, grant_years) {
  dplyr::filter(d, .data$`U54 Fiscal Year Funded` %in% grant_years) %>%
    dplyr::mutate(`U54 Fiscal Year Funded` = factor(`U54 Fiscal Year Funded`))

}

#' Filter grant table d to grants submitted within the Program Grant fiscal years grant_years.
#'
#' Starting from the grant table, filter the table to only include
#' grants that were submitted in specific U54 fiscal years (listed as
#' Y01, Y02, ...). Note: grant funding can occur in a different
#' fiscal year from grant submission.
#'
#' @param d The grant table
#' @param grant_years A list of PG fiscal years (e.g., Y01, Y02) to filter to.
#'
#' @return A filtered grant table with only grants that were submitted in grant_years.
#' @export
#'
#' @examples
filter_u54_fiscal_years_submitted<-function(d, grant_years) {
  dplyr::filter(d, .data$`U54 Fiscal Year Submitted` %in% grant_years) %>%
    dplyr::mutate(`U54 Fiscal Year Submitted` = factor(`U54 Fiscal Year Submitted`))
}


