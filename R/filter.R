#
# filter.R
#
# This contains code for filtering the grant table based on various criteria. It is easier
# to implement these as functions, rather than remember the specific variables to filter on.
# The logic can be implemented consistently this way.


#' Filter grant table d to submissions in Partnership Grant year yr.
#'
#' The grant table contains grants from all time periods. However, reporting
#' often occurs by specific Grant Year. The Grant Year is different from the
#' calendar year and fiscal year, and is specifically defined for the grant.
#'
#' Additionally, a grant can be submitted (more than once) prior to being funded
#' (if it ever does get funded). This function handles submissions, rather than
#' funding.
#'
#' Thus, this function filters a grant table to those that were submitted
#' in Partnership Grant year yr.
#'
#' NOTE: This function is vector-aware, meaning that multiple grant years can be
#' included.
#'
#' @param d Grant table
#' @param yr Partnership Grant Year to filter by.
#'
#' @return A grant table filtered to submissions from year yr.
#' @export
#'
#' @importFrom rlang .data
#' @examples
filter_grants_submitted_in_year<-function(d, yr) {
  assertthat::assert_that(has_name(d, "U54 Fiscal Year Submitted"))
  d %>%
    filter_grants_submitted() %>%
    dplyr::filter(.data$`U54 Fiscal Year Submitted` %in% yr)
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
#' @importFrom lubridate %within%
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
filter_grants_submitted_between<-function(d, start, end=format(Sys.time(), '%Y%m%d')) {
  d %>%
    filter_grants_submitted() %>%
    dplyr::filter(.data$`Submission Date` %within% lubridate::interval(start,end))

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

#' Filter to grants funded between start and end.
#'
#' This function filters the grants table to only grants that are funded between the
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
#' @importFrom lubridate %within%
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
filter_grants_funded_between<-function(d, start, end=format(Sys.time(), '%Y-%m-%d')) {
  filter_grants_funded(d) %>%
    dplyr::filter(.data$`Funding Start Date` %within% lubridate::interval(start,end))

}
#' Filter grant table d to grants with funding starting in PG year yr.
#'
#' @param d Grant table
#' @param yr Program Grant Year to filter by.
#'
#' @return A grant table filtered to grants starting in Program Grant year yr.
#' @export
#'
#' @importFrom rlang .data
#' @examples
filter_grants_funded_in_year<-function(d, yr) {
  dplyr::filter(filter_grants_funded(d), .data$`U54 Fiscal Year Funded`==yr)
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


#' Filter grant table on those grants with any core support.
#'
#' @param d  The grant table
#'
#' @return A filtered grant table with only grants having grant support.
#' @export
#'
#' @examples
filter_core_supported<-function(d) {
  dplyr::filter(d, !is.na(.data$`U54 Core Support`))
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

#' Filter grants table to PHSU Cancer-related grants.
#'
#' This filter will reduce the grant table to those grants with a Grant Tags
#' field including PHSU Cancer-related.
#'
#' @param grants A grants table
#'
#' @return A filtered grants table that is PHSU Cancer Related
#' @export
#'
#' @examples
filter_grants_phsu_cancer_related<-function(grants) {
    dplyr::filter(grants, !!is_grant_phsu_cancer_related())
}

