#' Custom PHSU-MCC U54 fields
#'
#' Routines customized for the PHSU-MCC U54 partnership.
#'
#' @rdname u54
#'
#'
#'
NULL
#> NULL

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



