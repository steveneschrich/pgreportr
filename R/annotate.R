#' Annotate if grant is joint (compute from grant table)
#'
#' A grant is a joint grant if investigators from the two
#' primary institutions (PHSU and MCC) have roles on the
#' grant. This function determines if a grant is a joint
#' grant, taking as input a grant table and adding a new
#' column to the table called "is_grant_joint" which is a
#' logical.
#'
#' @param grants The grants table
#'
#' @return A grants table with a new logical column, is_grant_joint.
#' @export
#'
annotate_is_grant_joint<-function(grants) {
  grants %>%
    tidyr::unnest(investigators) %>%
    dplyr::group_by(grant_id) %>%
    dplyr::summarize(is_grant_joint=any(Institution=="PHSU") & any(Institution=="MCC")) %>%
    dplyr::right_join(grants, by=c("grant_id"="grant_id"))
}



#' Annotate if grant is ESI-related (compute from grant table).
#'
#' An Early Stage investigator (ESI) is noted in the Partnership Role for an
#' individual associated with a grant. Note that a person's Partnership Role can
#' change over the course of different grant submissions. See is_esi_investigator()
#' on how this works.
#'
#' If an ESI is associated with a grant (in any grant role), then the grant
#' is considered an ESI-related grant.
#'
#'
#' @param grants The grants table
#'
#' @return A grants table with a new logical column, is_grant_esi_related.
#' @export
#'
#' @examples
annotate_is_grant_esi_related<-function(grants) {
  grants %>%
    tidyr::unnest(investigators) %>%
    dplyr::group_by(grant_id) %>%
    dplyr::summarize(`is_grant_esi_related`=any(`Partnership Role` == "ESI", na.rm=T)) %>%
    dplyr::right_join(grants, by=c("grant_id"="grant_id"))
}




#' Title
#'
#' @param grants
#'
#' @return
#' @export
#'
#' @examples
annotate_is_grant_funded <-function(grants) {
  grants %>%
    dplyr::mutate(`is_grant_funded` = (`Grant Status`=="Funded"))
}

rtype_grants <- c("R00","R01", "R03", "R15", "R21", "R25", "R50","R34")
annotate_is_grant_Rtype <- function(grants) {
  grants %>%
    dplyr::mutate(`is_grant_rtype` = (`Grant Agency` == "NIH" & `Grant Type` %in% rtype_grants))
}
#' Title
#'
#' @param grants
#'
#' @return
#' @export
#'
#' @examples
annotate_is_grant_pending_review <- function(grants) {
  grants %>%
    dplyr::mutate(`is_grant_pending_review` = (`Grant Status`=="Pending Review"))
}

#' Title
#'
#' @param grants
#'
#' @return
#' @export
#'
#' @examples
annotate_is_grant_not_funded <- function(grants) {
  grants %>%
    dplyr::mutate(`is_grant_not_funded` = (`Grant Status`=="Not Funded"))
}

#' Title
#'
#' @param grants
#'
#' @return
#' @export
#'
#' @examples
annotate_is_grant_in_preparation <- function(grants) {
  grants %>%
    dplyr::mutate(`is_grant_in_preparation` = (`Grant Status`=="In Preparation"))
}

#' Title
#'
#' @param grants
#'
#' @return
#' @export
#'
#' @examples
annotate_is_grant_submitted <- function(grants) {
  grants %>%
    dplyr::mutate(`is_grant_submitted` = (!`Grant Status`%in% c("In Preparation","Resubmission")))
}



#' Title
#'
#' @param grants
#'
#' @return
#' @export
#'
#' @examples
annotate_is_grant_phsu_cancer_related <- function(grants) {

  grants %>%
    dplyr::mutate(`is_grant_phsu_cancer_related` = (grant_tags___0==1))
}

#' Title
#'
#' @param grants
#'
#' @return
#' @export
#'
#' @examples
annotate_is_grant_moffitt_health_disparities <- function(grants) {

  grants %>%
    dplyr::mutate(`is_grant_moffitt_health_disparities` = (grant_tags___1==1))
}

#' Assign the Partnership Grant year to grant submitted, grant funded.
#'
#' @param d The tibble to operate on.
#'
#' @return A modified tibble with date submitted/funded as U54 years.
#' @export
#'
#' @importFrom rlang .data
#' @examples
annotate_pg_year<-function(d) {
  dplyr::mutate(d,
                `U54 Fiscal Year Submitted`=factor(
                  convert_date_to_pg_year(.data$`Submission Date`), levels=print_pg_years()),
                `U54 Fiscal Year Funded` = factor(
                  convert_date_to_pg_year(
                    dplyr::case_when(.data$`Grant Status`=="Funded" ~ .data$`Funding Start Date`)
                  ), levels=print_pg_years()),
                `U54 Fiscal Year Not Funded` = factor(
                  convert_date_to_pg_year(
                    dplyr::case_when(.data$`Grant Status`=="Not Funded" ~
                      lubridate::add_with_rollback(.data$`Submission Date`, base::months(6)))
                  ), levels = print_pg_years())
  )
}


