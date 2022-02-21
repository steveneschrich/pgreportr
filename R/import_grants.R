#' Import REDCap export data into R for reporting.
#'
#' The REDCap data can be exported in R format. This consists of an
#' R script and corresponding CSV to read in. Labels are defined,
#' factors are created, etc.
#'
#' There are a few caveats to using this type of data which this function
#' overcomes. The result is a similar data structure, but cleaned up for use
#' in this library.
#'
#' @param f The filename for importing
#'
#' @return A tibble with grant data available.
#' @export
#'
#' @examples
import_grants<-function(uri, token) {

  redcap_tables <- read_redcap_data_online(uri, token)

  # Create the pubs and authors separately
  grants <- .create_grant_table(redcap_tables)
  investigators <- .create_investigators_table(redcap_tables)

  # Join by the grant id, then nest the investigator table inside the investigators variable
  grant_table<-dplyr::nest_join(grants, investigators, by="grant_id",
                                name = "investigators")


  # Add indicators for various conditions, some of which require both tables.
  grant_table <- grant_table %>%
    dplyr::mutate(
      is_grant_joint = .is_grant_joint(investigators),
      is_grant_funded = (`Grant Status`=="Funded"),
      is_grant_pending_review = (`Grant Status`=="Pending Review"),
      is_grant_not_funded = (`Grant Status`=="Not Funded"),
      is_grant_in_preparation = (`Grant Status`=="In Preparation"),
      is_grant_submitted = (!`Grant Status`%in% c("In Preparation","Resubmission")),
      is_grant_rtype = .is_grant_rtype(`Grant Agency`,`Grant Type`),
      is_esi_related = .is_esi_related(investigators),
      is_support = !is.na(`U54 Support`),
      is_core_support = !is.na(`U54 Core Support`),
      is_other_support = !is.na(`U54 Other Support`)
    )

  grant_table
}


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
#'
.is_grant_joint<-function(.x) {

  purrr::map_lgl(.x, function(.y) {
    any(.y$Institution == "PHSU") &
      any(.y$Institution == "MCC")
  })
}

.is_grant_rtype<-function(agency, type) {
    (agency == "NIH" & type %in% rtype_grants)
}


#' Create a table specific for grant information.
#'
#' Grant information is the bulk of the raw data, with investigators
#' as extra rows (to be deleted here).
#'
#' The data is formatted for data entry, not reporting, so there are
#' many different columns representing the same type of information.
#' Therefore, a significant amount of this function involves coalescing this
#' data into single columns.
#'
#' @param data Raw source data
#'
#' @return A tibble corresponding to grants.
#'
#' @importFrom rlang .data
#' @examples
.create_grant_table<-function(gl) {

  grants <- gl$label %>%
    .remove_investigators() %>%

    # Rename variables for easier use
    dplyr::rename(grant_id=.data$record_id) %>%

    # Grant status is fine as is, so just rename
    dplyr::rename(`Grant Status`=.data$`grant_status`) %>%

    # Creating a lubridate-supported date allows arithmetic
    dplyr::mutate(
      `Submission Date`=lubridate::ymd(.data$date),
      date = NULL
    ) %>%


    # Create specific funding start/stop dates from the project_period
    dplyr::mutate(`Funding Start Date` =
                    lubridate::mdy(split_date_range(.data$`project_period`)[,1])) %>%
    dplyr::mutate(`Funding End Date` =
                    lubridate::mdy(split_date_range(.data$`project_period`)[,2])) %>%

    # The Partnership Grant has fiscal years. Assign grant years.
    # The Fiscal Year Submitted is simple
    dplyr::mutate(`U54 Fiscal Year Submitted`=
                    .pg_year(.data$`Submission Date`)) %>%
    # The Fiscal Year Funded requires a Funded status, or else should be empty.
    dplyr::mutate(`U54 Fiscal Year Funded` =
                    .pg_year(
                      dplyr::case_when(.data$`Grant Status`=="Funded" ~ .data$`Funding Start Date`)
                    )) %>%
    # The Fiscal Year Not Funded is trickier. If the grant is not funded, then we assume that
    # the year not funded corresponds to 6 months after the Submission Date.
    dplyr::mutate(`U54 Fiscal Year Not Funded` =
                    .pg_year(
                      dplyr::case_when(.data$`Grant Status`=="Not Funded" ~
                                         lubridate::add_with_rollback(.data$`Submission Date`, base::months(6)))
                    )
    ) %>%


    # The grant agency, program and mechanism should be formatted.
    # TODO
    .format_funding_mechanism() %>%

    # Cores can be attributed to a grant submission
    # Convert checkbox to flag variables.
    .convert_checkbox_to_flags(
      raw_prefix = "cores_support",
      indicator_prefix = "isSupportedBy",
      dictionary = gl$dictionary
    ) %>%

    # Create consolidated fields (comma-separated)
    tidyr::unite(col = "U54 Core Support", sep = ", ", remove = FALSE, na.rm = TRUE,
                 paste0("cores_support___",
                        u54_cores)) %>%
    tidyr::unite(col = "U54 Other Support", sep = ", ", remove = FALSE, na.rm = TRUE,
                 paste0("cores_support___",
                        u54_othersupport)) %>%
    tidyr::unite(col = "U54 Support", sep = ", ", remove = FALSE, na.rm = TRUE,
                 paste0("cores_support___",
                        u54_support)) %>%


    # Convert checkbox to flag variables.
    .convert_checkbox_to_flags(
      raw_prefix = "grant_tags",
      indicator_prefix = "isTag",
      dictionary = gl$dictionary
    ) %>%

    # Create consolidated fields (comma-separated)
    tidyr::unite(col = "Tags", sep = ", ", remove = FALSE, na.rm = TRUE,
                 tidyr::starts_with("grant_tags___")) %>%

    # Create a summary U54 Year variable for use
    dplyr::mutate(`U54 Year`=.derive_fiscal_year_summary(`U54 Fiscal Year Submitted`, `U54 Fiscal Year Funded`))



  grants
}



#' Create a table specific for investigator information
#'
#' Investigator data is embedded in the raw data as extra rows with
#' no grant information, but investigator information. We extract out
#' the investigator information and do a bit of data wrangling to clean
#' it up.
#'
#' The linkage between investigators and grants is the record_id (renamed to grant_id).
#'
#' @param gl Raw source redcap data
#'
#' @return A tibble corresponding to investigators.
#'
#' @examples
.create_investigators_table<-function(gl) {

  gl$label %>%
    .remove_grants() %>%

    # Each of these fields has an "Other" option, so we coalesce.
    dplyr::mutate(`Investigator` = dplyr::coalesce(
      !!! dplyr::select(., .data$investigator_other, .data$investigator_name))
    ) %>%

    dplyr::mutate(`Role` = dplyr::coalesce(
      !!! dplyr::select(., role_other, investigator_role))
    ) %>%

    dplyr::mutate(`Institution` = dplyr::coalesce(
      !!! dplyr::select(., institution_other, investigator_institution))
    ) %>%

    dplyr::mutate(`Partnership Role` = dplyr::coalesce(
      !!! dplyr::select(., partnership_role_other, investigator_partnership_role))
    ) %>%

    # Clean up remaining variables.
    dplyr::rename(grant_id=record_id,
                  investigator_id=redcap_repeat_instance) %>%

    dplyr::mutate(`Role` = dplyr::recode(`Role`,
                                  "Principal Investigator" = "PI",
                                  "Co-Investigator" = "co-I"
    )) %>%

    dplyr::mutate(`Investigator Summary` = .derive_creator_summary(
      Investigator, `Partnership Role`, `Role`
    )) %>%

    # Select the order for the table
    dplyr::select(.data$grant_id,
                  .data$investigator_id,
                  .data$Investigator,
                  .data$Institution,
                  .data$Role,
                  .data$`Partnership Role`,
                  .data$`Investigator Summary`)
}


#' Remove investigator entries from a tibble.
#'
#' @param .x A tibble containing investigators and grants.
#'
#' @return A modified tibble with investigator entries removed.
#'
#' @importFrom rlang .data
#'
#' @examples
.remove_investigators <- function(.x) {

  .x %>%
    # Remove investigator rows (repeated instruments)
    dplyr::filter(is.na(.data$redcap_repeat_instrument)) %>%

    # Remove investigator-level columns
    dplyr::select(-tidyselect::starts_with("investigator_"),
                -.data$redcap_repeat_instrument,
                -.data$redcap_repeat_instance,
                -.data$grants_complete,
                -.data$investigators_complete,
                -.data$institution_other,
                -.data$partnership_role_other,
                -.data$role_other
  )
}

#' Remove grant entries from a tibble.
#'
#' @param .x A tibble containing investigators and grants.
#'
#' @return A modified tibble with grant entries removed.
#'
#' @importFrom rlang .data
#'
#' @examples
.remove_grants <- function(.x) {

  .x %>%
    # Remove investigator rows (repeated instruments)
    dplyr::filter(.data$redcap_repeat_instrument=="Investigators") %>%

    # Remove investigator-level columns
    dplyr::select(tidyselect::starts_with("investigator_"),
                  .data$redcap_repeat_instance,
                  .data$institution_other,
                  .data$partnership_role_other,
                  .data$role_other,
                  .data$record_id
    )
}

#' Format the funding agency, program and mechanism
#'
#' @param .x A tibble representing the grants.
#'
#' @return A modified tibble with funding mechanism formatted.
#'
#' @importFrom rlang .data
#' @examples
.format_funding_mechanism<-function(.x) {

  # The granting agency is spread out but only one unique.
  .x %>%
    dplyr::mutate(
      `Grant Agency` = dplyr::coalesce(.data$agency_other, .data$agency),
      agency = NULL,
      agency_other = NULL
    ) %>%

    # Funding program is also spread out (many more variables possible)
    dplyr::mutate(`Funding Program` = dplyr::coalesce(
      !!! dplyr::select(.,
                        tidyselect::ends_with("_other"),
                        tidyselect::ends_with("_prog")))
    ) %>%
    dplyr::select(-tidyselect::ends_with("_prog"),
                  -tidyselect::ends_with("_other")) %>%

    # Grant Type (Funding Mechanism) is similar, but with an extra variable or two.
    dplyr::mutate(`Grant Type` = dplyr::coalesce(
      !!! dplyr::select(.,
                        tidyselect::ends_with("_mech"),
                        tidyselect::ends_with("other_fund_mech"),
                        tidyselect::ends_with("nih_other_2")))
    ) %>%
    dplyr::select(
                  -tidyselect::ends_with("other_fund_mech"),
                  -tidyselect::ends_with("mech"), -"nih_other_2") %>%

    # The final report will use particular names.
    dplyr::rename(`Title`=grant_title) %>%

    # Create a composite string for the funding source (agency/program)
    dplyr::mutate(
      `Source` =  sprintf("%s%s",
                          `Grant Agency`,
                          ifelse(is.na(`Funding Program`), "", paste0("/",`Funding Program`)))) %>%

    dplyr::mutate(`Grant Summary` = sprintf("%s%s",
                                            Source,
                                            ifelse(is.na(`Grant Type`), "", paste0(" ",`Grant Type`))))


}



#' Format the fiscal year as submitted (funded).
#'
#' @details
#' Partnership grant years are numbered like Y05, Y06. These are important when
#' tracking outcomes in the grant. However, it can be a little confusing that there
#' is the date a grant was submitted vs the date a grant was funded. These can often
#' be years apart. This function takes both the year submitted (everyone has this) and
#' the year funded (not everyone has this) and formats a combination of the two.
#'
#' The format of the output is
#' ```
#' YrSubmitted (YrFunded)
#' ```
#'
#' @param submitted The year submitted
#' @param funded The year funded (or NA if not funded)
#'
#' @return A string combining the two
#'
#' @examples
#'
#' derive_fiscal_year_summary("Y05","Y06")
#'
.derive_fiscal_year_summary<-function(submitted, funded) {
  paste(sep="", submitted,
        ifelse(is.na(funded), "", paste(" (",funded,")",sep="")))
}


