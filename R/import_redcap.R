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
import_redcap_data<-function(f) {

  res<-read_redcap_data(f) %>%
    clean_redcap_data() %>%
    convert_redcap_data() %>%
    dplyr::select(!tidyselect::starts_with("is_"), tidyselect::starts_with("is_"))
}


#' Read RedCAP data into memory.
#'
#' Given that this data is in R format, it is actually a script
#' to interpret. However, we do a few things to that script to
#' make it easier to process.
#'
#' @param f The filename to read from.
#'
#' @return A tibble representing the redcap data.
#' @export
#'
#' @examples
read_redcap_data<-function(f) {
  assertthat::assert_that(!rlang::is_empty(f))
  message(sprintf("Loading redcap data from %s.", f))
  d<-readLines(f)

  # Wrap d in a function call, so that can evaluate it without respect to
  # the global environment
  d<-append(d,c(".internal_redcap_load<-function() {") , after=1)

  # One of the first commands is to clear all objects in the namespace. This
  # is good in general, but less helpful embedded in a larger program.
  rm_command<-grep("rm\\(list=ls\\(\\)\\)", d)
  stopifnot(length(rm_command) == 1 && rm_command > 0)

  d<-d[-rm_command]
  stopifnot(length(grep("rm(list=ls())",d))==0)

  # Next, when we read the csv file it doesn't have the option to be in another directory.
  # Here, we'll assume it's in the same place as f, so prepend f's path.
  rl<-grep("data=read.csv", d)
  d[rl]<-sub("read.csv\\('", sprintf("read.csv\\('%s%s", dirname(f), .Platform$file.sep ), d[rl])

  # This ends the function by returning the loaded data
  d<-append(d, c("data }"))

  # Now we can define the loader function, then call it. Local keeps it in
  # the function's environment (rather than global).
  source(exprs=parse(text=d), local=TRUE)

  res<-get(".internal_redcap_load")()

  res
}


#' Clean the imported redcap file for later use.
#'
#' The default is to create factors for variables and to
#' have labels that represent the questions. This library
#' is for reporting of the data in the database rather than
#' keeping the specific question text. Therefore, the
#' labels are stripped and the factors are converted to text.
#'
#'
#' @importFrom dplyr %>%
#' @param d  RedCAP data to clean.
#'
#' @return A cleaned version of the RedCAP data.
#' @export
#'
#' @examples
clean_redcap_data<-function(d) {
  # We strip out the labels to make it easier later.
  # TODO: translate labels (question text) to variables prefixed by something so that
  # they are available.
  question_text<-labels(d)
  d<-purrr::map_dfc(d, function(y) {
    class(y)<-setdiff(class(y), 'labelled')
    attr(y, 'label') <- NULL
    y
  }) %>%
    # Start by just converting everything to characters, not factors.
    dplyr::mutate(dplyr::across(where(~is.factor(.x) || is.logical(.x)), as.character)) %>%

    # Clean up things that are "" (or Other) to NA, for coalescing later.
    dplyr::mutate(dplyr::across(.cols=tidyselect::everything(),.fns=~dplyr::na_if(.,""))) %>%
    dplyr::mutate(dplyr::across(.cols=tidyselect::everything(),.fns=~dplyr::na_if(.,"Other")))

  d
}

#' Convert the RedCAP data into the grant tibble.
#'
#'
#' @param d The RedCAP data to convert.
#'
#' @return A grant tibble.
#' @export
#'
#' @examples
convert_redcap_data<-function(d) {


  grants<-create_grant_table(d)
  investigators<-create_investigators_table(d)

  # This creates the combined table (with investigators as a nested table).
  grant_table<-dplyr::nest_join(grants, investigators, by=c("grant_id"="grant_id"),
                         name = "investigators")

  # Add indicators for various conditions
  grant_table <- grant_table %>%
    annotate_is_grant_joint() %>%
    annotate_is_grant_esi_related() %>%
    annotate_is_grant_funded() %>%
    annotate_is_grant_pending_review() %>%
    annotate_is_grant_not_funded() %>%
    annotate_is_grant_in_preparation() %>%
    annotate_is_grant_Rtype() %>%
    annotate_is_grant_submitted()



  grant_table
}






#' Convert grant tags to printable text.
#'
#' @param d Tibble of grants
#'
#' @return A modified tibble with Grant Tags and Grant Status fields.
#' @export
#'
#' @importFrom rlang .data
assign_grant_tags<-function(d) {

  d %>%
    # Same story with Grant tags, they are not labeled
    annotate_is_grant_phsu_cancer_related() %>%
    annotate_is_grant_moffitt_health_disparities() %>%

    # Create some text-based fields that can be united into a comma-separated list for printing.
    dplyr::mutate(
      `PHSU Cancer`=ifelse(.data$`is_grant_phsu_cancer_related`, "PHSU Cancer Related", NA),
      `Moffitt Health Disparities`=ifelse(.data$`is_grant_moffitt_health_disparities`, "Moffitt Health Disparities", NA)
    ) %>%
    tidyr::unite(col="Grant Tags", .data$`PHSU Cancer`, .data$`Moffitt Health Disparities`, sep=", ", na.rm=TRUE) %>%
    # Clean up tags field in case of empty strings.
    dplyr::mutate(`Grant Tags`=dplyr::na_if(.data$`Grant Tags`, "")) %>%




    # Remove grant tags since they have been renamed/cleaned up
    dplyr::select(-tidyselect::starts_with("grant_tags___"))
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
#' @export
#'
#' @importFrom rlang .data
#' @examples
create_grant_table<-function(data) {


  grants<-dplyr::filter(data, is.na(.data$redcap_repeat_instrument)) %>%

    remove_investigators_table() %>%

    # Rename variables for easier use
    dplyr::rename(grant_id=.data$record_id) %>%
    # Grant status is fine as is, so just rename
    dplyr::rename(`Grant Status`=.data$`grant_status.factor`) %>%
    dplyr::select(-.data$`grant_status`) %>%

    ################################################################################
  # Create variables for grant table. Each change is separated for readability.
  ################################################################################

  # Creating a lubridate-supported date allows arithmetic
  dplyr::mutate(`Submission Date`=lubridate::mdy(.data$date)) %>%
    dplyr::select(-.data$date) %>%

    # Create specific funding start/stop dates from the project_period
    derive_funding_dates() %>%

    # The Partnership Grant has fiscal years. Assign grant years.
    annotate_pg_year() %>%

    # The grant agency, program and mechanism should be formatted.
    format_funding_mechanism() %>%

    # Cores can be attributed to a grant submission
    assign_cores() %>%

    # Specific tags exist for grants, including Cancer-related, etc.
    assign_grant_tags() %>%

    dplyr::mutate(`U54 Year`=derive_fiscal_year_summary(`U54 Fiscal Year Submitted`, `U54 Fiscal Year Funded`))



  grants
}



#' Create a table specific for investigator information.
#'
#' Investigator data is embedded in the raw data as extra rows with
#' no grant information, but investigator information. We extract out
#' the investigator information and do a bit of data wrangling to clean
#' it up.
#'
#' The linkage between investigators and grants is the record_id (renamed to grant_id).
#'
#' @param data Raw source redcap data
#'
#' @return A tibble corresponding to investigators.
#' @export
#'
#' @examples
create_investigators_table<-function(data) {

  investigator_source_variables<-c(
    "investigator_name.factor",
    "investigator_institution.factor",
    "investigator_role",
    "investigator_partnership_role"
  )
  assertthat::assert_that(assertthat::not_empty(data),
                          assertthat::has_name(data,investigator_source_variables))

  # Start by filtering on having a redcap_repeat_instrument (meaning is has investigators)
  dplyr::filter(data, redcap_repeat_instrument=="investigators") %>%

    # Each of these fields has an "Other" option, so we coalesce.
    dplyr::mutate(`Investigator` = dplyr::coalesce(
      !!! dplyr::select(., investigator_other, investigator_name.factor))
    ) %>%
    dplyr::select(., -investigator_other, -investigator_name.factor) %>%

    dplyr::mutate(`Role` = dplyr::coalesce(
      !!! dplyr::select(., role_other, investigator_role.factor))
    ) %>%
    dplyr::select(., -role_other, -investigator_role.factor) %>%

    dplyr::mutate(`Institution` = dplyr::coalesce(
      !!! dplyr::select(., institution_other, investigator_institution.factor))
    ) %>%
    dplyr::select(., -institution_other, -investigator_institution.factor) %>%

    dplyr::mutate(`Partnership Role` = dplyr::coalesce(
      !!! dplyr::select(., partnership_role_other, investigator_partnership_role.factor))
    ) %>%
    dplyr::select(., -partnership_role_other, -investigator_partnership_role.factor) %>%

    # Clean up remaining variables.
    dplyr::rename(grant_id=record_id,
                  investigator_id=redcap_repeat_instance) %>%

    dplyr::mutate(`Role` = dplyr::recode(`Role`,
                                  "Principal Investigator" = "PI",
                                  "Co-Investigator" = "co-I"
    )) %>%

    dplyr::mutate(`Investigator Summary` = derive_investigator_summary(.)) %>%
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
#' @param d A tibble containing investigators and grants.
#'
#' @return A modified tibble with investigator entries removed.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
remove_investigators_table<-function(d) {
  # Remove investigator-level columns
  dplyr::select(d, -tidyselect::starts_with("investigator"),
                -.data$redcap_repeat_instrument,
                -.data$redcap_repeat_instrument.factor,
                -.data$grants_complete,
                -.data$grants_complete.factor,
                -.data$institution_other,
                -.data$partnership_role_other,
                -.data$role_other
  )
}





#' Format the funding agency, program and mechanism
#'
#' @param d A tibble representing the grants.
#'
#' @return A modified tibble with funding mechanism formatted.
#' @export
#'
#' @importFrom rlang .data
#' @examples
format_funding_mechanism<-function(d) {

  # The granting agency is spread out but only one unique.
  dplyr::mutate(d, `Grant Agency` = dplyr::coalesce(.data$agency_other, .data$agency.factor)) %>%
    dplyr::select(-.data$agency.factor, -.data$agency_other, -.data$agency) %>%

    # Funding program is also spread out (many more variables possible)
    dplyr::mutate(`Funding Program` = dplyr::coalesce(
      !!! dplyr::select(.,
                        tidyselect::ends_with("_other"),
                        tidyselect::ends_with("_prog.factor")))
    ) %>%
    dplyr::select(-tidyselect::ends_with("_prog.factor"),
                  -tidyselect::ends_with("_prog"),
                  -tidyselect::ends_with("_other")) %>%

    # Grant Type (Funding Mechanism) is similar, but with an extra variable or two.
    dplyr::mutate(`Grant Type` = dplyr::coalesce(
      !!! dplyr::select(.,
                        tidyselect::ends_with("_mech.factor"),
                        tidyselect::ends_with("other_fund_mech"),
                        tidyselect::ends_with("nih_other_2")))
    ) %>%
    dplyr::select(-tidyselect::ends_with("_mech.factor"),
                  -tidyselect::ends_with("other_fund_mech"),
                  -tidyselect::ends_with("mech"), -"nih_other_2") %>%

    # The final report will use particular names.
    dplyr::rename(`Title`=grant_title) %>%

    # Create a composite string for the funding source (agency/program)
    dplyr::mutate(`Source`=derive_funding_source(`Grant Agency`, `Funding Program`)) %>%
    dplyr::mutate(`Grant Summary`=format_grant_summary(.))


}


#' Title
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
format_grant_summary<-function(d) {
  with(d,
        sprintf("%s%s",
              Source,
              ifelse(is.na(`Grant Type`), "", paste0(" ",`Grant Type`)))
  )
}


#' Convert core assignments to merged field.
#'
#' @param g Grant table
#'
#' @return A modified tibble with U54 Core Support variable.
#' @export
#'
#' @importFrom rlang .data
#' @examples
assign_cores<-function(g) {
  # Cores are just check boxes, but they are not labeled so we need to match them up.
  # Note: These mappings are in the Redcap codebook, but are hard-coded here for now.
  # TODO: Use the codebook to automatically create these mappings.
  g %>%
    # Remove the factor versions of the variables which are just "checked" and
    # "unchecked". The variable cores_support___1 has a 0/1 value which is equivalent
    # and easier to use.
    dplyr::select(-matches("cores_support___.\\.factor")) %>%

    # Start by renaming to appropriate descriptions.
    dplyr::rename(
      "PRBB Support"=cores_support___1,
      "QSC Support"=cores_support___2,
      "OC Support"=cores_support___3,
      "REC Support"=cores_support___4,
      "PEC Support"=cores_support___5,
      "Admin Core Support"=cores_support___6,
      "Research Project Support"=cores_support___7,
      "US-LACRN Supplement Support"=cores_support___8,
      "ESI (non-Partnership related) Support"=cores_support___9
    ) %>%
    # Create T/F flags for the support
    dplyr::mutate(
      `PRBB Support`=`PRBB Support` == 1,
      `QSC Support`=`QSC Support` == 1,
      `OC Support`=`OC Support` == 1,
      `REC Support` = `REC Support` == 1,
      `PEC Support` = `PEC Support` == 1,
      `Admin Core Support` = `Admin Core Support` == 1,
      `Research Project Support` = `Research Project Support` == 1,
      `US-LACRN Supplement Support` = `US-LACRN Supplement Support` == 1,
      `ESI (non-Partnership related) Support` = `ESI (non-Partnership related) Support` == 1,
    ) %>%
    # Create "tags" that can be coalesced into a single variable
    # TODO: Do this more elegantly.
    dplyr::mutate(
      `PRBBtag`=ifelse(.data$`PRBB Support`, "PRBB",NA),
      `QSCtag`=ifelse(.data$`QSC Support`, "QSC", NA),
      `OCtag`=ifelse(.data$`OC Support`, "OC", NA),
      `RECtag`=ifelse(.data$`REC Support`, "REC", NA),
      `PECtag`=ifelse(.data$`PEC Support`, "PEC", NA),
      `Admintag`=ifelse(.data$`Admin Core Support`, "Admin Core", NA),
      `Researchtag`=ifelse(.data$`Research Project Support`, "Research Project",NA),
      `US-LACRNtag`=ifelse(.data$`US-LACRN Supplement Support`, "US-LACRN Supplement",NA),
      `ESItag`=ifelse(.data$`ESI (non-Partnership related) Support`, "ESI (non-Partnership related)",NA)) %>%

    # Based on the tags, combine into a comma-separated list (for printing).
    tidyr::unite(col="U54 Core Support",
                 .data$`PRBBtag`,
                 .data$`QSCtag`,
                 .data$`OCtag`,
                 .data$`RECtag`,
                 .data$`PECtag`,
                 .data$`Admintag`,
                 sep=", ", na.rm=TRUE) %>%
    # Cores are separate from other types of support.
    tidyr::unite(col="Other Support",
                 .data$`Researchtag`,
                 .data$`US-LACRNtag`,
                 .data$`ESItag`,
                 sep=", ", na.rm=TRUE) %>%
    # Summarization is easier if empty strings are just NAs.
    dplyr::mutate(`U54 Core Support`=dplyr::na_if(.data$`U54 Core Support`, "")) %>%

    # Finally, remove extra columns not needed.
    dplyr::select(-tidyselect::starts_with("grant_supported_by_partnership_cores"))
}

