#' Derive investigator name and roles as single text.
#'
#' An investigator can have a partnership role and a grant role. This
#' formats the information in many different ways.
#'
#' @details
#' An investigator is a person associated with a grant. There can be one or
#' more investigators on each grant, and they may have different roles on
#' each grant. Additionally, the investigator has a partnership role which
#' may change over time. Thus, each grant has a tibble associated with the
#' investigators for that grant and include descriptions of their name,
#' grant role and partnership role.
#'
#' This function produces a text summary of this information so that it could
#' be printed out succinctly. There are many different parameters that can be
#' tweaked (see `format_investigator_name` for many formatting options passed on
#' through this function).
#'
#' @seealso [format_investigator_name()] for flags to format the investigator name.
#'
#' @param investigators The investigators tibble for a grant.
#'
#' @return A formatted string (or vector of strings) representing investigators
#' @export
#'
derive_investigator_summary<-function(investigators, ...) {
  assertthat::assert_that(assertthat::has_name(investigators,
                                               c("Investigator","Role","Partnership Role")))
  with(investigators,

       sprintf("%s%s%s",format_investigator_name(Investigator, ...),
               ifelse(is.na(Role), "", paste0(" (",Role,")")),
               ifelse(is_esi_investigator(`Partnership Role`), " [ESI]","")
       )
  )

}



#' Create a composite string for funding source (agency/program).
#'
#' This function formats the grant agency and funding program into
#' a combined string, with an optional slash if possible.
#'
#' @param grant_agency
#' @param funding_program
#'
#' @return
#' @export
#'
#' @examples
derive_funding_source<-function(grant_agency, funding_program) {
  sprintf("%s%s",
          grant_agency,
          ifelse(is.na(funding_program), "", paste0("/",funding_program)))
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
#' @export
#'
#' @examples
#'
#' derive_fiscal_year_summary("Y05","Y06")
#'
derive_fiscal_year_summary<-function(submitted, funded) {
  paste(sep="", submitted,
        ifelse(is.na(funded), "", paste(" (",funded,")",sep="")))
}




#' Add funding dates
#'
#' @param d Tibble representing the grant table.
#'
#' @return A modified tibble with Funding Start Date and Funding End Date.
#' @export
#'
#' @importFrom rlang .data
#' @examples
derive_funding_dates<-function(d) {
  d %>%
    dplyr::mutate(`Funding Start Date` =
                    convert_date(split_date_range(.data$`project_period`)[,1])) %>%
    dplyr::mutate(`Funding End Date` =
                    convert_date(split_date_range(.data$`project_period`)[,2]))
}



