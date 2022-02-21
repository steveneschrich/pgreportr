#' import.R
#'
#' General-purposes routines for importing data from REDCap.
#'
#' @name import
#'
NULL


#' Retrieve REDCap data from the server
#'
#' @description Using the REDCapR library, import the raw and labeled versions of
#' a project. Also download the data dictionary.
#'
#' @param uri
#' @param token
#'
#' @return
#' @export
#' @examples
read_redcap_data_online <- function(uri, token) {
  ds_raw <- REDCapR::redcap_read(redcap_uri = uri, token = token, raw_or_label = "raw")
  ds_label <- REDCapR::redcap_read(redcap_uri = uri, token = token, raw_or_label = "label")

  data_dictionary <- REDCapR::redcap_metadata_read(redcap_uri=uri, token=token)

  stopifnot(ds_raw$success & ds_label$success & data_dictionary$success)

  dictionary <- data_dictionary$data %>%
    dplyr::mutate(Levels = split_choices(select_choices_or_calculations))

  list(raw = ds_raw$data, label = ds_label$data, dictionary = dictionary)
}


#' Convert checkbox fields to indicator variables
#'
#' A checkbox field consists of many different fields with values Checked
#' and Unchecked. This function converts this to more human-readable form.
#'
#' @details This function performs several related tasks to transform a
#' checkbox (set of columns) into several human-readable columns. It is
#' highly customized for the specific data input and not a general-purpose
#' function.
#'
#' @param .x Data frame to operate on
#' @param raw_prefix The checkbox variable prefix (excluding the ___) in the original table
#' @param indicator_prefix The prefix to use in for human-readable flags. It will be prepended
#' to the variable names, with a '_' separating it.
#'
#' @return Data frame with new columns representing human-readable indicators.
#'
#' @examples
.convert_checkbox_to_flags <- function(.x, raw_prefix, indicator_prefix, dictionary) {

  .x %>%
    # Change core attribution columns to value, not checkbox status
    dplyr::mutate(dplyr::across(dplyr::starts_with(raw_prefix),
                                ~dplyr::recode(
                                  .x,
                                  "Checked" =map_checkbox_index_to_label(
                                    dplyr::cur_column(),
                                    dictionary
                                  ),
                                  .default = NA_character_
                                )
    )) %>%
    # Rename checkbox field to include label, not index.
    dplyr::rename_with(
      rename_field_from_checkbox_index,
      .cols = dplyr::starts_with(raw_prefix),
      dictionary
    ) %>%

    dplyr::mutate(dplyr::across(
      dplyr::starts_with(raw_prefix),
      ~!is.na(.x),
      .names="{indicator_prefix}_{stringr::str_remove(.col,paste0(raw_prefix,'___'))}"
    ))

}

#' Determine if any authors are ESI's
#'
#' @description This function provides a way, in mutate, to test if any
#' of the authors are ESI's.
#'
#' @details It is a little tricky, since the authors are nested, to test
#' if any of the authors are ESI's. This code is private to the library
#' since the whole purpose of the function is to create an indicator
#' variable so that we don't need to do this regularly.
#'
#' @param .x
#'
#' @return
#'
#' @examples
.is_esi_related <- function(.x) {
  purrr::map_lgl(.x, function(.y) {
    .y %>%
      dplyr::filter(`Partnership Role` == "ESI") %>%
      nrow() > 0
  })
}

#' Annotate date(s) to program grant year
#'
#' @description This function operates on a date or list of dates, returning
#' the corresponding program grant year (e.g., Y01).
#'
#' @param .x A date or vector of dates to map.
#'
#' @return A vector of characters representing program grant years.
#'
#' @examples
.pg_year <- function(.x) {
  factor(
    convert_date_to_pg_year(.x), levels=print_pg_years()
  )
}


#' Derive creator name and roles as single text
#'
#' A creator (investigator or author) can have a partnership role and a
#' specific role. This function formats the information for output.
#'
#' @details
#' A creator is a person associated with a work (grant or publication). There
#' can be one or
#' more creators, and they may have different roles on
#' each work. Additionally, the creator has a partnership role which
#' may change over time. Thus, each work has a tibble associated with the
#' investigators for that work and include descriptions of their name,
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
#'
.derive_creator_summary<-function(name, partnership_role, role=NA, ...) {

       sprintf("%s%s%s%s",format_investigator_name(name, ...),
               ifelse(!is.na(partnership_role), "*", ""),
               ifelse(is.na(role), "", paste0(" (",role,")")),
               ifelse(is_esi_investigator(partnership_role), " [ESI]","")
       )


}

#' Split multiple choices into discrete fields
#'
#' @description A selection is a series of choices that can be selected. This
#' function extracts them as a tibble (since each choice is itself coded).
#'
#' @param s A string to split into choices (vectorized).
#'
#' @return A tibble of `Value` and `Label` fields for all choices.
#'
#'
#' @examples
split_choices <- function(s) {
  purrr::map(stringr::str_split(s, "\\|"), function(x) {
    split_levels(x)
  })
}

#' Split choice levels in discrete columns
#'
#' @description REDCap choices are in the form of N, VALUE. This separates out
#' the two components, trims them and returns them as a tibble.
#'
#' @param s A string to split into Value and Label (vectorized).
#'
#' @return A tibble with `Value` and `Label` fields.
#'
#' @examples
split_levels <- function(s) {
  stringr::str_split_fixed(s, pattern = ",", n = 2) %>%
    magrittr::set_colnames(c("Value", "Label")) %>%
    tibble::as_tibble() %>%
    dplyr::filter(.data$Value != "") %>%
    dplyr::mutate(Value = stringr::str_trim(.data$Value),
                  Label = stringr::str_trim(.data$Label)
    ) %>%
    tibble::deframe()
}




