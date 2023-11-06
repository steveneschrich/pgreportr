#' import
#'
#' @description General-purposes routines for importing data
#' from REDCap.
#'
#' @details The `pgreportr` library manages two different object
#' types: grants and publications. The overall process is to import
#' this data directly from REDCap (using the [REDCapR::redcap_read()])
#' function. And then perform a number of data manipulations on the
#' imported data to make it suitable for later reporting.
#'
#' Therefore each individual import: [import_grants()]
#' and [import_publications()] will call [read_redcap_data_online()]
#' and then do followup processing. See the help items for
#' [import_grants()] and [import_publications()] for specifics.
#'
#' Where possible, common functionality, code and terminology is used
#' between objects. For instance, the concept of a program grant
#' year, creators (whether grant investigators or publication authors).
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

  dictionary <- .create_dictionary(data_dictionary)

  list(raw = ds_raw$data, label = ds_label$data, dictionary = dictionary)
}

#' Create dictionary from download object
#'
#' @param .x Download object (from [REDCapR::redcap_metadata_read()]).
#'
#' @return
#' @export
#'
#' @examples
.create_dictionary <- function(.x) {
  stopifnot(rlang::has_name(.x, "data"))

  dplyr::mutate(
    .x$data,
    Levels = ifelse(
      field_type %in% c("dropdown","checkbox"),
      split_choices(select_choices_or_calculations),
      NA
    )
  )
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

  stopifnot(length(dplyr::starts_with(raw_prefix, vars=colnames(.x)))>0)

  .x |>
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
    )) |>
    # Rename checkbox field to include label, not index.
    dplyr::rename_with(
      .fn = \(cn) { rename_field_from_checkbox_index(cn, dictionary) },
      .cols = dplyr::starts_with(raw_prefix)
    ) |>

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
#' @details This code is vectorized, which can be a little confusing in the context
#' of embedded index/value lists. Briefly, a choice (here) is of the form:
#' ```
#' "1, Choice A | 2, Choice B | 3, Choice C"
#' ```
#'
#' This function will take the choice text and convert it to a list of choices:
#' ```
#' [1] "1, Choice A" "2, Choice B"
#' [3] "3, Choice C"
#' ```
#'
#' This choice can be split apart into a named list with [split_choice_levels()]. That is,
#' ```
#'
#' ```
#'
#' This function will take multiple character vectors with embedded choice text,
#' extract the individual choices and convert each character vector a named list.
#'
#' @note If the input is a single choice text blob, then a single named vector
#' is returned. Otherwise, it is a list of named vectors (corresponding to the
#' input length).
#'
#' @param s A string to split into choices (vectorized).
#'
#' @return A tibble of `Value` and `Label` fields for all choices.
#'
#' @export
#' @examples
split_choices <- function(s) {

  # Extract text elements (choices) separated by '|' and then call
  # split levels on the list of individual
  choices <- stringi::stri_split_fixed(s, "|") |>
    purrr::map(split_choice_levels)

  # Special case, if only a single element return the contents (not the list)
  if (length(choices) == 1)
    choices[[1]]
  else
    choices
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
split_choice_levels <- function(s) {

  if (all(is.na(s)))
    return(NA)
  if (!methods::is(s, "character"))
    s <- purrr::map_chr(s, 1)

  # Split on commas and clean up leading/trailing spaces. Then, list_transpose
  # will give us two vectors: names and values.
  choices <- stringi::stri_trim_both(s) |>
    stringi::stri_remove_empty() |>
    stringi::stri_split_fixed(pattern=",", omit_empty = TRUE,n=2) |>
    purrr::map(stringi::stri_trim_both) |>
    purrr::list_transpose()

  rlang::set_names(choices[[2L]], as.character(choices[[1L]]))

}




