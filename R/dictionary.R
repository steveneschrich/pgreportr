#' Dictionary routines
#'
#' There is a data dictionary (metadata) that can be extracted from REDCap,
#' documenting the instruments and variables. This is important for mapping
#' some fields that are either in coded, checkbox or other forms.
#'
#' The functions in `dictionary.R` provide ways to extract information from
#' the data dictionary and to translate survey data (using the dictionary)
#' into usable form.
#'
#' @name dictionary
NULL
#> NULL


#' Return the choices for a field
#'
#' @param dictionary
#' @param field
#'
#' @return
#'
#' @examples
get_field_choices <- function(dictionary, field) {
  dictionary %>%
    dplyr::filter( field_name == field ) %>%
    dplyr::pull("Levels") %>%
    unlist()
}
#' Map a checkbox variable into levels
#'
#' @param .x
#' @param dictionary
#' @param prefix
#'
#' @return
#'
#' @examples
map_checkbox <- function(.x, dictionary, target, prefix = "manu_supported_cores") {

  choices <- get_field_choices(dictionary, prefix)
  .x %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with(prefix),
        map_checkbox_choice,
        choices
      )) %>%
    tidyr::unite(!!target,dplyr::starts_with(prefix), sep=", ", na.rm=TRUE)
}


#' Map checkbox choice to label
#'
#' @description A checkbox choice is a field that can be either Checked
#' or Unchecked. Transform this entry to the corresponding label text.
#'
#' @details In REDCap, a checkbox is represented as a series of fields
#' with the choice index appended. That is, it is of the form
#' ```
#' checkbox___1
#' ```
#' The data dictionary (metadata) provides a lookup of what 1 is labeled. The
#' content of the field is NA, Checked or Unchecked.
#'
#' This function will take a column of data from a tibble assumed to be in
#' checkbox form. It requires a tibble of choices, to map the Checked
#' value to a corresponding Label value.
#'
#' @param .x
#' @param .choices
#'
#' @return
#'
#' @examples
map_checkbox_choice<-function(.x, .choices) {

  # NA out the Unchecked option
  .x <- dplyr::na_if(.x, "Unchecked")
  # The value of the column selected embeds the index that can be mapped
  # to .choices.
  n <- .choices[stringr::str_extract(dplyr::cur_column(), "(?<=___)\\d+")]

  ifelse(.x == "Checked",n,"")

}


#' Title
#'
#' @param s
#' @param dictionary
#'
#' @return
#'
#' @examples
map_checkbox_index_to_label <- function(s, dictionary) {
  n <- extract_checkbox_index(s)

  label <- get_field_choices(dictionary, unique(stringr::str_remove(s,"___\\d+")))

  mapped_labels <- label[n]

  unname(mapped_labels)
}

#' Title
#'
#' @param s
#'
#' @return
#'
#' @examples
extract_checkbox_index <- function(s) {
  stringr::str_extract(s, "(?<=___)\\d+")
}

#' Title
#'
#' @param x
#' @param dictionary
#'
#' @return
#'
#' @examples
rename_field_from_checkbox_index <- function(x, dictionary) {
  n <- map_checkbox_index_to_label(x, dictionary)

  stringr::str_replace(x, "___\\d+",paste0("___",n))
}


