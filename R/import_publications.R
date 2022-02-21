#' Import publications from REDCap database
#'
#' @param uri
#' @param token
#'
#' @return
#' @export
#'
#' @examples
import_publications <- function(uri, token) {

  redcap_tables <- read_redcap_data_online(uri, token)

  # Create the pubs and authors separately
  pubs <- .create_publication_table(redcap_tables)
  authors <- .create_authors_table(redcap_tables)

  # Join by the pub id, then nest the author table inside the authors variable
  pub_table <- dplyr::nest_join(pubs, authors, by="pub_id", name = "authors")

  # Add indicators for various conditions requiring both tables.
  pub_table <- pub_table %>%
    dplyr::mutate(
      is_esi_related = .is_esi_related(authors),
      is_support = !is.na(`U54 Support`),
      is_core_support = !is.na(`U54 Core Support`),
      is_other_support = !is.na(`U54 Other Support`)
    )

  pub_table
}





#' Title
#'
#' @param pl
#'
#' @return
#'
#' @examples
.create_publication_table <- function(pl) {
  pubs<- pl$label %>%
    .remove_authors() %>%

    # Rename variables for easier use
    dplyr::rename(pub_id=.data$record_id) %>%

    # Creating a lubridate-supported date allows arithmetic
    dplyr::mutate(`Publication Date`=lubridate::ymd(.data$date_publication),
                  date_publication=NULL) %>%

    # The Partnership Grant has fiscal years. Assign grant years.
    dplyr::mutate(`U54 Year Published`=.pg_year(`Publication Date`)) %>%

    # Convert checkbox to flag variables.
    .convert_checkbox_to_flags(
      raw_prefix = "manu_supported_cores",
      indicator_prefix = "isSupportedBy",
      dictionary = pl$dictionary
    ) %>%

    # Create consolidated fields (comma-separated)
    tidyr::unite(col = "U54 Core Support", sep = ", ", remove = FALSE, na.rm = TRUE,
                 paste0("manu_supported_cores___",
                        u54_cores)) %>%
    tidyr::unite(col = "U54 Other Support", sep = ", ", remove = FALSE, na.rm = TRUE,
                 paste0("manu_supported_cores___",
                        u54_othersupport)) %>%
    tidyr::unite(col = "U54 Support", sep = ", ", remove = FALSE, na.rm = TRUE,
                 paste0("manu_supported_cores___",
                        u54_support)) %>%


    # Convert checkbox to flag variables.
    .convert_checkbox_to_flags(
      raw_prefix = "manuscript_tags",
      indicator_prefix = "isTag",
      dictionary = pl$dictionary

    ) %>%

    # Create consolidated fields (comma-separated)
    tidyr::unite(col = "Tags", sep = ", ", remove = FALSE, na.rm = TRUE,
                 tidyr::starts_with("manuscript_tags___"))

}


#' Create a table specific for author information
#'
#' Author data is embedded in the raw data as extra rows with
#' no publication information, but author information. We extract out
#' the author information and do a bit of data wrangling to clean
#' it up.
#'
#' The linkage between authors and publications is the pub_id.
#'
#' @param .x Raw source redcap data
#'
#' @return A tibble corresponding to authors.
#'
#' @examples
.create_authors_table<-function(pl) {

  pl$label %>%
    # Start by filtering on having a redcap_repeat_instrument (meaning is has authors)
    .remove_pubs() %>%
    # Rename fields to human-readable form.
    dplyr::rename(
      `Author` = .data$author_name,
      `Institution` = .data$author_institution,
      `Partnership Role` = .data$author_partnership_role,
      pub_id = record_id,
      author_id = redcap_repeat_instance
    ) %>%
    # Remove redcap indicator field
    dplyr::select(-authors_complete) %>%
    # NA the None role, so it's not printed.
    dplyr::mutate(`Partnership Role` = dplyr::na_if(`Partnership Role`, "None")) %>%
    # Create author summary text for printing
    dplyr::mutate(`Author Summary` = .derive_creator_summary(Author, `Partnership Role`))

}


#' Remove publication fields from publication tibble
#'
#' @description The publication table includes rows (and columns)
#' representing authors (one row per author). This function keeps
#' the rows and columns belonging to authors specifically.
#'
#' @param d A tibble containing publications and authors.
#'
#' @return A modified tibble with publication entries removed.
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#'
#' @examples
.remove_pubs<-function(.x) {

  .x %>%
    # Remove publication rows (keep repeat instruments)
    dplyr::filter(.data$redcap_repeat_instrument == "Authors") %>%
    # Keep author columns
    dplyr::select(tidyselect::starts_with("author"),
                  record_id,
                  redcap_repeat_instance
    )
}


#' Remove publication fields from publication tibble
#'
#' @description The publication table includes rows (and columns)
#' representing authors (one row per author). This function keeps
#' the rows and columns belonging to authors specifically.
#'
#' @param d A tibble containing publications and authors.
#'
#' @return A modified tibble with publication entries removed.
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#'
#' @examples
.remove_authors<-function(.x) {

  .x %>%
    # Remove author rows (exclude repeat instruments)
    dplyr::filter(is.na(.data$redcap_repeat_instrument)) %>%
    # Remove author columns
    dplyr::select(-tidyselect::starts_with("author"),
                  -redcap_repeat_instance
    )
}
