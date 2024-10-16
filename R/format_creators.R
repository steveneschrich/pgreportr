#' Format creators of various types
#'
#' @description Creators exist across different reporting types, such as
#'  grants (investigators), publications (authors) and
#'  presentations (presenters, co-authors). These are all considered as
#'  creators and much of the code associated with formatting the list of these
#'  creators is the same.
#'
#' @details
#' Creators can be investigators, authors or presenters depending on the research
#' product. In any of these cases, however, the formatting is similar if not identical.
#'
#' First, it is important to remember that a grant, publication or presentation table
#' consist of lists of creators. That is grant "A" has an investigator table unique
#' to this grant. Grant "B" also has a unique investigator table. Thus, when calling
#' [format_authors()], [format_presenters()] or [format_investigators()], we should
#' expect a list of tables representing different research products.
#'
#' From a list of tables, a formatting function is used to format the creators within
#' a research product as a single entity. Currently, this could be text or a flextable
#' "chunk". A set of functions (`format_authors_as_...`, `format_presenters_as_...`,
#' `format_investigators_as_...`)
#' expect a table of creators which are then formatted individually
#' (`format_author_as_...`, `format_presenters_as_...`, `format_investigators_as_...`) and
#' combined.
#'
#' The approach taken for formatting creators is generally as follows (considering
#' the creator type author):
#' * [format_authors(authors_list, author_formatting_function)]
#' * [format_authors_as_flextable?]
#' * [format_authors_as_text()]
#' * [format_author_as_text()]
#'
#' @name format_creators
NULL



#' Derive creator name and roles as single text
#'
#' A creator (investigator, author, presenter) can have a partnership role and a
#' specific role. This function formats the information for output.
#'
#' @details
#' A creator is a person associated with a research product (grant, publication
#' or presentation). There can be one or more creators, and they may have different roles on
#' each product. Additionally, the creator has a partnership role which
#' may change over time. Thus, each research product has a tibble associated with the
#' creators for that work and include descriptions of their name,
#' role in the product (coI, author) and partnership role.
#'
#' This function produces a text summary of this information so that it could
#' be printed out succinctly. There are many different parameters that can be
#' tweaked (see `format_investigator_name` for many formatting options passed on
#' through this function).
#'
#' A creators table has one or more creators using standardized field
#' names (not yet an object, though). Given this table, we convert the fields
#' into a single string (per creator) then consolidate the creators into a single
#' string (using collapse to separate the entries).
#'
#' The end of result of this function is a single string consolidating the
#' creators in `.x`.
#' @seealso [format_investigator_name()] for flags to format the investigator name.
#'
#'
#' @export
format_creators_as_text <- function(.x, collapse = ", ", ...) {
  dplyr::mutate(
    .x,
    txt = sprintf("%s%s%s%s%s",
                  pgimportr::format_name(name, ...),
                  ifelse(!is.na(pg_role), "*", ""),
                  ifelse(is.na(role), "", paste0(" (",role,")")),
                  ifelse(is_Current_ESI, " [Current ESI]",""),
                  ifelse(is_Former_ESI, " [Former ESI]","")
    )
  ) |>
    dplyr::pull("txt") |>
    stringr::str_c(collapse = collapse)
}





#' Format a list of creators into flextable structure
#'
#' @description Given a table of creators (with the table consisting
#'  of multiple columns, including the name, title, role, etc), consolidate
#'  this to flextable-formatted output.
#'
#' @details Flextable output is tabular but can involve multiple lines
#'  of text per row of a table (to accommodate formatting, etc). This
#'  function will apply the formatting function to a creator table (all of the
#'  creators for a single research product) and then combine the flextable
#'  output into a single flextable "chunk" suitable for adding to a flextable.
#'
#' @param x
#' @param formatting_function
#'
#' @return
#' @export
#'
#' @examples
format_creators_as_flextable_chunk <- function(x, formatting_function) {

  # This could be a rowwise or a pmap, we chose pmap so the formatting function
  # must take dynamic dots consisting of all the columns. Not sure why this was
  # done.
  creators <- purrr::pmap(x, formatting_function)

  # Special case of no creators (strange but it happened), return an
  # empty txt data frame.
  if ( length(creators) == 0 ) return(tibble::tibble(txt=""))

  # Combine creators together with commas and then combine
  creators <- creators |>
    # Combine list of creators together into single data frame with commas between.
    purrr::map(\(.x) {dplyr::bind_rows(.x, list(txt=", "))}) |>
    purrr::list_rbind() |>
    #purrr::map_dfr(~dplyr::bind_rows(.x, list(txt=", ")))  |>
    # Possible that some entries are empty, remove them.
    dplyr::filter(!is.na(txt)) |>
    # Trim off the last comma, should be an easier way somewhere
    dplyr::slice_head(n=-1)

  creators
}

#' Format a data frame of authors as flextable chunk output
#'
#' @param x
#' @param formatting_function A function suitable for generating flextable chunk
#'  output for an author. Default is [format_author_as_flextable_chunks()].
#'
#' @return A flextable chunk (structured data frame)
#' @export
#'
#' @seealso
#'  * [format_creators_as_flextable_chunk()] for generic formatting function for creators.
format_authors_as_flextable_chunk <- function(x, formatting_function = format_author_as_flextable_chunk) {

  format_creators_as_flextable_chunk(x, formatting_function)

}

#' Format a single author as a list of flextable chunks
#'
#' @description Format an author as a table of
#'  formatted chunks suitable for inserting into a [flextable::as_paragraph()].
#'
#' @param ... A list of variables corresponding to the author (name, affiliation, etc).
#'
#' @return A data frame of
#' @export
#'
#' @examples
format_author_as_flextable_chunk <- function(...) {
  x <- rlang::list2(...)

  # The formatted chunk is two rows of information:
  # Row1: formatted name with optional bold/underlining
  # Row2: optional superscript character
  # NOTE: The degree is not included in publications (from pubmed),
  # Superscript for Current/Former ESI (bold superscript)
  # Bold name if any partnership role
  # Underlined name if REC Trainee
  ftchunk <- tibble::tibble(
    txt = c(
      pgimportr::format_name(x$author_name, use_degree = FALSE),
      dplyr::case_when(
        x[["isPartnershipRole_Former ESI"]] ~ "*",
        x[["isPartnershipRole_Current ESI"]] ~ "\u2020",
        .default = NA_character_
      )
    ),
    bold = c(
      !is.na(x$partnership_role),
      TRUE
    ),
    underlined = c(
      x$partnership_role=="REC Trainee"|NA,
      NA
    ),
    vertical.align = c(
      NA_character_,
      "superscript"
    )
  )

  ftchunk
}


#' Format authors for text
#'
#' Given an author table, format the output to include the list name
#' and values as a text-worthy chunk.
#'
#' @details
#'
#'
#' @param .x A table of authors
#' @param collapse Character string between authors. Default is ', '
#'
#' @return
#' @export
#'
#' @examples
format_authors_as_text<-function(.x,  collapse=", ") {
  creators <- tibble::tibble(
    name = .x[["author_name"]],
    pg_role = .x[["partnership_role"]],
    role = NA,
    is_Current_ESI = .x[["isPartnershipRole_Current ESI"]],
    is_Former_ESI = .x[["isPartnershipRole_Former ESI"]],
    use_degree=FALSE
  )
  format_creators_as_text(creators, collapse = collapse)

}


#' Format a list of author tables
#'
#' @description Given a list (or one) author tables, format the authors per
#' the provided formatting function. Formatting functions currently include
#' * [format_authors_as_text()]
#' * [format_authors_as_flextable()]
#'
#' @details
#'
#'
#' @export
format_authors <- function(author_list, format_authors_function, ...) {
  # Special case: if the author list is empty then return an empty
  # string.
  if (length(author_list)==1 & rlang::is_empty(author_list[[1]]))
    return("")

  # Special case: if the author_list is actually just a data frame (only one set of
  # authors), then enclose in a list so purrr works correctly.
  if ( is.data.frame(author_list))
    author_list <- list(author_list)

  annotated_authors<-purrr::map(author_list, format_authors_function, ...)

  # Weird hack since we can't coerce the above without knowing if it's text
  # or a flextable chunk. Need to rethink this.
  if ( all(purrr::map_lgl(annotated_authors, is.character)))
    annotated_authors <- unlist(annotated_authors)

  annotated_authors
}


#' Format a data frame of presenters as flextable chunk output
#'
#' @param x
#' @param formatting_function A function suitable for generating flextable chunk
#'  output for an author. Default is [format_presenter_as_flextable_chunks()].
#'
#' @return A flextable chunk (structured data frame)
#' @export
#'
#' @seealso
#'  * [format_creators_as_flextable_chunk()] for generic formatting function for creators.
format_presenters_as_flextable_chunk <- function(x, formatting_function = format_presenter_as_flextable_chunk) {

  format_creators_as_flextable_chunk(x, formatting_function)

}

#' Format a single presenter as a list of flextable chunks
#'
#' @description Format a presenter as a table of
#'  formatted chunks suitable for inserting into a [flextable::as_paragraph()].
#'
#' @param ... A list of variables corresponding to the presenter (name, affiliation, etc).
#'
#' @return A data frame of
#' @export
#'
#' @examples
format_presenter_as_flextable_chunk <- function(...) {
  x <- rlang::list2(...)

  # The formatted chunk is two rows of information:
  # Row1: formatted name with optional bold/underlining
  # Row2: optional superscript character
  # NOTE: The degree is not included in publications (from pubmed),
  # Superscript for Current/Former ESI (bold superscript)
  # Bold name if any partnership role
  # Underlined name if REC Trainee
  ftchunk <- tibble::tibble(
    txt = c(
      pgimportr::format_name(x$presenter_name, use_degree = FALSE),
      dplyr::case_when(
        x[["isPartnershipRole_Former ESI"]] ~ "*",
        x[["isPartnershipRole_Current ESI"]] ~ "\u2020",
        .default = NA_character_
      )
    ),
    bold = c(
      !is.na(x$partnership_role),
      TRUE
    ),
    underlined = c(
      x$partnership_role=="REC Trainee"|NA,
      NA
    ),
    vertical.align = c(
      NA_character_,
      "superscript"
    )
  )

  ftchunk
}


#' @export
format_presenters_as_text <- function(.x, collapse = ", ") {
  creators <- tibble::tibble(
    name = .x[["presenter_name"]],
    pg_role = .x[["partnership_role"]],
    role = NA,
    is_Current_ESI = .x[["isPartnershipRole_Current ESI"]],
    is_Former_ESI = .x[["isPartnershipRole_Former ESI"]],
    # Presentations do not include degress, so exclude these
    use_degree = FALSE
  )
  format_creators_as_text(creators, collapse = collapse)
}



#' Format a list of presenter tables
#'
#' @description Given a list (or one) presenter tables, format the presenters per
#' the provided formatting function. Formatting functions currently include
#' * [format_presenters_as_text()]
#' * [format_presenters_as_flextable()]
#'
#' @details
#'
#' @param presenter_list A list of presenter tables to format
#' @param format_presenters_function Function to format a presenters table into
#'  a single formatted unit (text or otherwise).
#' @param ... Other arguments to pass to [pgimportr::format_name()]
#'
#' @return A list (same length as presenter_list) of formatted strings representing the
#'  presenters.
#' @export
format_presenters <- function(presenter_list, format_presenters_function, ...) {
  # Special case: if the presenter list is empty then return an empty
  # string.
  if (length(presenter_list)==1 & rlang::is_empty(presenter_list[[1]]))
    return("")

  # Special case: if the presenter_list is actually just a data frame (only one set of
  # presenters), then enclose in a list so purrr works correctly.
  if ( is.data.frame(presenter_list))
    presenter_list <- list(presenter_list)

  # Format all list elements using the formatting function provided.
  annotated_presenters<-purrr::map(presenter_list, format_presenters_function, ...)


  # Weird hack since we can't coerce the above without knowing if it's text
  # or a flextable chunk. Need to rethink this.
  if ( all(purrr::map_lgl(annotated_presenters, is.character)))
    annotated_presenters <- unlist(annotated_presenters)


  annotated_presenters
}

