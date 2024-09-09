#' Format publications as flextable output
#'
#' @description Parses a data frame of publication information into a
#'  formatted publication suitable for displaying in a [flextable::flextable()].
#'
#' @details This function is actually just a wrapper for the formatting routine,
#' see [format_publication_as_flextable()] for details. However, there is a bit
#' of boxing/unboxing to deal with, which this function manages. Use this
#' function when you have a data frame full of publications to format.
#'
#' @param x A data frame consisting of publication fields (and possibly other things).
#'
#' @return A list of formatted publications.
#' @export
#'
#' @examples
format_publications_as_flextable <- function(x) {
  purrr::pmap(x, format_publication_as_paragraph) |>
    unlist(recursive=FALSE)
}

#' Format a publication in flextable style
#'
#' @description Formats a single publication as a flextable paragraph.
#'
#' @details This function will create a [flextable::as_paragraph()] structure for
#' use in a flextable. It achieves this by creating a list of individual elements
#' and combining them together. These elements can be complex, formatted, etc.
#'
#' @param x A list with the necessary named elements.
#'
#' @return
#' @export
#'
#' @examples
format_publication_as_paragraph <- function(...) {
  x <- rlang::list2(...)
  ref <- list(
      format_authors_as_chunk(x$authors),
      list(txt=". "),
      list(txt=x$title),
      list(txt=". "),
      list(txt=x$journal, italic=TRUE),
      list(txt=": "),
      list(txt=format.Date(x$publication_date, "%Y %b;")),
      list(txt=as.character(x$pub_citation_vol)),
      list(txt=if (is.na(x$pub_citation_issue)) "" else sprintf("(%s)",x$pub_citation_issue)),
      list(txt=if (is.na(x$pub_citation_pg)) "" else paste0(":",x$pub_citation_pg)),
      list(txt=". PMID: "),
      list(txt=as.character(x$pmid)),
      list(txt=if (is.na(x$pmcid)) "" else ", "),
      list(txt=x$pmcid),
      list(txt=".")
  ) |>


    # Apparently a paragraph is a bunch of rows of chunks, wrapped in a list and paragraph class.
    # But there might be more, this is a bit sketchy. I found the seq_index in the as_paragraph
    # code.
    purrr::map_dfr(~.x) |>
    flextable::chunk_dataframe() |>
    dplyr::mutate(seq_index = dplyr::row_number()) |>
    list() |>
    magrittr::set_class("paragraph")

  ref

}
#  glue::glue_data(.x, "{Authors}. {title}, {journal}:{pub_citation_vol},{pub_citation_pg}. PMID: {pmid}, {pmcid}")



#' Format a data frame of authors as flextable output
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
format_authors_as_chunk <- function(x) {


  authors <- purrr::pmap(x, format_author_as_chunks) |>
    # Combine list of authors together into single data frame with commas between.
    purrr::map_dfr(~dplyr::bind_rows(.x, list(txt=", ")))  |>
    # Possible that some entries are empty, remove them.
    dplyr::filter(!is.na(txt)) |>
    # Trim off the last comma, should be an easier way somewhere
    dplyr::slice(-dplyr::n())

  authors
}

#' Format an author as a list of chunks for flextable
#'
#' @description Format an author (defined in a list) as a list of
#'  formatted chunks suitable for inserting into a [flextable::as_paragraph()].
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
format_author_as_chunks <- function(...) {
  x <- rlang::list2(...)

  # Start with the author name (standard format).
  # NOTE: The degree is not included in publications (from pubmed),
  # so these are excluded from the formatting.
  author <- pgimportr::format_name(x$author_name, use_degree = FALSE)

  # If author is not a partnership member, we just use the formatted name.
  if ( is.na(x$partnership_role))
    return(list(txt=author))

  # Otherwise, we need to add formatting based on role
  tibble::tribble(
   ~txt, ~bold, ~underlined, ~vertical.align,
   author, TRUE, x$partnership_role=="REC Trainee"|NA, NA_character_,
   dplyr::case_when(
     x[["isPartnershipRole_Former ESI"]] ~ "*",
     x[["isPartnershipRole_ESI"]] ~ "\u2020",
     .default = NA
   ),
   TRUE, NA, "superscript"
  )
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

  stringr::str_c(.x$author_summary, collapse = collapse)
}


#' Title
#'
#' @param author_list
#' @param format_authors_function
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
format_authors <- function(author_list, format_authors_function, ...) {
  # Special case: if the author list is empty then return an empty
  # string.
  if (length(author_list)==1 & rlang::is_empty(author_list[[1]]))
    return("")

  annotated_authors<-purrr::map_chr(author_list, format_authors_function, ...)

  annotated_authors
}
