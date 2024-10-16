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
#' # NB format_publication_as_flextable_paragraph
format_publication_as_paragraph <- function(...) {
  x <- rlang::list2(...)
  ref <- list(
      format_authors(x[["authors"]], format_authors_function = format_authors_as_flextable_chunk),
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




