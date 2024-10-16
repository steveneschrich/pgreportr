#' Format presentations as flextable output
#'
#' @description Parses a data frame of presentation information into a
#'  formatted presentation suitable for displaying in a [flextable::flextable()].
#'
#' @details This function is actually just a wrapper for the formatting routine,
#' see [format_presentation_as_flextable()] for details. However, there is a bit
#' of boxing/unboxing to deal with, which this function manages. Use this
#' function when you have a data frame full of presentations to format.
#'
#' @param x A data frame consisting of presentation fields (and possibly other things).
#'
#' @return A list of formatted presentations.
#' @export
#'
#' @examples
format_presentations_as_flextable <- function(x) {
  purrr::pmap(x, format_presentation_as_paragraph) |>
    unlist(recursive=FALSE)
}

#' Format a presentation in flextable style
#'
#' @description Formats a single presentation as a flextable paragraph.
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
#' # NB format_presentation_as_flextable_paragraph
format_presentation_as_paragraph <- function(...) {
  x <- rlang::list2(...)
  ref <- dplyr::bind_rows(
    format_presenters(x[["presenters"]], format_presenters_function = format_presenters_as_flextable_chunk),
    tibble::tribble(
      ~txt, ~italic, ~bold,
      ".", FALSE, FALSE,
      format.Date(x[["presentation_date"]]," (%Y, %b %d). "), FALSE, FALSE,
      x[["title"]], TRUE, FALSE,
      "[presentation]. ", FALSE, FALSE,
      x[["name_meeting"]], FALSE, FALSE,
      ", ",FALSE, FALSE,
      x[["location"]], FALSE, FALSE,
      ".", FALSE, FALSE
    )
  ) |>

    # Apparently a paragraph is a bunch of rows of chunks, wrapped in a list and paragraph class.
    # But there might be more, this is a bit sketchy. I found the seq_index in the as_paragraph
    # code.
    flextable::chunk_dataframe() |>
    dplyr::mutate(seq_index = dplyr::row_number()) |>
    list() |>
    magrittr::set_class("paragraph")

  ref

}
#  glue::glue_data(.x, "{Authors}. {title}, {journal}:{pub_citation_vol},{pub_citation_pg}. PMID: {pmid}, {pmcid}")




