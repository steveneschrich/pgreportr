#' Expand nested investigator fields
#'
#' @description The Investigator field is a nested tibble. This function expands the nested tibble,
#' creating multiple rows for the same grant with different investigators in each row.
#'
#' @details The [tidyr::unnest] function provides the ability to nest a tibble within a tibble field.
#' We use this to embed the investigator tibble into the grants tibble, so that filtering and counting
#' will proceed as normal. This is one way to accomplish a relational set of tables.
#'
#' However, when we need access to this embedded tibble, we can do two different things.
#'
#' First, we can use purrr::map to iterate over the embedded tibbles.
#'
#' Second, we can expand the tibble into many tibbles.
#'
#' This function accomplishes the second option.
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
expand_investigators<-function(d,...) {
  if ( nrow(d) == 0 )
    d %>% dplyr::bind_cols(tibble::tribble(
      ~investigator_id,~Investigator,~Institution,~Role,~"Partnership Role",~"Investigator Summary"
    ))

  else
    d %>% tidyr::unnest(...)
}
