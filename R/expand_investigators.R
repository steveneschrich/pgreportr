#' Title
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
