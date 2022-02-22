#' Title
#'
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
format_citation_as_text<-function(.x) {

  glue::glue_data(.x, "{Authors}. {title}, {journal}:{pub_citation_vol},{pub_citation_pg}. PMID: {pmid}, {pmcid}")
}
