







#' Collapse investigator list into string (deprecated)
#'
#' For a given tibble of investigators, assuming it represents a single group,
#' create a named list consisting of a single string representing all of the investigators
#' (separated by sep). The name of the list is the either a variable in the data or a
#' text string.
#'
#' @details
#' The primary purpose of this function is to collapse a tibble of investigators on the
#' summary_field (NB: should this actually just be a function?) into a single string,
#' separated by `sep`. The string value can also have a name associated with it, given
#' by `label`. If `label` is a variable in the investigator tibble and all values of that
#' variable are the same, then that value will be used. Some example is needed.
#'
#' For instance, if `label` is `Institution` and 1) this is a variable in the tibble and
#' 2) all rows have a single value for this variable (for instance, `MCC`), then the list
#' will be labeled as `MCC`=string.
#'
#' The label field allows you to specify one of the tibble columns to use as the label, or
#' if it's not found, then it is assumed to be a string to use for this purpose.
#'
#' @param investigators The investigators to collapse to a string.
#' @param label A field name of the tibble or a fixed string for the list name.
#' @param sep The string to use in separating individual investigators.
#' @return
#' @export
#'
#' @examples
collapse_investigators<-function(investigators, label="", collapse=TRUE, sep=";",
                                 summary_field="Investigator Summary") {
  summary_field<-dplyr::enquo(summary_field)

  # label, if grouped, is a tibble with a single value to use (assuming group by
  # used only variable for grouping).
  list_name<-ifelse(tibble::is_tibble(label),
                    label %>% dplyr::pull(1) %>% as.character(),
                    label)
  # Try to get the label from the tibble, otherwise use it as text.
  #list_name<-tryCatch({
  #  unique(as.character(dplyr::pull(investigators, !! enquo(label))))
  #}, error=function(cond){
  #  return(label)
  #})
  assertthat::assert_that(length(list_name)==1, msg="Label provided not unique in tibble.")

  list_values<-investigators %>%
    arrange_investigators_by_role() %>%
    dplyr::pull(!! summary_field)

  if ( collapse ) {
    list_values %>%
      stringr::str_c(collapse=sep)
  }

  stats::setNames(list(list_values), nm=list_name)
}

