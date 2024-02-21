#' Format grants/investigators to a text-printable output.
#'
#' @details
#' The grants and investigators consist of many fields that
#' can be compressed and rearranged for nice output. This function
#' attempts to format the grant/investigators in a way that is
#' suitable for text-based output (e.g., a spreadsheet).
#'
#' @param grants The grants table.
#' @param investigators The investigators table.
#'
#' @return A formatted table with a subset of columns for printing.
#' @export
#'
#' @examples
format_grants_text<-function(grants) {
  dplyr::mutate(
    grants,
    Investigators = purrr::map(investigators, \(.i) {
      format_investigators_text(.i) |>
        paste(collapse=";")
    })
  )
}

#  text_table<-collapse_investigators(grants) %>%
#    dplyr::select(investigators, everything())
#
##  text_table
#}
