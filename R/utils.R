
#' Title
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
format_date_as_month_year<-function(d) {
  format(d,format="%b %Y")
}

#' Title
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
format_date_as_mdy<-function(d) {
  format(d,format="%m/%d/%Y")
}


#' Split dates that are in the form of start_date - end_date.
#'
#' @param s A character vector representing the combined date.
#'
#' @return Two character vectors representing the first and second date, respectively.
#' @export
#'
#' @examples
split_date_range<-function(s) {
  stringr::str_split_fixed(s, "-", 2)
}





#' Paste two vectors together using sep, suppressing NA (rather than contaminating).
#'
#' @details
#' Paste and str_c both appear to have a problem combining two strings when one is NA.
#' Or alternately, when one string is NA the result should be only the first name with
#' no sep or second component. This function does that correctly in the case of NA.
#'
#' @param v1 String (or vector of strings)
#' @param v2 String (or vector of strings)
#' @param sep Separator (e.g., ',')
#'
#' @return A string (or vector of strings) with v1 and v2 pasted together, avoiding NA's.
#' @export
#'
paste_noNA<-function(v1, v2, sep=", ") {
  dplyr::bind_cols(v1=v1, v2=v2) %>%
    dplyr::mutate(
      v1 = dplyr::na_if(v1, ""),
      v2 = dplyr::na_if(v2, "")) %>%
    tidyr::unite(newname, v1, v2, na.rm=TRUE, sep=sep) %>%
    dplyr::pull(newname)

}


#' Return today's date as YYMMDD for printing.
#'
#' @return A string representing today's date.
#' @export
#'
#'
datestamp<-function() {
  format(Sys.time(),'%Y%m%d')
}




