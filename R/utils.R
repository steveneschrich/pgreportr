
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

  v1 <- replace(v1, is.na(v1), "")
  v2 <- replace(v2, is.na(v2),  "")

  x <- dplyr::case_when(
    v1 == "" ~ v2,
    v2 == "" ~ v1,
    TRUE ~ paste(v1, v2, sep=sep)
  )

  x
}


#' Return today's date as YYMMDD for printing.
#'
#' @return A string representing today's date.
#' @export
#'
#'
datestamp<-lubridate::today


#' Title
#'
#' @param s
#' @param open
#' @param close
#' @param add_space
#'
#' @return
#' @export
#'
#' @examples
str_enclose <- function(s, open="(",close=")", add_space = TRUE) {
  # Note: This needs to be something that keeps the NA, which paste does not do.
  s <- stringr::str_c(ifelse(add_space," ",""), open, s, close)
  s <- tidyr::replace_na(s,"")

  s
}



