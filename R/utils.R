
#' Format date value as month-year
#'
#' @description Short-hand function for formatting per month year standard.
#'
#' @param d A date to format
#'
#' @return A character vector representing the month year format of `d`.
#' @export
#'
#' @examples
#' \dontrun{
#' format_date_as_month_year(as.Date("2024-01-01"))
#' [1] "Jan 2024"
#' }
format_date_as_month_year<-function(d) {
  format(d,format="%b %Y")
}

#' Format date value as mdy
#'
#' @description Short-hand function for formatting per mdy standard (m/d/y).
#'
#' @param d A date to format
#'
#' @return A character vector representing the mdy format of `d`.
#' @export
#'
#' @examples
#' \dontrun{
#' format_date_as_mdy(as.Date("2024-01-01"))
#' [1] "01/01/2024"
#' }
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
#' \dontrun{
#' split_date_range("2024_01_01 - 2024_01_02")
#'      [,1]          [,2]
#' [1,] "2024_01_01 " " 2024_01_02"
#' }
split_date_range<-function(s) {
  stringr::str_split_fixed(s, "-", 2)
}





#' Paste two vectors together using sep, suppressing NA (rather than contaminating).
#'
#' @details
#' Paste and str_c both appear to have a problem combining two strings when one is NA.
#' Or alternately, when one string is NA the result should be only the first name with
#' no sep or second component. This function does that in the case of NA.
#'
#' @param v1 String (or vector of strings)
#' @param v2 String (or vector of strings)
#' @param sep Separator (e.g., ',')
#'
#' @return A string (or vector of strings) with v1 and v2 pasted together, avoiding NA's.
#' @export
#'
#' @examples
#' \dontrun{
#' paste_noNA("A",NA)
#' [1] "A"
#' }
#' \dontrun{
#' paste_noNA("A","B")
#' [1] "A, B"
#' }
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
datestamp<-lubridate::today


#' Enclose a string in brackets
#'
#' @description `str_enclose()` encloses a string in parentheses (default) or
#' other open/close characters. It supports adding a leading space and does not
#' enclose NA.
#'
#' @param s A string to enclose
#' @param open The open character (default is paren)
#' @param close The close character (default is paren)
#' @param add_space (TRUE) Should a leading space be added?
#'
#' @return `s` enclosed by `open` and `close`.
#' @export
#'
#' @examples
#' \dontrun{
#'   str_enclose("testing",open="[",close="]")
#' }
str_enclose <- function(s, open="(",close=")", add_space = TRUE) {
  # Note: This needs to be something that keeps the NA, which paste does not do.
  s <- stringr::str_c(ifelse(add_space," ",""), open, s, close)
  s <- tidyr::replace_na(s,"")

  s
}


#' Number of true values in logical array
#'
#' @description Return the count of the number of TRUE values from a vector
#'  as a convenience function.
#'
#' @details Often we use a logical test across a vector to find out
#' how many elements meet a specific criteria. For instance,
#' ```
#' length(which(x>4))
#' ```
#' This function is a convenience function to slightly
#' simplify that syntax since it comes up so much. Using the example
#' above,
#' ````
#' how_many(x>4)
#'
#' @param .x A logical vector to count up true values from
#' @param negate (FALSE) Should FALSE values be counted instead of TRUE
#' @return The number of TRUE (or FALSE if `negate`) values in the vector `.x`
#' @export
#'
#' @examples
#' \dontrun{
#' how_many(iris$Sepal.Length>5)
#' }
how_many <- function(.x, negate = FALSE) {
  if ( negate ) .x <- !.x
  length(which(.x))
}


