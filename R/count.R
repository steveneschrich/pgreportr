# count.R
# Count the number of different things.
#


#' Count grant submissions by year
#'
#' Summarize grant submissions (total) by U54 fiscal year.
#'
#' @param d Grants table
#'
#' @return A named vector of `year`=`count`.
#' @export
#'
count_submissions_by_year<-function(d) {
  d %>%
    dplyr::group_by(`U54 Fiscal Year Submitted`) %>%
    dplyr::summarize(n=dplyr::n(), .groups="drop_last") %>%
    dplyr::pull("n", name="U54 Fiscal Year Submitted")
}

#' Count number of grant submissions
#'
#' @param d Grants table
#'
#' @return The count of number of grant submissions
#' @export
#'
#' @examples
count_submissions<-function(d) {
  d %>%
    dplyr::summarize(n=dplyr::n()) %>%
    dplyr::pull("n")
}

count_funded<-function(d) {
  d %>%
    dplyr::group_by(`U54 Fiscal Year Funded`) %>%
    dplyr::summarize(n=n(), .group="drop_last") %>%
    pull("n", name="U54 Fiscal Year Funded")
}




#' Count funded submissions
#' This has some subtlety. We need to actually consider the year it is funded, as opposed
#' to the year it was submitted.
#'
#' @param d
#' @param yr
#'
#' @return
#' @export
#'
count_grants_funded_by_year<-function(d) {
  d %>%
    filter_grants_funded() %>%  # Only funded grants
    dplyr::group_by(`U54 Fiscal Year Funded`) %>% # Grouped by funded fiscal year
    dplyr::summarize(n=dplyr::n(), .groups="keep") %>% # Add count of each year
    dplyr::pull("n", name="U54 Fiscal Year Funded") # Extract named list of counts
}


#' Counts grants funded
#'
#' @param d Grants table
#'
#' @return Total grants funded
#' @export
#'
count_grants_funded<-function(d) {
  d %>%
    filter_grants_funded() %>%
    dplyr::summarize(n=dplyr::n()) %>%
    dplyr::pull("n")
}
#' Add a TOTAL to a list of counts.
#'
#' @param l The existing list.
#'
#' @return A new list with a TOTAL at the end.
#' @export
#'
#' @examples
add_list_total<-function(l) {
  s<-sum(l)
  c(l, "TOTAL"=s)
}
