#' Count number of research products based on different criteria
#'
#' @description Counting totals can be a bit difficult in complex
#'  data because the logic of what should be counted may not always
#'  be obvious. The `count_` functions in this package are designed
#'  to encode this logic and memorialize it for future reports.
#'  * [filter_between()]
#'  * [filter_in_year()]
#'  * [filter_supported()]
#'  * [filter_core_supported()]
#'  * [filter_by_tag()]
#'
#' @note Ideally the `count` routines will only count existing records and
#'  not try to do additional filtering, but this may be necessary (or desirable). If
#'  filtering, then it should use the [filter()] section of this package.
#' @name count
NULL

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
    dplyr::count(d, pg_year_submitted) |>
      dplyr::pull("n", name="pg_year_submitted")
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
  d |>
    dplyr::summarize(n=dplyr::n()) |>
    dplyr::pull("n")
}

count_funded<-function(d) {
  dplyr::count(d, pg_year_funded) |>
    pull("n", name="pg_year_funded")
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

    filter_grants_funded(d) |>  # Only funded grants
    dplyr::count(pg_year_funded) |> # Grouped by funded fiscal year
    dplyr::pull("n", name="pg_year_funded") # Extract named list of counts
}


#' Counts grants funded
#'
#' @param d Grants table
#'
#' @return Total grants funded
#' @export
#'
count_grants_funded<-function(d) {

    filter_grants_funded(d) |>
    dplyr::summarize(n=dplyr::n()) |>
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


#' Title
#'
#' @details This is a function to expand out the investigators to find ESI's,
#' then provide a total count of these. Note this will mean multiple
#' entries per grant.
#'
#' @param g
#'
#' @return
#' @export
#'
#' @examples
count_current_esi_mentions_in_grant<-function(g) {
  purrr::map_int(g$investigators, \(.x) {
    length(which(.x[["isPartnershipRole_Current ESI"]]))
  })

}

count_current_esi_mentions_in_pubs <- function(.x) {
  warn("This may not work correctly")
  purrr::map_int(.x$investigators, \(.y) {
    length(which(.y[["isPartnershipRole_Current ESI"]]))
  })

}

count_current_esi_mentions <- function(.x, var) {
  purrr::map_int(.x, \(.y) {
    how_many(.y[[var]])
  })
}
count_presentations_number_current_esi_mentions <- function(.x) {
  count_current_esi_mentions(
    .x[["presenters"]],
    var = "isPartnershipRole_Current ESI"
  )
}

 # purrr::map_int(.x$presenters, \(.y) {
#    how_many(.y[["isPartnershipRole_Current ESI"]])
#  })
#}
#' Title
#'
#' @param .x
#' @param var
#'
#' @return
#' @export
#'
#' @examples
count_current_esi_mentions <- function(.x, var) {
  sum(count_current_esi_creators(.x[[var]]))
}


#' Count the ESI's in a creator table
#'
#' @description In either a grant investigators or publication authors table, count
#' the number of ESIs.
#'
#' @details A grant has a table of investigators and a publication has a table
#' of authors. In either case, each creator can have a Partnership Role of
#' ESI. This function will count the number of such ESI's for the grant/publication.
#'
#' @note Empty tables can be a little tricky, so there is special code to return
#' a count of 0 if the table is empty.
#'
#' @param .x An investigator or author table
#'
#' @return The count of the number of ESI's
#' @export
#'
#' @examples
count_current_esi_creators <- function(.x) {
  purrr::map_int(.x, ~dplyr::filter(.x, is_creator_current_esi(partnership_role)) %>% nrow())
}

