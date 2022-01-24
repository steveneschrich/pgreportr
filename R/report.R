#' Filter and format a grant table
#'
#' Given a tibble associated with selected grants, format the tibble as a
#' flextable object to insert into an Office document. This particular function
#' manages the "joint grant" style.
#'
#' @details
#' The model for this package is to provide a fairly comprehensive tibble with
#' grant information. There are filters to subset to a given list of grants. Once
#' this is done, we want to generate pretty output representing the table. This
#' function will provide a nicely-formatted flextable suitable for reporting
#' requirements.
#'
#' Of note, this function will apply filters for the specific condition (e.g., joint
#' grants) as implemented by the `filter_*` approach in the library.
#'
#' @param d A grant table
#'
#' @return
#' @export
#'
#' @examples
report_joint_grants_as_flextable<-function(grants, ...) {
  d %>%
    filter_grants_joint() %>%
    dplyr::arrange(`Submission Date`) %>%
    style_grants_as_flextable(...)

}

report_joint_grants<-function(grants, style=c("flextable","text"), ...) {
  style<-match.arg(style)

  switch(style,
         flextable = report_joint_grants_as_flextable(grants, ...),
         text = report_joint_grants_as_text(grants, ...)
  )
}
