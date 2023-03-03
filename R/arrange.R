#' Arrange investigator table by role
#'
#' Grant roles have a certain hierarchy (PI, Co-I, postdoc, etc). This function
#' will sort the investigators by this predefined hierarchy.
#'
#' The package variable `role_order` defines the order of roles to be considered.
#'
#' @param investigators An investigators tibble
#'
#' @return A sorted investigators table, ordered by investigator role.
#' @export
#'
arrange_investigators_by_role<-function(investigators) {
  assertthat::assert_that(is.data.frame(investigators),
                          assertthat::has_name(investigators, "Role"),
                          exists("role_order"))

  dplyr::arrange(investigators, match(.data$Role, role_order))

}
