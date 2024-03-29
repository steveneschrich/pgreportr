#' Example grants table.
#'
#' A dataset containing grant information from the PHSU-MCC Partnership. The
#' specific dataset covers multiple years and involves investigators at
#' both institutions for a reasonable set of test cases.
#'
#' @format A tibble with 119 rows and 52 variables:
#' \describe{
#'   \item{grant_id}{Unique identifier for grant (generated by redcap)}
#'   \item{investigators}{An embedded tibble of investigators}
#'   ...
#' }
grants <- function(...) {
  ido::Data("pgreportr::grants", ...)
}
