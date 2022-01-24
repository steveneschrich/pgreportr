#' Title
#'
#' @details This code was taken from
#'  https://bookdown.org/yihui/rmarkdown-cookbook/custom-knit.html as a way
#'  to change the output file associated with markdown from within R Studio.
#'
#' @param input Input file
#' @param output_dir Output directory (default: delivery)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
knit_docx_with_datestamp <- function(input, output_dir = "delivery", ...) {
  rmarkdown::render(
    input,
    output_dir = here::here(output_dir),
    output_file = paste0(
      xfun::sans_ext(input), '_',
      lubridate::today(),
      ".docx"
    ),
    envir = globalenv()
  )
}
