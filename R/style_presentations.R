#' A styling function for presentations
#'
#' @details
#' This function does no filtering to the table, but does the work of styling the
#' table as a flextable. This can involve some heavy work, however, given the various
#' ways in which styling requires post-processing.
#'
#' The flextable (and text) styles are listed by alpha, delta, to distinguish them
#' as we are determining the best formatting approach.
#'
#' @param d A data frame of presentations (typed as presentations)
#'
#' @return A flextable consisting of formatted publication entries.
#' @export
#'
#' @seealso
#'  * [style] for general styling information.
#'  * [style_presentations] for general presentation style information.
#'  * [style_publictions] for publication styling.
#'  * [style_grants] for grant styling.
#'
style_presentations_as_flextable_delta<-function(d, ...) {
  tbl <- d |>
    # Add in a presentation number (specific to this data).
    dplyr::mutate(
      `Abstract Number`=abstract_id,
      `ESI Related` = dplyr::case_when(
        is_current_esi_related ~ "Current ESI",
        is_former_esi_related ~ "Former ESI",
        .default = ""
      ),
      `Presentation Year` = presentation_year,
      `U54 Support` = support
    )
  # Create a flextable. Define the specific columns to use in the table here, even
  # if the columns do not exist in the source data (we can add formatting to a new
  # column during formatting).
  ft <- flextable::flextable(
    tbl,
    col_keys=c(
      "Abstract Number",
      "Presentation Year",
      "Formatted Reference",
      "U54 Support",
      "ESI Related")
  )

  # The reference is actually a composite of many different columns, composed into
  # a single flextable cell (as a formatted paragraph). This is rather complex, but
  # the flextable::compose is used to store the results into the empty column we
  # created "Formatted Reference" for printing.
  ft <- flextable::compose(
    ft,
    j = "Formatted Reference",
    # This should eventually be a function call in tidy select setup.
    value = format_presentations_as_flextable(ft$body$dataset)
  )

  # The cell text should now all be present in the table. Next step is
  # to apply formatting (alignment, padding, etc) to it.
  ft <- ft |>
    # Define column widths. This should agree with the number of columns in the table.
    flextable::width(width = c(0.25, 0.25, 5,1.5,1.5)) |>
    # Center content
    flextable::align(j = c("Presentation Year","Abstract Number"), align="center") |>
    # Remove extra space in small columns
    flextable::padding(
      j = c("Presentation Year", "Abstract Number"),
      padding.left=0,padding.right=0
    ) |>
    # Justify the publication
    flextable::align(j="Formatted Reference", align = "justify") |>

    # Add footnotes for annotation.
    flextable::add_footer_lines(values = c("L1","L2","L3")) |>
    flextable::compose(
      i=1,j=1, part="footer",
      value = flextable::as_paragraph(
        flextable::as_b("Partnership Member")
      )
    ) |>
    flextable::compose(
      i=2,j=1, part= "footer",
      value = flextable::as_paragraph(
        flextable::as_b("REC Trainee") |> dplyr::mutate(underlined=TRUE))
    ) |>
    flextable::compose(
      i=3, j=1, part="footer",
      value = flextable::as_paragraph(
        flextable::as_b("Role: \u2020 (Current ESI), * (Former ESI)")
      )
    ) |>
    # Last thing should be apply the overall styling
    apply_u54reportr_flextable_style()


  ft
}


#' Format presentations/authors as text-printable output
#'
#' @details
#' The presentations consist of many fields that
#' can be compressed and rearranged for nice output. This function
#' attempts to format the presentations/authors in a way that is
#' suitable for text-based output (e.g., a spreadsheet).
#'
#' @param .x The presentations table
#' @param ... Other parameters to pass to format_presenters.
#' @return A formatted table with a subset of columns for printing.
#' @export
#'
style_presentations_as_text_alpha<-function(.x, ...) {
  if ( nrow(.x) == 0) return(.x)

  text_table <- .x |>
    dplyr::mutate(`Authors` = format_presenters(
      presenters,
      format_presenters_function=format_presenters_as_text,
      ...
    )) |>
    dplyr::select(`Authors`, tidyselect::everything())

  text_table
}
