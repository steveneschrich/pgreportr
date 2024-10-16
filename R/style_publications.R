#' Format publications/authors as text-printable output
#'
#' @details
#' The publications consist of many fields that
#' can be compressed and rearranged for nice output. This function
#' attempts to format the publications/authors in a way that is
#' suitable for text-based output (e.g., a spreadsheet).
#'
#' @param .x The publications table
#' @param ... Other parameters to pass to format_investigators.
#' @return A formatted table with a subset of columns for printing.
#' @export
#'
style_publications_as_text_alpha<-function(.x, ...) {
  if ( nrow(.x) == 0) return(.x)

  text_table <- .x |>
    dplyr::mutate(`Authors` = format_authors(authors,
                                             format_authors_function=format_authors_as_text,
                                             ...
    )) |>
    dplyr::select(`Authors`, tidyselect::everything())

  text_table
}
style_pubs_as_text_alpha <- style_publications_as_text_alpha


#' A styling function for publications
#'
#' @details
#' This function does no filtering to the table, but does the work of styling the
#' table as a flextable. This can involve some heavy work, however, given the various
#' ways in which styling requires post-processing.
#'
#' The flextable (and text) styles are listed by alpha, delta, to distinguish them
#' as we are determining the best formatting approach.
#'
#' @param d A data frame of publications
#'
#' @return A flextable consisting of formatted publication entries.
#' @export
#'
#' @examples
style_publications_as_flextable_delta<-function(d, ...) {
  tbl <- d |>
    # Add in a publication number (specific to this data).
    dplyr::mutate(
      `Citation Number`=dplyr::row_number(),
      `ESI Related` = dplyr::case_when(
        is_current_esi_related ~ "Current ESI",
        is_former_esi_related ~ "Former ESI",
        .default = ""
      ),
      `Publication Year` = publication_year,
      `U54 Support` = support
    )
  # Create a flextable. Define the specific columns to use in the table here, even
  # if the columns do not exist in the source data (we can add formatting to a new
  # column during formatting).
  ft <- flextable::flextable(
    tbl,
    col_keys=c(
      "Citation Number",
      "Publication Year",
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
    value = format_publications_as_flextable(ft$body$dataset)
  )

  # The cell text should now all be present in the table. Next step is
  # to apply formatting (alignment, padding, etc) to it.
  ft <- ft |>
    # Define column widths. This should agree with the number of columns in the table.
    flextable::width(width = c(0.25, 0.25, 5,1.5,1.5)) |>
    # Center content
    flextable::align(j = c("Publication Year","Citation Number"), align="center") |>
    # Remove extra space in small columns
    flextable::padding(
      j = c("Publication Year", "Citation Number"),
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

style_pubs_as_flextable_delta <- style_publications_as_flextable_delta


#' Title
#'
#' @details
#' This function does no filtering to the table, but does the work of styling the
#' table as a flextable. This can involve some heavy work, however, given the various
#' ways in which styling requires post-processing.
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
style_pubs_as_flextable_gamma<-function(d, ...) {
  d %>%
    dplyr::mutate(`Authors` = format_authors(authors,
                                             format_authors_function=format_authors_as_text
    )) %>%
    dplyr::mutate(Citation = format_citation_as_text(.)) %>%
    dplyr::select(`Citation`, `Publication Date`, `U54 Core Support`) %>%

    # Add to flextable, this is likely to change.
    flextable::flextable(col_keys=c("Citation", "Publication Date", "U54 Core Support"),

                         cwidth=c(5, 1.5, 1.5)) %>%
    # Add an extra header row for formatting
    #flextable::add_header_row(top=TRUE,values=c(
    #  "Date Submitted", "Grant Title", "Grant Agency", "Status",
    #  "Funded","Investigators"),
    #  colwidths=c(1,1,1,1,1,2,1)) %>%
    #flextable::merge_h(1, part="header") %>%
    #flextable::merge_v(j=1:5, part="header") %>%
    #flextable::align(j = ~`Funded`, align="center", part="header") %>%

    # Add ESI label
    flextable::footnote(i = 1, j = 1, part = "header",
                        value = flextable::as_paragraph(
                          "Early Stage Investigator (ESI)"
                        ),
                        ref_symbols = c("[ESI]")
    ) %>%
    # Add Grant Status footnote.
    # flextable::footnote(i = 1, j = 6, part = "header",
    #                     value = flextable::as_paragraph(
    #                      "Grant Status: F (Funded), NF (Not Funded), PR (Pending Review), IP (In Preparation)"
    #                     ),
    #                     ref_symbols = c("2")
    # ) %>%

    # Last thing should be apply the overall styling
    apply_u54reportr_flextable_style()


}

