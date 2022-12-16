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
style_grants_as_flextable_alpha<-function(d, ...) {
  d %>%



    # Format these fields up front
    dplyr::mutate(
      `Submission Date` = format_date_as_month_year(.data$`Submission Date`),
      `Dates` = stringr::str_c(format_date_as_mdy(.data$`Funding Start Date`),
                               format_date_as_mdy(.data$`Funding End Date`),
                               sep="-")
    ) %>%

    dplyr::rename(
      `Total Direct Costs` = .data$`total_direct_costs`,
      `Date Submitted` = .data$`Submission Date`,
      `Grant Title` = .data$`Title`,
      `Status` = .data$`Grant Status`,

    ) %>%

    # Select to specific fields to display
    dplyr::select(.data$`investigators`,
                  .data$`Date Submitted`,
                  .data$`Grant Title`,
                  .data$`Grant Summary`,
                  .data$`Status`,
                  .data$`Dates`, .data$`Total Direct Costs`) %>%

    # Add to flextable, this is likely to change.
    flextable::flextable(col_keys=c("Investigator Names","Date Submitted",
                                    "Grant Title", "Grant Summary",
                                    "Status","Dates","Total Direct Costs",
                                    "investigators"
    ),#) %>%#,
    cwidth=c(1.12, 0.81, 1.57, 0.94, 0.63, 1.75, 1.75,0.1)) %>%
    # Manually set widths (using template).
    #flextable::width(j=1:7,
    #                 width=c(1.12, 0.81, 1.57, 0.94, 0.63, 1.75, 1.75)) %>%
    # Collapse investigators (tibble) to single flextable cells.
    flextable::compose(j = ~`Investigator Names`,
                       value = format_investigators_by_institution(investigators,
                                                                   group="Institution",
                                                                   format_investigator_group=format_investigator_group_as_flextable,
                                                                   combine_investigator_groups=combine_investigator_groups_as_flextable,
                                                                   ...
                       )
    ) %>%
    # Remove the investigators field, we only need it for the formatting.
    flextable::void(j = ~ investigators, part="all") %>%
    # Then specific formatting for this report
    # Set alignments, style for columns
    flextable::set_header_labels(`Investigator Names`="Investigators") %>%
    flextable::align(j= ~`Total Direct Costs`, align = "right",part="body") %>%
    flextable::italic(j= ~`Grant Title`, part="body") %>%
    flextable::align(j=~`Date Submitted`, align="center", part="body") %>%

    # Add an extra header row for formatting
    #flextable::add_header_row(top=TRUE,values=c(
    #  "Date Submitted", "Grant Title", "Grant Agency", "Status",
    #  "Funded","Investigators"),
    #  colwidths=c(1,1,1,1,1,2,1)) %>%
    #flextable::merge_h(1, part="header") %>%
    #flextable::merge_v(j=1:5, part="header") %>%
    #flextable::align(j = ~`Funded`, align="center", part="header") %>%

    # Last thing should be apply the overall styling
  apply_u54reportr_flextable_style()


}




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
style_grants_as_flextable_beta<-function(d, ...) {
  d %>%

    dplyr::select(`Grant Type`, `Source`, `investigators`, `Title`, `U54 Core Support`) %>%

    # Add to flextable, this is likely to change.
    flextable::flextable(col_keys=c("Grant Type","Source","Investigators","Title","U54 Core Support",
                                    "investigators"),

    cwidth=c(1, 1, 2,2.5, 1, 10)) %>%
    # Manually set widths (using template).
    #flextable::width(j=1:7,
    #                 width=c(1.12, 0.81, 1.57, 0.94, 0.63, 1.75, 1.75)) %>%
    # Collapse investigators (tibble) to single flextable cells.
    flextable::compose(j = ~`Investigators`,
                       value = format_investigators_by_institution(investigators,
                                                                   group="Institution",
                                                                   format_investigator_group=format_investigator_group_as_flextable,
                                                                   combine_investigator_groups=combine_investigator_groups_as_flextable,
                                                                   ...
                       )
    ) %>%
    # Remove the investigators field, we only need it for the formatting.
    flextable::void(j = ~ investigators, part="all") %>%
    # Then specific formatting for this report
    # Set alignments, style for columns
    flextable::set_header_labels(`Investigators`="Investigators") %>%
    flextable::italic(j= ~`Title`, part="body") %>%
    flextable::bold(j= ~`Grant Type`, part="body") %>%

    # Add an extra header row for formatting
    #flextable::add_header_row(top=TRUE,values=c(
    #  "Date Submitted", "Grant Title", "Grant Agency", "Status",
    #  "Funded","Investigators"),
    #  colwidths=c(1,1,1,1,1,2,1)) %>%
    #flextable::merge_h(1, part="header") %>%
    #flextable::merge_v(j=1:5, part="header") %>%
    #flextable::align(j = ~`Funded`, align="center", part="header") %>%

    # Last thing should be apply the overall styling
  apply_u54reportr_flextable_style()


}




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
style_grants_as_flextable_gamma<-function(d, ...) {
  d %>%
    dplyr::mutate(`planned_date_of_submission`=lubridate::ymd(`planned_date_of_submission`)) %>%
    dplyr::mutate(`Submission Date` =
                   dplyr::coalesce(`Submission Date`, `planned_date_of_submission`) ) %>%
    dplyr::select(`Grant Type`, `Source`, `Submission Date`,  `investigators`, `Title`, `Grant Status`,`U54 Core Support`) %>%
    dplyr::mutate(`Grant Status` =
                    dplyr::case_when(
                      `Grant Status` == "Funded" ~"F",
                      `Grant Status` == "In Preparation" ~ "IP",
                      `Grant Status` == "Not Funded" ~ "NF",
                      `Grant Status` == "Pending Review" ~ "PR"
                    )) %>%

    # Add to flextable, this is likely to change.
    flextable::flextable(col_keys=c("Grant Type","Source","Submission Date","Investigators","Title","Grant Status",
                                    "U54 Core Support",
                                    "investigators"),

                         cwidth=c(0.2, 0.5, 0.5,1.75,2.5, 0.5, 0.5,1)) %>%
    # Manually set widths (using template).
    #flextable::width(j=1:7,
    #                 width=c(1.12, 0.81, 1.57, 0.94, 0.63, 1.75, 1.75)) %>%
    # Collapse investigators (tibble) to single flextable cells.
    flextable::compose(j = ~`Investigators`,
                       value = format_investigators_by_institution(investigators,
                                                                   group="Institution",
                                                                   format_investigator_group=format_investigator_group_as_flextable,
                                                                   combine_investigator_groups=combine_investigator_groups_as_flextable,
                                                                   ...
                       )
    ) %>%
    # Remove the investigators field, we only need it for the formatting.
    flextable::void(j = ~ investigators, part="all") %>%
    # Then specific formatting for this report
    # Set alignments, style for columns
    flextable::set_header_labels(`Investigators`="Investigators") %>%
    flextable::italic(j= ~`Title`, part="body") %>%
    flextable::bold(j= ~`Grant Type`, part="body") %>%

    # Add an extra header row for formatting
    #flextable::add_header_row(top=TRUE,values=c(
    #  "Date Submitted", "Grant Title", "Grant Agency", "Status",
    #  "Funded","Investigators"),
    #  colwidths=c(1,1,1,1,1,2,1)) %>%
    #flextable::merge_h(1, part="header") %>%
    #flextable::merge_v(j=1:5, part="header") %>%
    #flextable::align(j = ~`Funded`, align="center", part="header") %>%

    # Add ESI label
    flextable::footnote(i = 1, j = 4, part = "header",
      value = flextable::as_paragraph(
        "Early Stage Investigator (ESI)"
      ),
      ref_symbols = c("1")
    ) %>%
    # Add Grant Status footnote.
    flextable::footnote(i = 1, j = 6, part = "header",
      value = flextable::as_paragraph(
        "Grant Status: F (Funded), NF (Not Funded), PR (Pending Review), IP (In Preparation)"
      ),
      ref_symbols = c("2")
    ) %>%

    # Last thing should be apply the overall styling
  apply_u54reportr_flextable_style()


}


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
style_grants_as_flextable_epsilon<-function(d, ...) {
  d |>
    dplyr::mutate(
      `Grant Number` = dplyr::row_number(),
      `planned_date_of_submission`=lubridate::ymd(`planned_date_of_submission`),
      `Submission Date` = dplyr::coalesce(`Submission Date`, `planned_date_of_submission`),
      ESI = ifelse(is_esi_related, "ESI-related",""),
      `Grant Status` =
        dplyr::case_when(
          `Grant Status` == "Funded" ~"F",
          `Grant Status` == "In Preparation" ~ "IP",
          `Grant Status` == "Not Funded" ~ "NF",
          `Grant Status` == "Pending Review" ~ "PR"
        )
    ) |>
    dplyr::select(
      `Grant Number`,
      `U54 Year`,
      Source,
      investigators,
      Title,
      `Grant Status`,
      ESI
    ) |>

    # Add to flextable, this is likely to change.
    flextable::flextable(
      col_keys=c(
        "Grant Number","U54 Year","Source","Investigators","Title","Grant Status",
        "ESI", "investigators"
      ),

                         cwidth=c(0.3, 0.4, 1.0,1.75,2.5, 0.5, 0.75)) |>
    # Manually set widths (using template).
    #flextable::width(j=1:7,
    #                 width=c(1.12, 0.81, 1.57, 0.94, 0.63, 1.75, 1.75)) %>%
    # Collapse investigators (tibble) to single flextable cells.
    flextable::compose(j = ~`Investigators`,
                       value = format_investigators_by_institution(investigators,
                                                                   group="Institution",
                                                                   format_investigator_group=format_investigator_group_as_flextable,
                                                                   combine_investigator_groups=combine_investigator_groups_as_flextable,
                                                                   ...
                       )
    ) |>
    # Remove the investigators field, we only need it for the formatting.
    flextable::void(j = ~ investigators, part="all") |>
    # Then specific formatting for this report
    # Set alignments, style for columns
    flextable::set_header_labels(`Investigators`="Investigators") |>
    flextable::italic(j= ~`Title`, part="body") |>

    # Add an extra header row for formatting
    #flextable::add_header_row(top=TRUE,values=c(
    #  "Date Submitted", "Grant Title", "Grant Agency", "Status",
    #  "Funded","Investigators"),
    #  colwidths=c(1,1,1,1,1,2,1)) %>%
    #flextable::merge_h(1, part="header") %>%
    #flextable::merge_v(j=1:5, part="header") %>%
    #flextable::align(j = ~`Funded`, align="center", part="header") %>%
    # Add ESI label
    flextable::footnote(i = 1, j = 4, part = "header",
                        value = flextable::as_paragraph(
                          "Early Stage Investigator (ESI)"
                        ),
                        ref_symbols = c("1")
    ) |>

    flextable::footnote(i = 1, j = 2, part = "header",
                        value = flextable::as_paragraph(
                          "U54 Year\n",
                          dplyr::select(d, "U54 Year") |>
                            dplyr::filter(!stringr::str_detect(`U54 Year`, "\\(")) |>
                            dplyr::distinct() |>
                            dplyr::left_join(pg_grant_years, by=c("U54 Year" = "year")) |>
                            dplyr::mutate(
                              yr_annotation = sprintf("%s: %s - %s",`U54 Year`, start_date, end_date)
                            ) |>
                            dplyr::pull(yr_annotation) |>
                            stringr::str_c(collapse="\n")
                        ),
                        ref_symbols = c("2")
    ) |>
    # Add Grant Status footnote.
    flextable::footnote(i = 1, j = 6, part = "header",
                        value = flextable::as_paragraph(
                          "Grant Status: F (Funded), NF (Not Funded), PR (Pending Review), IP (In Preparation)"
                        ),
                        ref_symbols = c("3")
    ) |>



    # Last thing should be apply the overall styling
    apply_u54reportr_flextable_style()


}


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
style_grants_as_flextable_delta<-function(d, ...) {
  d %>%

    dplyr::select(`Grant Type`, `Source`, `investigators`, `Title`, `Grant Status`,`U54 Core Support`,`Submission Date`) %>%
    dplyr::mutate(`Submission Date` = format_date_as_month_year(.data$`Submission Date`)) %>%
    # Add to flextable, this is likely to change.
    flextable::flextable(col_keys=c("Grant Type","Source","Investigators","Title","Grant Status",
                                    "U54 Core Support","Submission Date",
                                    "investigators"),

                         cwidth=c(1, 1, 2,2.5, 1, 1,1,1)) %>%
    # Manually set widths (using template).
    #flextable::width(j=1:7,
    #                 width=c(1.12, 0.81, 1.57, 0.94, 0.63, 1.75, 1.75)) %>%
    # Collapse investigators (tibble) to single flextable cells.
    flextable::compose(j = ~`Investigators`,
                       value = format_investigators_by_institution(investigators,
                                                                   group="Institution",
                                                                   format_investigator_group=format_investigator_group_as_flextable,
                                                                   combine_investigator_groups=combine_investigator_groups_as_flextable,
                                                                   ...
                       )
    ) %>%
    # Remove the investigators field, we only need it for the formatting.
    flextable::void(j = ~ investigators, part="all") %>%
    # Then specific formatting for this report
    # Set alignments, style for columns
    flextable::set_header_labels(`Investigators`="Investigators") %>%
    flextable::italic(j= ~`Title`, part="body") %>%
    flextable::bold(j= ~`Grant Type`, part="body") %>%

    # Add an extra header row for formatting
    #flextable::add_header_row(top=TRUE,values=c(
    #  "Date Submitted", "Grant Title", "Grant Agency", "Status",
    #  "Funded","Investigators"),
    #  colwidths=c(1,1,1,1,1,2,1)) %>%
    #flextable::merge_h(1, part="header") %>%
    #flextable::merge_v(j=1:5, part="header") %>%
    #flextable::align(j = ~`Funded`, align="center", part="header") %>%

    # Last thing should be apply the overall styling
  apply_u54reportr_flextable_style()


}


#' Format grants/investigators to a text-printable output.
#'
#' @details
#' The grants consist of many fields that
#' can be compressed and rearranged for nice output. This function
#' attempts to format the grant/investigators in a way that is
#' suitable for text-based output (e.g., a spreadsheet).
#'
#' @param grants The grants table.
#' @param ... Other parameters to pass to format_investigators.
#' @return A formatted table with a subset of columns for printing.
#' @export
#'
style_grants_as_text_alpha<-function(grants, ...) {
  if ( nrow(grants) == 0) return(grants)

  text_table <- grants %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`Investigators` = format_investigators_by_institution(list(investigators),
                                                                    group="Institution",
                                                                    format_investigator_group=format_investigator_group_as_text,
                                                                    combine_investigator_groups =combine_investigator_groups_as_text,
                                                                    ...
    )) %>%
    dplyr::select(`Investigators`, tidyselect::everything())

  text_table
}

#' Title
#'
#' @param grants
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
style_grants_as_text_beta<-function(grants, ...) {
  text_table <- grants %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`Investigators` = format_investigators(list(investigators),
                                                                 format_investigator_group=format_investigator_group_as_text,
                                                                 combine_investigator_groups=combine_investigator_groups_as_text,
                                                                 label="",
                                                                 ...
    )) %>%
    dplyr::select(Investigators, tidyselect::everything())

  text_table
}

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
style_pubs_as_text_alpha<-function(.x, ...) {
  if ( nrow(.x) == 0) return(.x)

  text_table <- .x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`Authors` = format_authors(list(authors),
                                             format_authors_function=format_authors_as_text,
                                             ...
    )) %>%
    dplyr::select(`Authors`, tidyselect::everything())

  text_table
}

#' Apply default style
#'
#' This is a temporary hack until I figure out flextable themes.
#'
#' @param table
#'
#' @return
#' @export
#'
apply_u54reportr_flextable_style<-function(table) {
  table %>%
    flextable::bg(bg="#4F81BD", part="header") %>%
    flextable::color(color="#FFFFFF", part="header") %>%
    flextable::set_table_properties(width=1.0, layout="autofit") %>%
    flextable::fontsize(size=8, part = "all")
}





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
style_pubs_as_flextable_delta<-function(d, ...) {
  tbl <- d |>
    # Add in a publication number (specific to this data).
    dplyr::mutate(`Citation Number`=dplyr::row_number())

  # Create a flextable. Define the specific columns to use in the table here, even
  # if the columns do not exist in the source data (we can add formatting to a new
  # column during formatting).
  ft <- flextable::flextable(
    tbl,
    col_keys=c(
      "Publication Year",
      "Citation Number",
      "Formatted Reference",
      "Publication Date",
      "U54 Core Support")
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
      i=1,j=1, part = "footer",
      value = flextable::as_paragraph(
        flextable::as_b(flextable::as_sup("*")),
        flextable::as_b(" Partnership member"))) |>
    flextable::compose(
      i=2,j=1, part= "footer",
      value = flextable::as_paragraph(
        flextable::as_b("REC Trainee") |> dplyr::mutate(underlined=TRUE))
    ) |>
    flextable::compose(
      i=3, j=1, part="footer",
      value = flextable::as_paragraph(
        flextable::as_b("[ESI] Early Stage Investigator"))
    ) |>


    # Last thing should be apply the overall styling
    apply_u54reportr_flextable_style()


  ft
}

