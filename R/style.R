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
  d |>
    dplyr::mutate(`planned_date_of_submission`=lubridate::ymd(`planned_date_of_submission`)) |>
    dplyr::mutate(`Submission Date` =
                   dplyr::coalesce(submission_date, `planned_date_of_submission`),
                  `Grant Type` = grant_type,
                  Source = grant_source,
                  Title = grant_title,
                  `Grant Status` = grant_status,
                  `U54 Support` = support,
                  # combined not via stringr though. That's more tidyr
                  `ESI Related` = vec_unite(
                    c(ifelse(is_current_esi_related, "Current ESI","")),
                    c(ifelse(is_former_esi_related, "Former ESI", "")),
                    sep=", ",
                    na.rm = TRUE,
                    na.empty = TRUE
                  )

    )|>
    dplyr::select(`Grant Type`, `Source`, `Submission Date`,  `investigators`, `Title`, `Grant Status`,`U54 Support`,`ESI Related`) |>
    dplyr::mutate(`Grant Status` =
                    dplyr::case_when(
                      `Grant Status` == "Funded" ~"F",
                      `Grant Status` == "In Preparation" ~ "IP",
                      `Grant Status` == "Not Funded" ~ "NF",
                      `Grant Status` == "Pending Review" ~ "PR"
                    )) |>

    # Add to flextable, this is likely to change.
    flextable::flextable(col_keys=c("Grant Type","Source","Submission Date","Investigators","Title","Grant Status",
                                    "U54 Support", "ESI Related",
                                    "investigators"),

                         cwidth=c(0.2, 0.5, 0.5,1.75,2.5, 0.4, 0.4,0.3,1)) %>%
    # Manually set widths (using template).
    #flextable::width(j=1:7,
    #                 width=c(1.12, 0.81, 1.57, 0.94, 0.63, 1.75, 1.75)) %>%
    # Collapse investigators (tibble) to single flextable cells.
    flextable::compose(j = ~`Investigators`,
                       value = format_investigators_by_institution(investigators,
                                                                   group="investigator_institution",
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
    flextable::bold(j= ~`Grant Type`, part="body") |>

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
        "Role: \u2020 (Current ESI), * (Former ESI)"
      ),
      ref_symbols = c("1")
    ) |>
    # Add Grant Status footnote.
    flextable::footnote(i = 1, j = 6, part = "header",
      value = flextable::as_paragraph(
        "Grant Status: F (Funded), NF (Not Funded), PR (Pending Review), IP (In Preparation)"
      ),
      ref_symbols = c("2")
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
style_grants_as_flextable_epsilon<-function(d, ...) {

  pgyrs <- pg_project_years()
  d |>
    dplyr::mutate(
      `Grant Number` = dplyr::row_number(),
      `planned_date_of_submission`=lubridate::ymd(`planned_date_of_submission`),
      `Submission Date` = dplyr::coalesce(submission_date, planned_date_of_submission),
      ESI = ifelse(is_current_esi_related, "Current ESI-related",""),
      `Grant Status` =
        dplyr::case_when(
          `grant_status` == "Funded" ~"F",
          `grant_status` == "In Preparation" ~ "IP",
          `grant_status` == "Not Funded" ~ "NF",
          `grant_status` == "Pending Review" ~ "PR"
        ),
      `U54 Year` = pg_year,
      Source = grant_source,
      Title = grant_title
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
                                                                   group="investigator_institution",
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



    # flextable::footnote(i = 1, j = 2, part = "header",
    #                     value = flextable::as_paragraph(
    #                       "U54 Year\n",
    #                       dplyr::select(d, "U54 Year") |>
    #                         dplyr::filter(!stringr::str_detect(`U54 Year`, "\\(")) |>
    #                         dplyr::distinct() |>
    #                         dplyr::left_join(pgyrs, by=c("U54 Year" = "name")) |>
    #                         dplyr::mutate(
    #                           yr_annotation = sprintf("%s: %s - %s",`U54 Year`, start_date, end_date)
    #                         ) |>
    #                         dplyr::pull(yr_annotation) |>
    #                         stringr::str_c(collapse="\n")
    #                     ),
    #                     ref_symbols = c("2")
    # ) |>
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
                                                                    group="investigator_institution",
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
  table |>
    flextable::bg(bg="#4F81BD", part="header") |>
    flextable::color(color="#FFFFFF", part="header") |>
    flextable::set_table_properties(width=1.0, layout="autofit") |>
    flextable::fontsize(size=8, part = "all")
}






