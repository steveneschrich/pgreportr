#' Does the given string include a special tag (^n^)
#'
#' To allow for formatting of strings in a table format, we insert
#' special tags (^n^) that can be found and replaced with formatting
#' once we layout the table.
#'
#' @param str
#'
#' @return
#' @export
#'
#' @examples
str_includes_tag<-function(str) {
  !is.na(str) & stringr::str_starts(str, "\\^\\d\\^")
}


#' Replace a special tag with superscript formatting.
#'
#' @param str
#'
#' @return
#' @export
#'
#' @examples
str_replace_tag_superscript<-function(str) {
  notes<-stringr::str_replace_all(stringr::str_extract_all(str, "\\^\\d\\^", simplify=TRUE), "\\^","")
  txt<-stringr::str_replace_all(str, "\\^\\d\\^", "")
  c(lapply(notes, as_sup), txt)
}

#' Format a flextable conditionally.
#'
#' @param ft - Flextable to operate on
#' @param column - Name of column to modify
#' @param cf - conditional function (only apply formatting if function is true)
#' @param ff - formatting function (defaults to as_b)
#'
#' @return
#' @export
#'
#' @examples
format_table_conditionally<-function(ft, column, cf=function() {TRUE}, ff=flextable::as_b) {
  format_field(ft, 1, column=column, cf=cf, ff=ff)
}

#' Apply a formatting function to a flextable field conditionally.
#'
#' @param ft - Flextable to operate on
#' @param row - Current row of table to work on
#' @param column - Name of column to modify
#' @param cf - conditional function (only apply formatting if function is true)
#' @param ff - formatting function (defaults to as_b)
#'
#' @return A modified flextable.
#' @export
#'
#' @examples
format_field<-function(ft, row, column, cf, ff=flextable::as_b) {
  modified_ft<-ft
  if ( row == nrow(ft$body$dataset)+1)
    modified_ft
  else {
    val<-ft$body$dataset[row,column]
    if (cf(val)) val<-ff(val)

    modified_ft<-flextable::compose(format_field(ft=ft, row=row+1, column, cf, ff),
                                    i=row,
                                    j=column,
                                    value=flextable::as_paragraph(list_values=val)
    )
  }
  modified_ft

}


#' Format grants and investigators for MS-Word reporting.
#'
#' This function does multiple things with respect to formatting the output
#' of a grant table. It is assumed that the grant table has been filtered if
#' needed. This table will then be annotated with investigator information and
#' formatted using flextable.
#'
#' @param d A composite (grant, investigator) object to report on.
#'
#' @return
#' @export
#'
#' @examples
format_grants_flextable<-function(grants, investigators) {
  grants %>%
    dplyr::left_join(format_investigators(investigators), by=c("grant_id"="grant_id")) %>%
    dplyr::select(`grant_id`, `U54 Fiscal Year Submitted`, `U54 Fiscal Year Funded`,
                  `Grant Type`, `Source`, `Investigators`, `Title`, `Grant Status`, `U54 Core Support`,
                  `Partnership Role`) %>%

    # Create a summary U54 Year, combining submitted and funded.
    dplyr::mutate(`U54 Year`=u54reportr::derive_fiscal_year_summary(`U54 Fiscal Year Submitted`, `U54 Fiscal Year Funded`)) %>%


    # Arrange by fiscal year submitted.
    dplyr::arrange(`U54 Fiscal Year Submitted`, `Source`) %>%
    # For flextable, select the columns to show (including some for intermediate work).
    flextable::flextable(col_keys=c("U54 Year", "Grant Type","Source","Investigators","Title",
                                    "Grant Status", "U54 Core Support",
                                    "Partnership Role", "grant_id")) %>%

    flextable::bg(bg="#4F81BD", part="header") %>%
    flextable::color(color="#FFFFFF", part="header") %>%
    flextable::set_table_properties(width=1.0, layout="autofit")  %>%
    flextable::fontsize(size=8, part="header") %>%
    flextable::merge_v(j="grant_id", target=c("grant_id",
                                              "U54 Year",
                                              "Grant Type",
                                              "Source",
                                              "Title",
                                              "Grant Status",
                                              "U54 Core Support")) %>%
    flextable::bold(i = ~ is_esi_investigator(`Partnership Role`) | is_new_investigator(`Partnership Role`) | is_trainee(`Partnership Role`), j = ~ Investigators) %>%

    flextable::void(c("Partnership Role", "grant_id"), part="body") %>%
    flextable::void(c("Partnership Role", "grant_id"), part="header") %>%
    format_table_conditionally(., "Investigators", cf=str_includes_tag, ff=str_replace_tag_superscript)

}




#' Title
#'
#' @param grants
#' @param investigators
#'
#' @return
#' @export
#'
#' @examples
format_grants_flextable_2<-function(grants, investigators,
                                    show_grant_status=TRUE,
                                    show_u54_year=TRUE) {
  grants %>%
    dplyr::left_join(collapse_investigator_table(investigators), by=c("grant_id"="grant_id")) %>%
    dplyr::select(`grant_id`, `U54 Fiscal Year Submitted`, `U54 Fiscal Year Funded`,
                  `Grant Type`, `Source`, `Investigators`, `Title`, `Grant Status`, `U54 Core Support`) %>%

    # Create a summary U54 Year, combining submitted and funded.
    dplyr::mutate(`U54 Year`=u54reportr::derive_fiscal_year_summary(`U54 Fiscal Year Submitted`, `U54 Fiscal Year Funded`)) %>%


    # Arrange by fiscal year submitted.
    dplyr::arrange(`U54 Fiscal Year Submitted`, `Source`) %>%
    # For flextable, select the columns to show (including some for intermediate work).
    flextable::flextable(col_keys=stats::na.omit(c(ifelse(show_u54_year, "U54 Year",NA),
                                            "Grant Type","Source",
                                            "Investigators","Title",
                                            ifelse(show_grant_status, "Grant Status", NA),
                                            "U54 Core Support",
                                            "Partnership Role"))) %>%

    flextable::bg(bg="#4F81BD", part="header") %>%
    flextable::color(color="#FFFFFF", part="header") %>%
    flextable::set_table_properties(width=1.0, layout="autofit")  %>%
    #    flextable::merge_v(j="grant_id", target=c("grant_id",
    #                                              "U54 Year",
    #                                              "Grant Type",
    #                                              "Source",
    #                                              "Title",
    #                                              "Grant Status",
    #                                              "U54 Core Support")) %>%
    flextable::bold(i = ~ is_esi_investigator(`Partnership Role`) | is_new_investigator(`Partnership Role`) | is_trainee(`Partnership Role`), j = ~ Investigators) %>%

    flextable::bold(j="Grant Type")
  #%>%
  #  format_table_conditionally(., "Investigators", cf=str_includes_tag, ff=str_replace_tag_superscript)

}



#' Title
#'
#' @param grants
#' @param investigators
#'
#' @return
#' @export
#'
#' @examples
format_grants_as_flextable_3<-function(grants,
                                    show_grant_status=TRUE,
                                    show_u54_year=TRUE) {
  grants %>%
    dplyr::mutate(Investigators=stringr::str_replace_all(Investigators, ";","\n")) %>%
   # dplyr::select(`grant_id`, `U54 Fiscal Year Submitted`, `U54 Fiscal Year Funded`,
  #                `Grant Type`, `Source`, `Investigators`, `Title`, `Grant Status`, `U54 Core Support`) %>%

    # Arrange by fiscal year submitted.
   # dplyr::arrange(`U54 Fiscal Year Submitted`, `Source`) %>%
    # For flextable, select the columns to show (including some for intermediate work).
    #flextable::flextable(col_keys=na.omit(c(ifelse(show_u54_year, "U54 Year",NA),
    ##                                        "Grant Type","Source",
    ##                                        "Investigators","Title",
    #                                        ifelse(show_grant_status, "Grant Status", NA),
    #                                        "U54 Core Support",
    #                                        "Partnership Role"))) %>%

    flextable::flextable() %>%
    flextable::bg(bg="#4F81BD", part="header") %>%
    flextable::color(color="#FFFFFF", part="header") %>%
    flextable::set_table_properties(width=1.0, layout="autofit")  %>%
    #    flextable::merge_v(j="grant_id", target=c("grant_id",
    #                                              "U54 Year",
    #                                              "Grant Type",
    #                                              "Source",
    #                                              "Title",
    #                                              "Grant Status",
    #                                              "U54 Core Support")) %>%
    #flextable::bold(i = ~ is_esi_investigator(`Partnership Role`) | is_new_investigator(`Partnership Role`) | is_trainee(`Partnership Role`), j = ~ Investigators) %>%

    flextable::bold(j="Grant Type")
  #%>%
  #  format_table_conditionally(., "Investigators", cf=str_includes_tag, ff=str_replace_tag_superscript)

}





