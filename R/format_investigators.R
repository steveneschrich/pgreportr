#' Collapse investigators to a single string
#'
#' There are (potentially) multiple investigators per grant. This function will
#' reduce this to a single text string per grant, adding it to the grants table as the
#' `Investigators` field.
#'
#' @details
#' Given the grants table, the current implementation is to have investigators (a tibble)
#' nested inside of the grant tibble. This is convenient to retain the dplyr semantics,
#' but at some point the investigators have to be collapsed to a single string. This is that
#' function.
#'
#' The job here is to take the investigators out, format the investigators per the style
#' guidelines, then store the resulting strings back into the grants table.
#'
#' @param grants the grants tibble
#'
#' @return A grants tibble with a new column (Investigators).
#' @export
#'
collapse_investigators<-function(grants) {
  grants %>%
    tidyr::unnest(investigators) %>%
    dplyr::group_by(`grant_id`) %>%
    dplyr::summarize(
      `Investigators`=paste(format_investigators_text(dplyr::cur_data()), collapse="; ")
    ) %>%
    dplyr::right_join(grants, by=c("grant_id"="grant_id"))

}

collapse_invstigators_alt <- function(.x) {
  purrr::map_chr(.x, function(.y) {
    .y %>%
      format_investigators_text()
  })
}






##############################################
# BELOW HERE IS GOOD
##############################################



#' Format an investigator group as a flextable chunk
#'
#'
#' An investigator group is a tibble of investigators that are considered a
#' grouping, for the purposes of reporting. Examples of groups include an
#' institution, or by Role. The goal of this function is to format this
#' group of investigators into a list of flextable "chunks".
#'
#' @details
#' The investigator group (a tibble of related investigators) should be
#' formatted as a list of flextable chunks. A group label can be applied
#' to these investigators, potentially formatted itself.
#'
#' The `label` field is an overloaded field. It is either a simple text string
#' representing the name of the group, or a tibble. If it is a tibble, it is
#' expected to contain a single value representing the label to use. The parameter
#' is constructed this way to enable the `dplyr::group_map()` function to work,
#' since it passes a tibble with the grouped variables.
#'
#' @param i An investigator tibble
#' @param label A text string or a tibble containing one value
#' @param label_sep Separator between label and investigators (default: newline)
#' @param sep Separator between investigators (default: newline)
#' @return A flextable chunk representing the group.
#' @export
#'
format_investigator_group_as_flextable<-function(investigators, label="",
                                                        label_sep="\n",
                                                        sep="\n", summary_field="Investigator Summary",
                                                        ...) {

  # The label is either a tibble (with one entry, representing the label) or
  # just a plain string.
  if ( is.data.frame(label))
    label <- as.character(label[[1]])

  # Create investigator chunks.
  formatted_investigators<-investigators %>%
    arrange_investigators_by_role() %>%
    dplyr::rowwise() %>%
    dplyr::summarize(
      list(dplyr::bind_rows(
        flextable::as_chunk(format_investigator_name(.data$investigator_name, ...), ),
        flextable::as_chunk(ifelse(is.na(.data$investigator_role), "", paste0(" (",.data$investigator_role,")"))),
        flextable::as_sup(ifelse(isPartnershipRole_ESI,"1","")),
        flextable::as_chunk(sep)
      ))
    ) %>%
    # Extract all of the tibble rows to a single tibble.
    stats::setNames(nm=NULL) %>%
    purrr::map_dfr(dplyr::bind_rows) %>%
    # Filter all empty rows out
    dplyr::filter(!txt == "") %>%
    # Remove final entry (sep after last investigator).
    dplyr::slice(-dplyr::n())
    #unlist(., recursive = FALSE, use.names=FALSE)

  # Final output is bold label, investigators, and newline.
  out<-dplyr::bind_rows(
    flextable::as_chunk(paste0(label,label_sep), props=officer::fp_text(bold=TRUE)),
    formatted_investigators
  )

  # Needed since the class information is lost in translation
  class(out)<-c("chunk", "data.frame")

  out
  #list(list(),
  #     formatted_investigators) %>%
  #  unlist(., recursive = FALSE, use.names=FALSE)



}


#' Format investigator groups as text chunks.
#'
#' Given an investigator group, format the output to include the list name
#' and values as a text chunk.
#'
#' @details
#' The format of the input should be `group`=`string`. This creates a text summary
#' by combining the `group` and `string` into a single string (separated by newline).
#'
#' @param i The investigator group
#'
#' @return
#' @export
#'
#' @examples
format_investigator_group_as_text<-function(investigators, label="",
                                                   label_sep=": ", sep="; ",
                                            summary_field="Investigator Summary", ...) {
  # The label is either a tibble (with one entry, representing the label) or
  # just a plain string.
  if ( is.data.frame(label))
    label <- as.character(label[[1]])


  # Create formatted investigator name
  investigators <- investigators |>
    arrange_investigators_by_role() |>
    dplyr::mutate(
      formatted_investigator_name = sprintf("%s%s%s", format_investigator_name(investigator_name,...),
                                                        str_enclose(investigator_role),
                                                        ifelse(isPartnershipRole_ESI," [ESI]", "")
    ))

  # Final output is label, investigators. Only do label
  # if not empty or NA.
  retstr <- stringi::stri_flatten(investigators$formatted_investigator_name, collapse=sep)

  if ( !is.na(label) && !(label==""))
    retstr <- stringi::stri_flatten(c(label, retstr), collapse = label_sep)

  retstr


}




#' Format a list of investigator flextable chunks.
#'
#' Given a list of possible grouped investigators as flextable chunks, format this into a
#' single flextable paragraph.
#'
#' @param l A list of investigator flextable chunks
#'
#' @return A flextable paragraph representing all of the chunks.
#' @export
#'
#' @examples
combine_investigator_groups_as_flextable<-function(l, sep="\n") {
  # This bit of dark magic combines the list of tibbles into a single
  # tibble (adding in a separator between each).
  combined_tibble<-purrr::map_dfr(l, function(x) {
    bind_rows(x, flextable::as_chunk(sep))
  }) %>%
  # Remove final entry (sep after last investigator). There must be a cleaner way.
  dplyr::slice(-dplyr::n())

  # And this bit (from split.dataframe source code) turns it all into a list.
  tibble_as_list<-lapply(split(1:nrow(combined_tibble), f=1:nrow(combined_tibble)), function(ind) {
    combined_tibble[ind, , drop=FALSE]
  })

  # Then we can convert to a paragraph, whew!
  flextable::as_paragraph(list_values=tibble_as_list)

}


#' Format a list of investigator text chunks.
#'
#' Given a list of possibly grouped investigators as text chunks, format this into a single
#' string as text.
#'
#' @details
#' Combining investigators into a single string is a two-step process. First, each group
#' can be combined into a text chunk. This list of text chunks would then be further combined
#' into a single string (this function). The idea is that this process can be done for
#' both flextables and text using the same type of functions. This function handles
#' text output.
#'
#' @param l A list of investigator text chunks
#'
#' @return A string representing all investigators
#' @export
#'
combine_investigator_groups_as_text<-function(l, sep=" | ",...) {
  stringr::str_c(l, collapse=sep)
}


#' Format investigators by group
#'
#' Given a list of investigator tibbles, format the investigators of each tibble by grouping.
#'
#' @details
#' This function assumes there is a list of tibbles provided, as one might expect to have
#' by extracting the `investigators` field from a `grants` tibble. This list of investigators
#' will be processed individually.
#'
#' Each investigator tibble will be grouped by the `group` variable, then summarized according
#' to the `format_investigator_group` function (see format_investigator_group_as_* for ideas).
#' Each investigator group will then be combined together as a final output using the
#' `format_investigator_group_as_paragraph` function.
#'
#' The final result of this function call should be a list of the same length as the input
#' (`investigators_list`) with each value being an appropriate representation of the investigators,
#' grouped by `group` and formatted by `format_investigator_group` and
#' `format_investigator_group_as_paragraph`.
#'
#' NB: This might be better if we use a flag like "flextable" which would then find the appropriate
#' functions, assuming they exist. Then the twin functions don't have to be explicitly named.
#'
#' NB: There is a bug in group_map that it doesn't allow for groupings by variables (e.g.,
#' the "group" variable, which when you group by would be !!group). It for some reason uses
#' the literal value as opposed to interpreting. So for now, ignore the group variable and
#' have explicit functions for each grouping variable of interest (probably not many anyway).
#' @param investigators_list
#' @param group
#' @param format_investigator_group
#' @param format_investigator_groups_as_paragraph
#' @param summary_field What field to use for the investigator (defaults to `Investigator Summary`)
#' @return
#' @export
#'
format_investigators_by_institution<-function(investigators_list, group,
                                              format_investigator_group,
                                              combine_investigator_groups,
                                              ...) {


  # Special case: if the investigators list is empty then return an empty
  # string.
  if (length(investigators_list)< 1 | nrow(investigators_list[[1]]) == 0)
    return("")

  # The investigators list is a list of investigator tibbles. Extract from each
  # a collapsed version of investigators (as a list of concatenated investigators).
  annotated_investigators<-purrr::map(investigators_list, function(investigators) {
    investigators %>%
      # Arrange by the role, then transform institution into levels ordered by role.
      arrange_investigators_by_role() %>%
      dplyr::mutate(investigator_institution=factor(investigator_institution, levels=unique(investigator_institution))) %>%
      dplyr::group_by(investigator_institution) %>%
      dplyr::group_map(format_investigator_group, ...) %>%
      # Given a tibble of formatted investigators, collect into a single formatted paragraph.
      combine_investigator_groups()
  }) %>%
    # This is needed since thanks to lists we are a level too deep...
    unlist(recursive=FALSE)

  annotated_investigators
}


#' Title
#'
#' @param investigators_list
#' @param format_investigator_group
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
format_investigators<-function(investigators_list, format_investigator_group, combine_investigator_groups, ...) {

  # Special case: if the investigators list is empty then return an empty
  # string.
  if (length(investigators_list)==1 & rlang::is_empty(investigators_list[[1]]))
    return("")

  annotated_investigators<-purrr::map_chr(investigators_list, function(investigators) {
    investigators |>
      arrange_investigators_by_role() |>
      dplyr::group_map(format_investigator_group,...) |>
      combine_investigator_groups()
  })

  annotated_investigators
}





#' @describeIn format_name Format investigator
format_investigator_name <- function(...) {
  pgimportr::format_name(...)
}

#' @describeIn format_name Format author
format_author_name <- function(...) {
  pgimportr::format_name(...)
}
