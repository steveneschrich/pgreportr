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
  label<-ifelse(tibble::is_tibble(label),
                label %>% dplyr::pull(1) %>% as.character(),
                label)

  # Create investigator chunks.
  formatted_investigators<-investigators %>%
    arrange_investigators_by_role() %>%
    dplyr::rowwise() %>%
    dplyr::summarize(
      list(
        flextable::as_chunk(format_investigator_name(.data$Investigator, ...), ),
        flextable::as_chunk(ifelse(is.na(.data$Role), "", paste0(" (",.data$Role,")"))),
        flextable::as_sup(ifelse(is_esi_investigator(`Partnership Role`),"1","")),
        flextable::as_chunk(sep)
      )
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
  label<-ifelse(tibble::is_tibble(label),
                label %>% dplyr::pull(1) %>% as.character(),
                label)

  # Create investigator chunks.
  formatted_investigators<-investigators %>%
    arrange_investigators_by_role() %>%
    dplyr::rowwise() %>%
    dplyr::summarize(
      list(
        format_investigator_name(.data$Investigator, ...),
        ifelse(is.na(.data$Role), "", paste0(" (",.data$Role,")")),
        ifelse(is_esi_investigator(`Partnership Role`)," [ESI]",""),
        sep
      )
    ) %>%
    # This code mirrors the flextable, so the result of the above is a list of bits. Combine them.
    unlist() %>%
    # The trailing sep is not needed
    head(-1) %>%
    # Combine all elements into a single string.
    stringr::str_c(collapse="")


  # Final output is label, investigators. Only do label
  # if not empty or NA.
  if ( !is.na(label) && !(label==""))
    formatted_investigators<-stringr::str_c(label, label_sep, formatted_investigators)


  formatted_investigators


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
      dplyr::mutate(Institution=factor(Institution, levels=unique(Institution))) %>%
      dplyr::group_by(Institution) %>%
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
    investigators %>%
      arrange_investigators_by_role() %>%
      dplyr::group_map(format_investigator_group,...) %>%
      combine_investigator_groups()
  })

  annotated_investigators
}



#' Format investigator name
#'
#' For reporting, the investigator full name is often not needed because it takes up too much space.
#' To uniquely identify a person, we use the convention of `GivenNameInitials. LastName, PhD`
#' for instance.
#'
#' @details
#' The data for pgreportr uses full names for investigators. Shortening these names to first initials
#' is tricky since people may have multiple given names (i.e., middle names). This code takes all names
#' except the last name (tokenized by whitespace) and abbreviates them, followed by a period. The last
#' name is then added and returned.
#'
#' The degree (in the form of ,degree) is another variation. This code handles it's presence
#' or absence. You can use the use_degree flag to print it out (or not).
#'
#' So, for instance
#' in the case of
#' ```
#'      John James Smith
#' ```
#' his formatted name would be
#' ```
#'      JJ. Smith
#' ```
#'
#' Note that this function supports either a single name or a vector of names, and will transform
#' accordingly.
#'
#' Of note, while this code is reasonably competent it is by no means bullet-proof. You would need
#' to check the results to verify it's worked as desired.
#'
#' TODO: There is bug if someone's name is John (Joe) Smith. The parens screw up a regex or
#' something.
#'
#' @param n String representing the investigator name
#' @param use_degree Should the degree be printed in the name?
#' @param use_first_name_only Should only the first given name be used?
#' @param use_initials Should only initials of the given name(s) be used?
#' @param use_period_after_initials Should a period follow given name initials?
#' @param use_last_name_first Should it be last name, first?
#'
#' @return A shortened name. If a vector is provided, a vector of shortened names is returned.
#'
#' @export
#'
#'
format_investigator_name<-function(n, use_degree=TRUE, use_first_name_only=FALSE,
                                   use_initials=TRUE, use_period_after_initials=TRUE,
                                   use_last_name_first=FALSE, ...) {
  # Name should be of the form
  # First other last, Degree
  name_and_degree<-stringr::str_split_fixed(n, ",( )*", n=2)


  # Get the space-separated fields
  nsplit<-stringr::str_split(name_and_degree[,1], " ")

  # Last name is the last one of the list
  last_name<-purrr::map_chr(nsplit, function(y){y[length(y)]})

  # Given names are all except the last.
  given_names<-purrr::map(nsplit, function(y){
    if ( length(y) > 1)
      y[1:length(y)-1]
    else
      NA
  })


  # Given name initials are the first character of all non-last name elements.
  # Note we replace empty with NA (degenerate case of no first name).
  if ( use_initials ) {
    given_names<-purrr::map(given_names, function(y){
      dplyr::na_if(stringr::str_sub(y[1:length(y)], start=1,end=1), "")
    })
  }

  # Reduce to first name/initial if needed
  given_names <- if ( use_first_name_only ) {
    purrr::map(given_names,1)
  } else {
    purrr::map_chr(given_names, function(s){
      stringr::str_c(s, collapse=ifelse(use_initials,""," "))
    })
  }

  # Add in the trailing period after initials, if needed
  if ( use_initials && use_period_after_initials )
    given_names<-purrr::map_chr(given_names, function(s) stringr::str_c(s, ".",sep=""))

  # Sometimes we have last, first ...
  if ( use_last_name_first ) {
    tmp<-given_names
    given_names<-last_name
    last_name<-tmp
    rm(tmp)
  }

  # Now collapse given and last name, with punctuation/spacing
  # given_name possibly ., then space unless it's NA and then nothing
  name<-purrr::map2_chr(given_names, last_name, function(given,last) {

    if (any(is.na(c(given,last))))
      stats::na.omit(c(given,last))
    else
      stringr::str_c(given, last, sep=ifelse(use_last_name_first,", "," "))

  })


  if ( use_degree )
    paste_noNA(name, name_and_degree[,2], sep=", ")
  else
    name
}




