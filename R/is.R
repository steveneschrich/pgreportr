
#' Title
#'
#' @param .x
#' @param support
#'
#' @return
#' @export
#'
#' @examples
is_supported_by <- function(.x=NULL, support) {
  dispatch_on_field(.x, paste0("isSupportedBy_", support))
}

#' @describeIn is_supported_by Is grant supported by a resource?
#' @export
is_grant_supported_by <- function(.x, ...) {
  is_supported_by(.x, ...)
}

#' @describeIn is_supported_by Is publication supported by a resource?
#' @export
is_pub_supported_by <- function(.x, ...) {
  is_supported_by(.x, ...)
}

#' @describeIn is_supported_by Is presentation supported by a resource?
#' @export
is_presentation_supported_by <- function(.x, ...) {
  is_supported_by(.x, ...)

}

#' Title
#'
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
is_support <- function(.x) {
  dispatch_on_field(tb, name="is_support")
}

#' Title
#'
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
is_core_support <- function(.x) {
  dispatch_on_field(tb, name="is_core_support")
}


#' @describeIn is_core_support Is grant supported by core
#' @export
is_grant_core_support <- function(.x) {
  is_core_support(.x)
}

#' @describeIn is_core_support Is pub supported by core
#' @export
is_pub_core_support <- function(.x) {
  is_core_support(.x)
}
#' @describeIn is_core_support Is presentation supported by core
#' @export
is_presentation_core_support <- function(.x) {
  is_core_support(.x)
}

#' Title
#'
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
is_other_support <- function(.x) {
  dispatch_on_field(tb, name="is_other_support")
}

#' @describeIn is_other_support Is grant supported by other support
#' @export
is_grant_other_support <- function(.x) {
  is_other_support(.x)
}
#' @describeIn is_other_support Is pub supported by other support
#' @export
is_pub_other_support <- function(.x) {
  is_other_supported(.x)
}

#' @describeIn is_other_support Is presentation supported by other support
#' @export
is_presentation_other_support <- function(.x) {
  is_other_supported(.x)
}

#' Title
#'
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
list_support <- function(.x) {
  stringr::str_remove(
    stringr::str_subset(colnames(.x),"^isSupportedBy_"),"^isSupportedBy_"
  )
}


#' Is the grant funded?
#'
#' Return a logical value for each grant status in the supplied list,
#' indicating if the grant is funded or not.
#'
#' The grant table field to use is Grant Status
#'
#' @param f String (or vector of strings) with Grant Status
#'
#' @return Logical indicating if the grant has been funded.
#' @export
#'
is_grant_funded<-function(tb=NULL) {
  dispatch_on_field(tb, name="is_grant_funded")
}

#' Is the grant not funded (a decision has been made)?
#'
#' Return a logical value for each grant status in the supplied list,
#' indicating if the grant is officially not funded.
#'
#' The grant table field to use is Grant Status
#'
#' Note this differs from pending
#' review, in which the decision has not yet been made.
#'
#' @param f String (or vector of strings) with Grant Status
#'
#' @return Logical indicating if the grant has not been funded.
#' @export
#'
is_grant_not_funded<-function(tb=NULL) {
  dispatch_on_field(tb, name="is_grant_not_funded")
}

#' Is the grant pending review?
#'
#' Return a logical value for each grant status in the supplied list,
#' indicating if the grant is pending review or not.
#'
#' The grant table field to use is Grant Status
#'
#' @param f String (or vector of strings) with Grant Status
#'
#' @return Logical indicating if the grant is pending review.
#' @export
#'
is_grant_pending_review<-function(tb=NULL) {
  dispatch_on_field(tb, name="is_grant_pending_review")
}


#' Is the grant submitted?
#'
#' Return a logical value for each grant status in the supplied list,
#' indicating if the grant is submitted or not.
#'
#' The grant table field to use is Grant Status
#'
#' @param f String (or vector of strings) with Grant Status
#'
#' @return Logical indicating if the grant is submitted.
#' @export
#'
is_grant_submitted<-function(tb=NULL) {
  dispatch_on_field(tb, name="is_grant_submitted")
}

#' Is the grant in preparation?
#'
#' The field is_grant_in_preparation indicates if the grant is in preparation.
#' Return a vector of logicals indicating if the grants represented by that
#' field are in preparation or not.
#'
#' Note this function primarily exists as a method for documenting the use of the field
#' for users of the
#' library, not necessarily library developers.
#'
#' @param f A vector of logical values extracted from the is_grant_in_preparation field.
#'
#' @return A vector of logical values indicating if the grant is In Preparation.
#' @export
#'
is_grant_in_preparation<-function(tb=NULL) {
  dispatch_on_field(tb, name="is_grant_in_preparation")
}





#' Is the grant R type?
#'
#' This is an indicator if the grant is an Rtype grant (e.g., R01, R03)
#' Return a vector of logicals indicating if the grants represented by that
#' field are R-type or not (or the field name if tb is not included).
#'
#'
#' @param tb A grants table or nothing (in which case the field name is returned).
#'
#' @return A vector of logical values indicating if the grant is Rtype or field name.
#' @export
#'
#' @example
is_grant_rtype<-function(tb=NULL) {
  dispatch_on_field(tb, name="is_grant_rtype")
}


#' Is the grant a joint grant between the partner institutions?
#'
#' This is an indicator if the grant is a joint grant between PHSU and Moffitt (a criteria
#' for reporting). Return a vector of logicals indicating if the grants represented by that
#' field are joint or not (or the field name if tb is not included).
#'
#'
#' @param tb A grants table or nothing (in which case the field name is returned).
#'
#' @return A vector of logical values indicating if the grant is PHSU Cancer Related or field name.
#' @export
#'
#' @example
#
is_grant_joint<-function(tb=NULL) {
  dispatch_on_field(tb, name="is_grant_joint")
}

#' Is the grant an ESI-related grant?
#'
#' This is an indicator if the grant is an ESI-related grant (a criteria
#' for reporting). In particular, an ESI-related grant is one in which any of the
#' key personnel listed in the database is an ESI.
#'
#' Return a vector of logicals indicating if the grants represented by that
#' field are ESI-related or not (or the field name if tb is not included).
#'
#'
#' @param .x A grants table or nothing (in which case the field name is returned).
#'
#' @return A vector of logical values indicating if the grant is ESI-related or the field name.
#' @export
#'
#' @example
#
is_current_esi_related <- function(.x=NULL) {
  dispatch_on_field(.x, name="is_current_esi_related")
}

is_former_esi_related <- function(.x = NULL) {
  dispatch_on_field(.x, name="is_former_esi_related")
}


#' @export
is_grant_current_esi_related<-function(...) {
  is_current_esi_related(...)
}
is_grant_former_esi_related <- function(...) {
  is_former_esi_related(...)
}

#' @export
is_publication_current_esi_related <- function(...) {
  is_current_esi_related(...)
}
is_pub_current_esi_related <- is_publication_current_esi_related
is_publication_former_esi_related <- function(...) {
  is_former_esi_related(...)
}
is_pub_former_esi_related <- is_publication_former_esi_related


#' @export
is_presentation_current_esi_related <- function(...) {
  is_current_esi_related(...)
}
is_presentation_former_esi_related <- function(...) {
  is_former_esi_related(...)
}


#' Operator overloading function
#'
#' For many cases, we would like to not expose the specifics of the indicator variables in the
#' dataset explicitly, since function calls are the things that are well documented in the library.
#'
#' Therefore, we essentially have getters for a variety of variables but with a small twist. The
#' simplest form of usage is to say grants %>% is_phsu_cancer_related() or more simply,
#' is_phsu_cancer_related(grants). This would return the underlying variable
#' (is_phsu_cancer_related) which is already a logical. However, it would be
#' clearly documented as a function call (rather than having to know the specific field).
#'
#' The second, less clear usage of the function is when trying to use it in a dplyr statement. For instance,
#' in filter: grants %>% filter(!!is_phsu_cancer_related()) allows us to filter on a field that I might
#' not know the name of but is clearly documented within the function.
#'
#' This function is the general-purpose operator overload dispatcher that figures out, based on whether
#' or not an argument is provided, whether to return the variable name (for !!) or to return a list of
#' values (if I provided a table).
#'
#' In practical terms (within the library), you can then define a new function is_phsu_related() that
#' just calls this dispatcher with the appropriate variable name. Essentially a java-like getter.
#'
#'
#'
#' @param tb A data.frame or tibble to extract a value from. May be NULL to retrieve the name itself.
#' @param name The name of the variable to extract
#'
#' @return Either a name (as a symbol, if the table value is NULL) or a list of values from the field.
#' @export
#'
#' @examples
dispatch_on_field<-function(tb, name) {
  if ( is.null(tb))
    rlang::sym(name)
  else
    dplyr::pull(tb, !!name)

}




#' Does investigator have a target role
#'
#' @param role
#' @param target
#'
#' @return A logical indicating if the investigator is the role
#' @export
#'
#' @examples
is_role <- function(role, target) {
  if (length(role) == 0)
    FALSE
  else
    !is.na(role) & grepl(target, role)
}

#' @describeIn is_role Is investigator ESI?
#' @export
is_current_esi_investigator <- function(role) is_role(role, "Current ESI")

is_former_esi_investigator <- function(role) is_role(role, "Former ESI")

#' @describeIn is_role Is the role a New Investigator role?
#' @export
is_new_investigator<-function(role) is_role(role, "New Investigator")


#' @describeIn is_role Is the role a trainee?
#' @export
is_trainee<-function(role) is_role(role, "Trainee")



#' Extract tag
#'
#' @param .x
#' @param tag
#'
#' @return
#' @export
#'
#' @examples
has_tag <- function(.x=NULL, tag) {
  dispatch_on_field(.x, paste0("isTag_",tag))
}



#' Title
#'
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
list_tags <- function(.x) {
  stringr::str_remove(stringr::str_subset(colnames(.x),"^isTag_"),"^isTag_")
}

#' Title
#'
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
is_creator_current_esi <- function(.x) {
  .x == "Current ESI"
}
is_creator_former_esi <- function(.x) {
  .x == "Former ESI"
}
#'
#' @export
is_author_current_esi <- function(.x) is_creator_current_esi(.x)
is_author_former_esi <- function(.x) is_creator_former_esi(.x)

#'
#' @export
is_investigator_current_esi <- function(.x) is_creator_current_esi(.x)

is_investigator_former_esi <- function(.x) is_creator_former_esi(.x)
