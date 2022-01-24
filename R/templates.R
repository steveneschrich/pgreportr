#' Load docx template
#'
#' For reporting, there are several templates available for generating
#' reports. This function will load the template (selected by the tag name)
#' and return it for use by officer, etc.
#'
#' The template is particularly important because it defines various styles that
#' are assumed to exist and be configured a certain way for the needed look. These
#' template styles can be altered in the template documents, thereby changing the
#' look of the report relatively straightforwardly.
#'
#' @param tag The template (currently monthly or yearly).
#'
#' @return A docx object based on the template.
#' @export
#'
get_docx_template<-function(tag="monthly") {

  file<-switch(stringr::str_to_lower(tag),
               monthly="Monthly_Report_Template.docx",
               yearly="Yearly_Report_Template.docx"
  )

  template_file<-system.file("extdata/templates", file, package="pgreportr")
  doc <- officer::read_docx(path = template_file)

}
