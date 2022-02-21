#' PGRT - Program Grant Reporting Tool. NIH Program Grants (e.g., U54, SPORE, etc) often involve
#' the tracking and reporting of scientific outcomes from the research. This involves outcomes such
#' as number of grant submissions, number of papers published, and other types of metrics. This type
#' of information is very helpful for understanding the productivity of the program, however it can
#' be rather cumbersome to collect, organize and report. The PGOR solution is a two-fold approach:
#' data is collected through a series of REDCap forms and reported through an R library (pgor). This
#' combination allows for distributed data collection (e.g., all investigators can update their own
#' grant records) and automated, reproducible reporting (through R/markdown).
#'
#' The pgrt library is a package for loading REDCap outcomes data, processing and filtering this
#' data, and finally reporting this data in various summaries and tables. Underlying this entire
#' process is the concept of an over-arching program grant (`pg`) that partially funds these
#' activities and supports infrastructure to accomplish the program aims.
#'
#' Briefly, a REDCap server is used to collect and curate the raw data on submitted/funded grants,
#' as well as publications. This library is then used to extract the data from the REDCap server
#' and turn it into reports, tables and other reportables.
#'
#' GRANTS
#' Most of the current functionality is in grant submissions and funded grants.
#' Key to this package is the grants table. It is a tibble (tidyverse) that contains grants, one
#' line per grant. There are a number of different columns useful for sorting/filtering, and each
#' one has an embedded tibble representing investigators.
#'
#' @docType package
#' @name u54reportr
#' @import assertthat
#' @importFrom utils head tail
NULL
#> NULL
