# A program grant is a set of parameters associated with the grant.
# There are a number of helper functions and accessors for this. For
# now, these are reexported from pgimportr.

#' @importFrom pgimportr convert_date_to_pg_year
#' @export
pgimportr::convert_date_to_pg_year

#' @importFrom pgimportr pg_start_date
#' @export
pgimportr::pg_start_date

#' @importFrom pgimportr pg_project_years
#' @export
pgimportr::pg_project_years

#' @importFrom pgimportr pg_project_period
#' @export
pgimportr::pg_project_period

#' @importFrom pgimportr pg_final_year
#' @export
pgimportr::pg_final_year

# The grant and publication tables are tibbles with a specific type (and
# a couple of attributes extra). There are helper functions for these
# types in pgimportr which are re-exported here for convenience.
#
# source: types.R in pgimportr

#' @importFrom pgimportr is_grant_table
#' @export
pgimportr::is_grant_table

#' @importFrom pgimportr is_publication_table
#' @export
pgimportr::is_publication_table

#' @importFrom pgimportr dictionary
#' @export
pgimportr::dictionary

