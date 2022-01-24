# grant_qc
# The goal of this file (and functions) is to provide a QC framework built on the
# testthat package, that would allow you to run a long series of tests throughout
# the process of loading/annotating the package. This is built up from a series
# of individual experiences that we have run into when dealing with this data.


evaluate_grant_qc <- function(grants) {

  testthat::test_that("Grant Status is not empty.", {
    checkmate::expect_character(grants$`Grant Status`, any.missing=FALSE)
  })

  testthat::test_that("Grant Status has only allowed values.", {
    testthat::expect_setequal(grants$`Grant Status`,
                              c("Funded","Not Funded","In Preparation","Pending Review","Resubmission"))
  })

  testthat::test_that("Grant submission date only for funded, not funded, pending review.", {
    testthat::expect_true(all(
      !is.na(grants$`Submission Date`) ==
        grants$`Grant Status` %in% c("Funded","Not Funded","Pending Review"))
    )

  })

  testthat::test_that("Grant submission date empty for in prep", {
    testthat::expect_true(all(
      is.na(grants$`Submission Date`) == grants$`Grant Status` %in% c("In Preparation")
    ))
  })

  testthat::test_that("Grant ID unique and non-empty", {
    checkmate::expect_numeric(grants$grant_id, lower=1, finite=TRUE, any.missing=FALSE, unique=TRUE)
  })

  testthat::test_that("Grant Title is not null", {
    checkmate::expect_character(grants$Title, any.missing=FALSE)
  })



  testthat::test_that("Project date conversion works.", {
    testthat::expect_true(all(is.na(grants$project_period)==is.na(grants$`Funding Start Date`)))
    testthat::expect_true(all(is.na(grants$project_period)==is.na(grants$`Funding End Date`)))
  })

  mismatched_start <- which(!(is.na(grants$project_period)==is.na(grants$`Funding Start Date`)))
  mismatched_end <- which(!(is.na(grants$project_period)==is.na(grants$`Funding End Date`)))


  testthat::test_that("Submitted grant has appropriate status.", {
    testthat::expect_true(all(grants$is_grant_submitted==!is.na(grants$`Submission Date`)))
    testthat::expect_true(all(grants$`Grant Status`[grants$is_grant_submitted] %in%
                                c("Pending Review","Funded","Not Funded")))
  })
}


test_raw<-function(grants) {

  testthat::test_that("Submission date is ok.", {
    testthat::expect_true()
  })
}
