test_that("multiplication works", {
  investigators<-tibble::tribble(
    ~investigator_id, ~Investigator, ~Institution, ~Role, ~`Partnership Role`,
    1, "John Smith, PhD", "MCC", "Co-I","ESI",
    2, "Jane Smith, MD", "PHSU", "PI", "Member"
  )
  testthat::expect_equal(arrange_investigators_by_role(investigators)$investigator_id, c(2,1))
})
