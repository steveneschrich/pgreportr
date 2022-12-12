test_that("format dates works", {
  expect_equal(format_date_as_month_year(as.Date("2022-01-01")), "Jan 2022")
  expect_equal(format_date_as_mdy(as.Date("2022-01-15")), "01/15/2022")
})
