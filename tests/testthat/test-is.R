# Edge cases in these functions are important, but a little strange.
# It should work if one of the values is NA (returning a F), but
# also if you send in something empty altogether.
test_that("is_esi_investigator works", {
  expect_equal(is_esi_investigator(c("ESI","",NA)), c(T,F,F))
  expect_equal(is_esi_investigator(c(), c(FALSE)))
})
