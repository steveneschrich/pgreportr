test_that("shorten_given_names", {
  expect_equal(u54reportr::format_investigator_name("John Joseph Smith"), "JJ. Smith" )
  expect_equal(u54reportr::format_investigator_name("John J Smith"), "JJ. Smith")
  expect_equal(u54reportr::format_investigator_name("John Smith"), "J. Smith")
  expect_equal(u54reportr::format_investigator_name(c("John Joseph Smith","Franklin Delano Roosevelt")), c("JJ. Smith","FD. Roosevelt"))
  expect_equal(format_investigator_name(c("Smith","John Smith")), c("Smith","J. Smith"))
})
