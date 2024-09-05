test_that("Investigator formatting works", {
  expect_equal(format_name("John Jay Smith, PhD"), "JJ. Smith, PhD")
  expect_equal(format_name("John Jay Smith, PhD, MD"), "JJ. Smith, PhD, MD")
  expect_equal(format_name("Smith, PhD"), "Smith, PhD")
  expect_equal(format_name("John Jay Smith, PhD", use_degree = FALSE), "JJ. Smith")
  expect_equal(format_name("John Jay Smith, PhD, MD", use_degree = FALSE), "JJ. Smith")
  expect_equal(format_name("Smith, PhD", use_degree = FALSE), "Smith")

  name_list<-c("John Jay Smith", "Smith","John Smith")
  expect_equal(format_name(name_list,
                                   use_first_name_only=FALSE,
                                   use_initials=TRUE,
                                   use_period_after_initials = TRUE,
                                   use_last_name_first=FALSE), c("JJ. Smith","Smith","J. Smith"))

  expect_equal(format_name(name_list,
                                   use_first_name_only=TRUE,
                                   use_initials=TRUE,
                                   use_period_after_initials = TRUE,
                                   use_last_name_first=FALSE), c("J. Smith","Smith","J. Smith"))

  expect_equal(format_name(name_list,
                                   use_first_name_only=TRUE,
                                   use_initials=TRUE,
                                   use_period_after_initials = FALSE,
                                   use_last_name_first=FALSE), c("J Smith","Smith","J Smith"))
  expect_equal(format_name(name_list,
                                   use_first_name_only=TRUE,
                                   use_initials=FALSE,
                                   use_period_after_initials = FALSE,
                                   use_last_name_first=FALSE), c("John Smith","Smith","John Smith"))
  expect_equal(format_name(name_list,
                                   use_first_name_only=TRUE,
                                   use_initials=FALSE,
                                   use_period_after_initials = TRUE,
                                   use_last_name_first=FALSE), c("John Smith","Smith","John Smith"))

  expect_equal(format_name(name_list,
                                   use_first_name_only=FALSE,
                                   use_initials=FALSE,
                                   use_period_after_initials = TRUE,
                                   use_last_name_first=TRUE), c("Smith, John Jay","Smith","Smith, John"))

  expect_equal(format_name(name_list,
                                   use_first_name_only=TRUE,
                                   use_initials=FALSE,
                                   use_period_after_initials = TRUE,
                                   use_last_name_first=TRUE), c("Smith, John","Smith","Smith, John"))

  expect_equal(format_name(name_list,
                                   use_first_name_only=TRUE,
                                   use_initials=TRUE,
                                   use_period_after_initials = FALSE,
                                   use_last_name_first=TRUE), c("Smith, J","Smith","Smith, J"))

  expect_equal(format_name(name_list,
                                   use_first_name_only=TRUE,
                                   use_initials=TRUE,
                                   use_period_after_initials = TRUE,
                                   use_last_name_first=TRUE), c("Smith, J.","Smith","Smith, J."))

  expect_equal(format_name(name_list,
                                   use_first_name_only=FALSE,
                                   use_initials=TRUE,
                                   use_period_after_initials = TRUE,
                                   use_last_name_first=TRUE), c("Smith, JJ.","Smith","Smith, J."))


  })
