test_that("paste_noNA works as singleton", {
  expect_equal(paste_noNA("Test","String"), "Test, String")
  expect_equal(paste_noNA("Test","String", sep=">>"), "Test>>String")
  expect_equal(paste_noNA("Test",""), "Test")
  expect_equal(paste_noNA("","Test"), "Test")
  expect_equal(paste_noNA("",""),"")
  expect_equal(paste_noNA("Test",NA), "Test")
  expect_equal(paste_noNA(NA, "Test"), "Test")
  expect_equal(paste_noNA(NA, NA), "")
})

test_that("paste_noNA works as vectors", {
  expect_equal(paste_noNA(c("T1","T2","T3"), c("A1","A2","A3")), c("T1, A1","T2, A2","T3, A3"))
  expect_equal(paste_noNA(c("T1"), c("T2")), "T1, T2")
  expect_equal(paste_noNA(c(NA,"T2","T3"), c("A1","A2","A3")), c("A1","T2, A2","T3, A3"))
  expect_equal(paste_noNA(c(NA,"T2","T3"), c("A1",NA,"A3")), c("A1","T2","T3, A3"))
  expect_equal(paste_noNA(c(NA,"T2",NA), c("A1",NA,"A3")), c("A1","T2","A3"))
  expect_equal(paste_noNA(c(NA,"T2","T3"), c(NA,"A2","A3")), c("","T2, A2","T3, A3"))
  expect_equal(paste_noNA(c(NA,NA,NA), c("A1","A2","A3")), c("A1","A2","A3"))
})

