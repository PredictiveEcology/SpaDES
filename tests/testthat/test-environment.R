test_that(".spadesEnv functions work", {
  test1 <- 1L:10L

  SpaDES:::.assignSpaDES("test1", test1)
  expect_true(exists("test1", envir = SpaDES:::.spadesEnv))
  expect_true(SpaDES:::.existsSpaDES("test1"))
  expect_equal(test1, SpaDES:::.getSpaDES("test1"))

  changeObjEnv("test1", environment(), SpaDES:::.spadesEnv, TRUE)
  expect_false(exists("test1", envir = SpaDES:::.spadesEnv))
})
