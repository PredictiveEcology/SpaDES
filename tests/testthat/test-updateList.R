test_that("updateList behaves correctly", {
  L1 <- list(a = "hst", b = NA_character_, c = 43)
  L2 <- list(a = "gst", c = 42, d = list(letters))
  L12 <- list(a = "gst", b = NA_character_, c = 42, d = list(letters))

  expect_equal(L1, updateList(NULL, L1))
  expect_equal(L1, updateList(L1, NULL))
  expect_equal(updateList(L1, L2), L12)

  L3 <- list("pst", 41, list(LETTERS))
  expect_error(updateList(L1, L3), "All elements in lists x,y must be named.")
  expect_error(updateList(NULL, L3), "All elements in list y must be named.")
  expect_error(updateList(L3, NULL), "All elements in list x must be named.")

  expect_equal(updateList(NULL, NULL), list())
})
