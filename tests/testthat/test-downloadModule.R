test_that("downloadModule downloads and unzips module files", {
  tmpdir <- file.path(tempdir(), "modules")

  f <- downloadModule("caribouMovementLcc", tmpdir)
  expect_more_than(length(f), 0)
  expect_more_than(length(file.path(tmpdir)), 0)
  expect_more_than(length(file.path(tmpdir, "caribouMovementLcc")), 0)

  unlink(tmpdir, recursive=TRUE)
})
