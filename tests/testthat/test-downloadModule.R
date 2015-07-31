test_that("downloadModule downloads and unzips module files", {
  tmpdir <- file.path(tempdir(), "modules")
  on.exit(unlink(tmpdir, recursive=TRUE))

  f <- downloadModule("caribouMovementLcc", tmpdir)

  expect_more_than(length(f), 0)
  expect_more_than(length(file.path(tmpdir)), 0)
  expect_more_than(length(file.path(tmpdir, "caribouMovementLcc")), 0)
})
