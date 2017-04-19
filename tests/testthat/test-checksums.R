test_that("checksums read and written correctly", {
  library(magrittr)

  sampleDir <- system.file("maps", package = "SpaDES")
  sampleFiles <- list.files(sampleDir, pattern = "[.]tif", full.names = TRUE)
  tmpdir <- file.path(tempdir(), "test_checksums", "data") %>%
    checkPath(create = TRUE)
  on.exit(unlink(dirname(tmpdir), recursive = TRUE), add = TRUE)

  file.copy(sampleFiles, tmpdir)

  csf <- file.path(tmpdir, "CHECKSUMS.txt")
  cnames.r <- c("result", "expectedFile", "actualFile", "checksum.x", "checksum.y",
                "algorithm.x", "algorithm.y")
  cnames.w <- c("file", "checksum", "algorithm")
  csums <- c("77c56d42fecac5b1", "8affcdf311555fd6", "e2dd8734d6ed3d05",
             "f21251dcdf23dde0", "86e342cfc6876b7d")

  # 1. read checksums without CHECKSUMS.txt file
  expect_error(checksums("test_checksums", dirname(dirname(tmpdir))))

  # 2. read checksums with empty CHECKSUMS.txt file
  file.create(csf)
  txt <- checksums("test_checksums", dirname(dirname(tmpdir)))
  expect_true(all(colnames(txt) == cnames.r))
  expect_equal(nrow(txt), 0)

  # 3. write checksums without CHECKSUMS.txt
  file.remove(csf)
  txt <- checksums("test_checksums", dirname(dirname(tmpdir)), write = TRUE)
  expect_true(all(colnames(txt) == cnames.w))
  expect_equal(nrow(txt), 5)
  expect_true(all(txt$file == basename(sampleFiles)))
  expect_true(all(txt$checksum == csums))

  # 4. read checksums with non-empty CHECKSUMS.txt file
  out <- data.frame(file = basename(sampleFiles[-1]),
                    checksum = csums[-1],
                    algorithm = c("xxhash64", "xxhash64", "xxhash64", "xxhash64"),
                    stringsAsFactors = FALSE)
  write.table(out, csf, eol = "\n", col.names = TRUE, row.names = FALSE)

  txt <- checksums("test_checksums", dirname(dirname(tmpdir)), write = TRUE)
  expect_true(all(colnames(txt) == cnames.w))
  expect_equal(nrow(txt), 5)
  expect_true(all(txt$file == basename(sampleFiles)))
  expect_true(all(txt$checksum == csums))
})
