test_that("module templates work", {
  library(knitr)
  library(igraph)

  path <- file.path(tempdir(), "modules") %>% checkPath(create = TRUE)

  on.exit({
    detach('package:knitr')
    detach('package:igraph')
    unlink(path, recursive = TRUE)
  })

  expect_true(file.exists(path))
  moduleName <- "myModule"

  newModule(moduleName, path, FALSE, unitTests = TRUE)

  mpath <- file.path(path, moduleName)

  expect_true(file.exists(mpath))
  expect_true(file.exists(file.path(mpath, "citation.bib")))
  expect_true(file.exists(file.path(mpath, "LICENSE")))
  expect_true(file.exists(file.path(mpath, paste0(moduleName, ".R"))))
  expect_true(file.exists(file.path(mpath, paste0(moduleName, ".Rmd"))))
  expect_true(file.exists(file.path(mpath, "README.txt")))
  expect_true(dir.exists(file.path(mpath, "data")))
  expect_true(dir.exists(file.path(mpath, "tests")))
  expect_true(dir.exists(file.path(mpath, "tests", "testthat")))
  expect_true(file.exists(file.path(mpath, "tests", "unitTests.R")))
  expect_true(file.exists(file.path(mpath, "tests", "testthat", "test-template.R")))
  expect_true(file.exists(file.path(mpath, "data", "CHECKSUMS.txt")))

  utils::capture.output(
    zipModule(name = moduleName, path = path, version = "0.0.2", flags = "-q -r9X")
  )

  expect_true(file.exists(file.path(mpath, paste0(moduleName, "_0.0.2.zip"))))

  # Test that the .Rmd file actually can run with knitr
  expect_equal(knitr::knit(input = file.path(mpath, paste0(moduleName, ".Rmd")),
                           output = file.path(mpath, paste0(moduleName, ".md")),
                           quiet = TRUE),
               file.path(mpath, paste0(moduleName, ".md")))

  # Test that the dummy unit tests work
  #test_file(file.path(mpath, "tests", "testthat", "test-template.R"))
})
