test_that("module coverage work 1", {
  library(data.table); on.exit(detach("package:data.table"))
  library(covr); on.exit(detach("package:covr"))
  library(dplyr); on.exit(detach("package:dplyr"))
  library(testthat); on.exit(detach("package:testthat"))

  name <- "testModule"
  tmpdir <- tempdir()
  path <- file.path(tmpdir, "testModule") %>% checkPath(create = TRUE)
  newModule(name = name, path = path, open = FALSE)
  moduleCoverageTest <- moduleCoverage(name = name, path = path)
  expect_is(moduleCoverageTest, "list")
  expect_equal(names(moduleCoverageTest),
               c("moduleCoverage", "functionCoverage",
                 "testedFunctions", "untestedFunctions"))
  expect_is(moduleCoverageTest$moduleCoverage, "coverage")
  expect_equal(names(attributes(moduleCoverageTest$moduleCoverage)),
               c("names", "class"))

  expect_is(moduleCoverageTest$functionCoverage, "coverage")
  expect_equal(names(attributes(moduleCoverageTest$functionCoverage)),
               c("names", "class"))
  expect_equal(percent_coverage(moduleCoverageTest$moduleCoverage),0)
  expect_equal(percent_coverage(moduleCoverageTest$functionCoverage),0)
  expect_is(moduleCoverageTest$testedFunctions, "data.table")
  expect_is(moduleCoverageTest$untestedFunctions, "data.table")
  rm(moduleCoverageTest)
  unlink(tmpdir, recursive = TRUE)

  tmpdir <- tempdir()
  path <- file.path(tmpdir, "testModule") %>% checkPath(create = TRUE)
  newModule(name = name, path = path, open = FALSE)
  moduleCoverageTest <- moduleCoverage(name = name, path = path,
                                       byFunctionName = FALSE)
  expect_is(moduleCoverageTest, "list")
  expect_equal(names(moduleCoverageTest),
               c("moduleCoverage", "functionCoverage",
                 "testedFunctions", "untestedFunctions"))
  expect_is(moduleCoverageTest$moduleCoverage, "coverage")
  expect_equal(names(attributes(moduleCoverageTest$moduleCoverage)),
               c("names", "class"))
  expect_is(moduleCoverageTest$functionCoverage, "coverage")
  expect_equal(names(attributes(moduleCoverageTest$functionCoverage)),
               c("names", "class"))
  expect_equal(percent_coverage(moduleCoverageTest$moduleCoverage),60)
  expect_equal(percent_coverage(moduleCoverageTest$functionCoverage),60)
  expect_equal(moduleCoverageTest$testedFunctions,
               data.table(FunctionName = c("testModuleEvent1", "testModuleEvent2"),
                          Coverage = 100))
  expect_equal(moduleCoverageTest$untestedFunctions,
               data.table(FunctionName = c("testModuleInit", "testModulePlot",
                                           "testModuleSave")))
  unlink(tmpdir, recursive = TRUE)
})
