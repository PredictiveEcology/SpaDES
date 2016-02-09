################################################################################
#' Calculate module coverage of unit tests
#'
#' Calculate the test coverage by unit tests for the module and its functions.
#'
#' @param name  Character string. The module's name.
#'
#' @param path  Character string. The path to the module directory
#'              (default is the current working directory).
#'
#' @return Return a list of two coverage objects and two data.table objects.
#' The two coverage objects are named `moduleCoverage` and `functionCoverage`.
#' The `moduleCoverage` object contains the percent value of unit test coverage
#' for the module.
#' The `functionCoverage` object contains percentage values for unit test
#' coverage for each function defined in the module.
#' Please use \code{\link[covr]{shine}} to view the coverage information.
#' Two data.tables give the information of all the tested and untested functions
#' in the module.
#'
#' @note When running this function, the test files must be strictly placed in
#' the \file{tests/testthat/} directory under module path.
#' To automatically generate this folder, please set \code{unitTests = TRUE}
#' when creating a new module using \code{\link{newModule}}.
#' To accurately test your module, the test filename must follw the format
#' \code{test-functionName.R}.
#'
#' @seealso \code{\link{newModule}}.
#'
#' @include simList-class.R
#' @importFrom data.table data.table
#' @export
#' @docType methods
#' @rdname moduleCoverage
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#'  library(igraph)
#'  library(SpaDES)
#'  tmpdir <- file.path(tempdir(), "coverage")
#'  modulePath <- file.path(tmpdir, "Modules") %>% checkPath(create = TRUE)
#'  moduleName <- "forestAge" # sample module to test
#'  downloadModule(name = moduleName, path = modulePath) # download sample module
#'  testResults <- moduleCoverage(name = moduleName, path = modulePath)
#'  shine(testResults$moduleCoverage)
#'  shine(testResults$functionCoverage)
#'  unlink(tmpdir, recursive = TRUE)
#' }
setGeneric("moduleCoverage", function(name, path) {
  standardGeneric("moduleCoverage")
})

#' @export
#' @rdname moduleCoverage
setMethod(
  "moduleCoverage",
  signature(name = "character", path = "character"),
  definition = function(name, path) {
    tmpdir <- file.path(tempdir(), "moduleCoverage")
    dir.create(tmpdir); on.exit(unlink(tmpdir, recursive = TRUE))
    fnDir <- file.path(tmpdir, "moduleFunctions") %>% checkPath(create = TRUE)
    testDir <- file.path(path, name, "tests", "testthat")

    if (!requireNamespace("covr", quietly = TRUE) ||
        !requireNamespace("testthat", quietly = TRUE)) {
      stop("Suggested packages `covr` and `testthat` not found. ",
           "Both must be installed to test module coverage.")
    }
    stopifnot(dir.exists(testDir))

    fnCoverage <- list()
    mCoverage <- list()
    untestedFunctions <- data.table(FunctionName = character())
    testedFunctions <- data.table(FunctionName = character(), Coverage = numeric())
    dummyTestFile <- file.path(tmpdir, "test-dummyTestFile.R")
    cat("test_that(\"this is a temporary dummy test file. \", {\n",
        "  expect_equal(1, 1) \n",
        "})\n", file = dummyTestFile, fill = FALSE, sep = "")
    # read the module
    mySim <- simInit(times = list(start = 0, end = 1),
                     params = list(),
                     modules = list(paste0(name)),
                     objects = list(),
                     paths = list(modulePath = path, outputPath = tmpdir))

    objects <- mget(objects(mySim), envir(mySim))
    objects <- objects[which(lapply(objects, is.function) == TRUE)]
    fnIndex <- which(names(objects) != paste("doEvent.", name, sep = ""))

    for (i in fnIndex) {
      fnName <- file.path(fnDir, paste0(names(objects[i]), ".R", sep = ""))
      fnLines <- deparse(objects[i][[1]])
      cat(names(objects[i]), " <- ", fnLines[1:2], "\n", sep = "", file = fnName)
      cat(fnLines[3:length(fnLines)], sep = "\n", file = fnName, append = TRUE)
      source(fnName)
    }
    rm(i)

    for (i in fnIndex) {
      testfiles <- file.path(testDir, paste0("test-", objects(mySim)[i], ".R"))
      if (file.exists(testfiles)) {
        mTest <- covr::function_coverage(
          objects(mySim)[i], env = envir(mySim),
          testthat::test_file(testfiles, env = envir(mySim))
        )
        fnTest <- covr::function_coverage(objects(mySim)[i],
                                          testthat::test_file(testfiles))
        testedFunctions <- rbind(
          testedFunctions,
          data.table(FunctionName = objects(mySim)[i],
                     Coverage = round(covr::percent_coverage(fnTest), 2))
        )
        mCoverage <- append(mCoverage, mTest)
        fnCoverage <- append(fnCoverage, fnTest)

      } else {
        mTest <- covr::function_coverage(
          objects(mySim)[i], env = envir(mySim),
          testthat::test_file(dummyTestFile, env = envir(mySim))
        )
        fnTest <- covr::function_coverage(
          objects(mySim)[i], testthat::test_file(dummyTestFile)
        )
        untestedFunctions <- rbind(
          untestedFunctions,
          data.table(FunctionName = objects(mySim)[i])
        )
        mCoverage <- append(mCoverage, mTest)
        fnCoverage <- append(fnCoverage, fnTest)
      }
    }
    class(mCoverage) <- "coverage"
    class(fnCoverage) <- "coverage"
    unlink(tmpdir, recursive = TRUE)
    return(list(moduleCoverage = mCoverage,
                functionCoverage = fnCoverage,
                testedFunctions = testedFunctions,
                untestedFunctions = untestedFunctions))
})

#' @export
#' @rdname moduleCoverage
setMethod(
  "moduleCoverage",
  signature(name = "character", path = "missing"),
  definition = function(name) {
    moduleCoverage(name = name, path = ".")
})
