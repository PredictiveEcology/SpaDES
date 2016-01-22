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
#' @return Return two coverage objects: moduleCoverage and functionCoverage.
#' The moduleCoverage contains percentage of coverage by unit tests for the module.
#' The functioinCoverage contains percentages of coverage by unit tests for functions in the module.
#' The returned two objects are compatible to \code{shine} function in \code{covr} package.
#' Please use \code{shine} to view the information of coverage.
#'
#' @note For running this function, the tests file must be restrictly placed in tests/testthat folder under module path.
#'       To automatically generate this folder, please set unitTests = TRUE when develop a new module using \code{\link{newModule}}.
#'
#' @seealso \code{\link{newModule}}.
#'
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname moduleCoverage
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#'  library(magrittr)
#'  library(SpaDES)
#'  tmpdir <- tempdir()
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
    tmpdir <- tempdir(); on.exit(unlink(tmpdir, recursive = TRUE))
    fnDir <- file.path(path, name, "moduleFunctions") %>%
      checkPath(create = TRUE)
    testDir <- file.path(path, name, "tests", "testthat")

    if (!requireNamespace("covr", quietly = TRUE) ||
        !requireNamespace("testthat", quietly = TRUE)) {
      stop("Suggested packages `covr` and `testthat` not found. ",
           "Both must be installed to test module coverage.")
    }
    stopifnot(dir.exists(testDir))

    fCoverage <- list()
    mCoverage <- list()

    # read the module
    mySim <- simInit(times = list(start = 0, end = 1),
                     params = list(),
                     modules = list(paste0(name)),
                     objects = list(),
                     paths = list(modulePath = path, outputPath = tmpdir))

    objects <- mget(objects(mySim), envir(mySim))
    fnIndex <- which(lapply(objects, is.function) == TRUE)

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
        mTest <- covr::function_coverage(objects(mySim)[i], env = envir(mySim),
                                         testthat::test_file(testfiles, env = envir(mySim)))
        fnTest <- covr::function_coverage(objects(mySim)[i],
                                          testthat::test_file(testfiles))
        mCoverage <- append(mCoverage, mTest)
        fnCoverage <- append(fnCoverage, fnTest)
      } else {
        mTest <- covr::function_coverage(objects(mySim)[i], env = envir(mySim),
                                         testthat::test_dir(testDir, env = envir(mySim)))
        fnTest <- covr::function_coverage(objects(mySim)[i],
                                          testthat::test_dir(testDir))
        mCoverage <- append(mCoverage, mTest)
        fnCoverage <- append(fnCoverage, fnTest)
      }
    }
    class(mCoverage) <- "coverage"
    class(fnCoverage) <- "coverage"
    unlink(fnDir, recursive = TRUE)
    return(list(moduleCoverage = mCoverage, functionCoverage = fnCoverage))
})

#' @export
#' @rdname moduleCoverage
setMethod(
  "moduleCoverage",
  signature(name = "character", path = "missing"),
  definition = function(name) {
    moduleCoverage(name = name, path = ".")
})
