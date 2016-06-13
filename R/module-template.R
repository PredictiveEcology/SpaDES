################################################################################
#' Create new module from template.
#'
#' Autogenerate a skeleton for a new SpaDES module, a template for a
#' documentation file, a citation file, a license file, a README.txt file, and a
#' folder that contains unit tests information.
#' The \code{newModuleDocumentation} will not generate the module file, but will
#' create the other 4 files.
#'
#' All files will be created within a subfolder named \code{name} within the
#' \code{path}.
#'
#' @param name  Character string. Your module's name.
#'
#' @param path  Character string. Subdirectory in which to place the new module code file.
#'              The default is the current working directory.
#'
#' @param open  Logical. Should the new module file be opened after creation?
#'              Default \code{TRUE}.
#'
#' @param unitTests Logical. Should the new module include unit test files?
#'                  Default \code{TRUE}.
#'                  Unit testing relies on the \code{testthat} package.
#'
#' @return Nothing is returned. The new module file is created at
#' \code{path/name.R}, as well as ancillary files for documentation, citation,
#' license, readme, and unit tests folder.
#'
#' @note On Windows there is currently a bug in RStudio that it doesn't know
#' what editor to open with \code{file.edit} is called (which is what moduleName
#' does). This will return an error:
#'
#' \code{Error in editor(file = file, title = title) :}
#' \code{argument "name" is missing, with no default}
#'
#' You can just browse to the file and open it manually.
#'
#' @export
#' @docType methods
#' @rdname newModule
# @importFrom utils file.edit
#' @author Alex Chubaty and Eliot McIntire
#'
#' @examples
#' \dontrun{
#'   ## create a "myModule" module in the "modules" subdirectory.
#'   newModule("myModule", "modules")
#' }
setGeneric("newModule", function(name, path, open, unitTests) {
  standardGeneric("newModule")
})

#' @export
#' @rdname newModule
setMethod(
  "newModule",
  signature = c(name = "character", path = "character", open = "logical",
                unitTests = "logical"),
  definition = function(name, path, open, unitTests) {
    path <- checkPath(path, create = TRUE)
    nestedPath <- file.path(path, name) %>% checkPath(create = TRUE)
    dataPath <- file.path(nestedPath, "data") %>% checkPath(create = TRUE)

    # empty data checksum file
    cat("", file = file.path(dataPath, "CHECKSUMS.txt"))

    # module code file
    newModuleCode(name = name, path = path, open = open)

    if (unitTests) { newModuleTests(name = name, path = path, open = open) }

    ### Make Rmarkdown file for module documentation
    newModuleDocumentation(name = name, path = path, open = open)
})

#' @export
#' @rdname newModule
setMethod(
  "newModule",
  signature = c(name = "character", path = "missing", open = "logical",
                unitTests = "logical"),
  definition = function(name, open, unitTests) {
    newModule(name = name, path = ".", open = open, unitTests = unitTests)
})

#' @export
#' @rdname newModule
setMethod(
  "newModule",
  signature = c(name = "character", path = "character", open = "missing",
                unitTests = "logical"),
  definition = function(name, path, unitTests) {
    newModule(name = name, path = path, open = TRUE, unitTests = unitTests)
})

#' @export
#' @rdname newModule
setMethod(
  "newModule",
  signature = c(name = "character", path = "missing", open = "missing",
                unitTests = "logical"),
  definition = function(name, unitTests) {
    newModule(name = name, path = ".", open = TRUE, unitTests = unitTests)
})

#' @export
#' @rdname newModule
setMethod(
  "newModule",
  signature = c(name = "character", path = "character", open = "logical",
                unitTests = "missing"),
  definition = function(name, path, open) {
    newModule(name = name, path = path, open = open, unitTests = TRUE)
})

#' @export
#' @rdname newModule
setMethod(
  "newModule",
  signature = c(name = "character", path = "missing", open = "logical",
                unitTests = "missing"),
  definition = function(name, open) {
    newModule(name = name, path = ".", open = open, unitTests = TRUE)
})

#' @export
#' @rdname newModule
setMethod(
  "newModule",
  signature = c(name = "character", path = "character", open = "missing",
                unitTests = "missing"),
  definition = function(name, path) {
    newModule(name = name, path = path, open = TRUE, unitTests = TRUE)
})

#' @export
#' @rdname newModule
setMethod(
  "newModule",
  signature = c(name = "character", path = "missing", open = "missing",
                unitTests = "missing"),
  definition = function(name) {
    newModule(name = name, path = ".", open = TRUE, unitTests = TRUE)
})

################################################################################
#' @export
#' @docType methods
#' @rdname newModule
# @importFrom utils file.edit
#' @author Eliot McIntire and Alex Chubaty
#'
setGeneric("newModuleCode", function(name, path, open) {
  standardGeneric("newModuleCode")
})

#' @export
#' @rdname newModule
setMethod(
  "newModuleCode",
  signature = c(name = "character", path = "character", open = "logical"),
  definition = function(name, path, open) {
    path <- checkPath(path, create = TRUE)
    nestedPath <- file.path(path, name) %>% checkPath(create = TRUE)
    filenameR <- file.path(nestedPath, paste0(name, ".R"))
    cat("
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = \"", name, "\",
  description = \"insert module description here\",
  keywords = c(\"insert key words here\"),
  authors = c(person(c(\"First\", \"Middle\"), \"Last\", email=\"email@example.com\", role=c(\"aut\", \"cre\"))),
  childModules = character(),
  version = numeric_version(\"", as.character(packageVersion("SpaDES")), "\"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., \"year\",
  citation = list(\"citation.bib\"),
  documentation = list(\"README.txt\", \"", name, ".Rmd\"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter(\"paramName\", \"paramClass\", value, min, max, \"parameter description\")),
    defineParameter(\".plotInitialTime\", \"numeric\", NA, NA, NA, \"This describes the simulation time at which the first plot event should occur\"),
    defineParameter(\".plotInterval\", \"numeric\", NA, NA, NA, \"This describes the simulation time at which the first plot event should occur\"),
    defineParameter(\".saveInitialTime\", \"numeric\", NA, NA, NA, \"This describes the simulation time at which the first save event should occur\"),
    defineParameter(\".saveInterval\", \"numeric\", NA, NA, NA, \"This describes the simulation time at which the first save event should occur\")
  ),
  inputObjects = data.frame(
    objectName = NA_character_,
    objectClass = NA_character_,
    sourceURL = \"\",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = NA_character_,
    objectClass = NA_character_,
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.", name, " = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == \"init\") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$", name, "Init(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, params(sim)$", name, "$.plotInitialTime, \"", name, "\", \"plot\")
    sim <- scheduleEvent(sim, params(sim)$", name, "$.saveInitialTime, \"", name, "\", \"save\")
  } else if (eventType == \"plot\") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    #Plot(objectFromModule) # uncomment this, replace with object to plot
    # schedule future event(s)

    # e.g.,
    #sim <- scheduleEvent(sim, params(sim)$", name, "$.plotInitialTime, \"", name, "\", \"plot\")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == \"save\") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, \"", name, "\", \"save\")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == \"event1\") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, \"", name, "\", \"templateEvent\")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == \"event2\") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, \"", name, "\", \"templateEvent\")

    # ! ----- STOP EDITING ----- ! #
  } else {
    warning(paste(\"Undefined event type: \'\", current(sim)[1, \"eventType\", with = FALSE],
                  \"\' in module \'\", current(sim)[1, \"moduleName\", with = FALSE], \"\'\", sep = \"\"))
  }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
", name, "Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
", name, "Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
", name, "Plot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot(\"object\")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
", name, "Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event1Test1 <- \" this is test for event 1. \" # for dummy unit test
  sim$event1Test2 <- 999 # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
", name, "Event2 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event2Test1 <- \" this is test for event 2. \" # for dummy unit test
  sim$event2Test2 <- 777  # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above\n",
    file = filenameR, fill = FALSE, sep = "")

    if (open) {
      # use tryCatch: Rstudio bug causes file open to fail on Windows (#209)
      tryCatch(file.edit(filenameR), error = function(e) {
        warning("A bug in RStudio for Windows prevented the opening of the file:\n",
                filenameR, "\nPlease open it manually.")
      })
    }
})

################################################################################
#' @export
#' @docType methods
#' @rdname newModule
# @importFrom utils file.edit
#' @author Eliot McIntire
#'
setGeneric("newModuleDocumentation", function(name, path, open) {
  standardGeneric("newModuleDocumentation")
})

#' @export
#' @rdname newModule
setMethod(
  "newModuleDocumentation",
  signature = c(name = "character", path = "character", open = "logical"),
  definition = function(name, path, open) {
    path <- checkPath(path, create = TRUE)
    nestedPath <- file.path(path, name) %>% checkPath(create = TRUE)
    filenameRmd <- file.path(nestedPath, paste0(name, ".Rmd"))
    filenameCitation <- file.path(nestedPath, "citation.bib")
    filenameLICENSE <- file.path(nestedPath, "LICENSE")
    filenameREADME <- file.path(nestedPath, "README.txt")

    ### Make Rmarkdown file for module documentation
    cat(
"---
title: \"", name, "\"
author: \"Module Author\"
date: \"", format(Sys.Date(), "%d %B %Y"), "\"
output: pdf_document
---

# Overview

Provide an overview of what the module does / how to use the module.

Module documentation should be written so that others can use your module.
This is a template for module documentation, and should be changed to reflect your module.

## RMarkdown

RMarkdown syntax allows R code, outputs, and figures to be rendered in the documentation.

For help writing in RMarkdown, see http://rmarkdown.rstudio.com/.

# Usage

```{r module_usage}
library(SpaDES)
library(magrittr)

inputDir <- file.path(tempdir(), \"inputs\") %>% checkPath(create = TRUE)
outputDir <- file.path(tempdir(), \"outputs\")
times <- list(start = 0, end = 10)
parameters <- list(
  .globals = list(burnStats = \"nPixelsBurned\"),
  #.progress = list(type = \"text\", interval = 1), # for a progress bar
  ## If there are further modules, each can have its own set of parameters:
  #module1 = list(param1 = value1, param2 = value2),
  #module2 = list(param1 = value1, param2 = value2)
)
modules <- list(\"", name, "\")
objects <- list()
paths <- list(
  cachePath = file.path(outputDir, \"cache\"),
  modulePath = file.path(\"..\"),
  inputPath = inputDir,
  outputPath = outputDir
)

mySim <- simInit(times = times, params = parameters, modules = modules,
                 objects = objects, paths = paths)

spades(mySim)
```

# Events

Describe what happens for each event type.

## Plotting

Write what is plotted.

## Saving

Write what is saved.

# Data dependencies

## Input data

How to obtain input data, and a description of the data required by the module.
If `sourceURL` is specified, `downloadData(\"", name, "\", \"path/to/modules/dir\")` may be sufficient.

## Output data

Description of the module outputs.

# Links to other modules

Describe any anticipated linkages to other modules.

",
        file = filenameRmd, fill = FALSE, sep = "")

    ### Make citation.bib file
    cat("
@Manual{,
  title = {", name ,"},
  author = {{Authors}},
  organization = {Organization},
  address = {Somewhere, Someplace},
  year = {", format(Sys.Date(), "%Y"), "},
  url = {},
}
",
        file = filenameCitation, fill = FALSE, sep = "")

    ### Make LICENSE file
    cat("
# Provide explicit details of the license for this module.
# See http://choosealicense.com for help selecting one.",
        file = filenameLICENSE, fill = FALSE, sep = "")

    ### Make README file
    cat("
Any other details that a user may need to know, like where to get more information,
where to download data, etc.",
        file = filenameREADME, fill = FALSE, sep = "")

    if (open) {
      # use tryCatch: Rstudio bug causes file open to fail on Windows (#209)
      tryCatch(file.edit(filenameRmd), error = function(e) {
        warning("A bug in RStudio for Windows prevented the opening of the file:\n",
                filenameRmd, "\nPlease open it manually.")
      })
    }

    return(invisible(NULL))
})

#' @export
#' @rdname newModule
setMethod("newModuleDocumentation",
          signature = c(name = "character", path = "missing", open = "logical"),
          definition = function(name, open) {
            newModuleDocumentation(name = name, path = ".", open = open)
})

#' @export
#' @rdname newModule
setMethod("newModuleDocumentation",
          signature = c(name = "character", path = "character", open = "missing"),
          definition = function(name, path) {
            newModuleDocumentation(name = name, path = path, open = TRUE)
})

#' @export
#' @rdname newModule
setMethod("newModuleDocumentation",
          signature = c(name = "character", path = "missing", open = "missing"),
          definition = function(name) {
            newModuleDocumentation(name = name, path = ".", open = TRUE)
})

################################################################################
#' @export
#' @docType methods
#' @rdname newModule
# @importFrom utils file.edit
#' @author Eliot McIntire and Alex Chubaty
#'
setGeneric("newModuleTests", function(name, path, open) {
  standardGeneric("newModuleTests")
})

#' @export
#' @rdname newModule
setMethod(
  "newModuleTests",
  signature = c(name = "character", path = "character", open = "logical"),
  definition = function(name, path, open) {
    if (!requireNamespace("testthat", quietly = TRUE)) {
      warning('The `testthat` package is required to run unit tests on modules.')
    }
    path <- checkPath(path, create = TRUE)
    nestedPath <- file.path(path, name) %>% checkPath(create = TRUE)
    testDir <- file.path(nestedPath, "tests") %>% checkPath(create = TRUE)
    testthatDir <- file.path(testDir, "testthat") %>% checkPath(create = TRUE)

    # create two R files in unit tests folder:
    unitTestsR <- file.path(testDir, "unitTests.R") # source this to run all tests
    testTemplate <- file.path(testthatDir, "test-template.R")

    cat("
# Please build your own test file from test-Template.R, and place it in tests folder
# please specify the package you need to run the sim function in the test files.

# to test all the test files in the tests folder:
test_dir(\"", testthatDir, "\")

# Alternative, you can use test_file to test individual test file, e.g.:
test_file(\"", file.path(testthatDir, "test-template.R"), "\")\n",
        file = unitTestsR, fill = FALSE, sep = "")

    ## test template file
    cat("
# Please do three things to ensure this template is correctly modified:
# 1. Rename this file based on the content you are testing using
#    `test-functionName.R` format so that your can directly call `moduleCoverage`
#    to calculate module coverage information.
#    `functionName` is a function's name in your module (e.g., `", name, "Event1`).
# 2. Copy this file to the tests folder (i.e., `", testthatDir, "`).\n
# 3. Modify the test description based on the content you are testing:
test_that(\"test Event1 and Event2.\", {
  module <- list(\"", name, "\")
  path <- list(modulePath = \"", path, "\",
               outputPath = file.path(tempdir(), \"outputs\"))
  parameters <- list(
    #.progress = list(type = \"graphical\", interval = 1),
    .globals = list(verbose = FALSE),
    ", name ," = list(.saveInitialTime = NA)
  )
  times <- list(start = 0, end = 1)

  # If your test function contains `time(sim)`, you can test the function at a
  # particular simulation time by defining the start time above.
  object1 <- \"object1\" # please specify
  object2 <- \"object2\" # please specify
  objects <- list(\"object1\" = object1, \"object2\" = object2)

  mySim <- simInit(times = times,
                   params = parameters,
                   modules = module,
                   objects = objects,
                   paths = path)

  # You may need to set the random seed if your module or its functions use the
  # random number generator.
  set.seed(1234)

  # You have two strategies to test your module:
  # 1. Test the overall simulation results for the given objects, using the
  #    sample code below:

  output <- spades(mySim, debug = FALSE)

  # is output a simList?
  expect_is(output, \"simList\")

  # does output have your module in it
  expect_true(any(unlist(modules(output)) %in% c(unlist(module))))

  # did it simulate to the end?
  expect_true(time(output) == 1)

  # 2. Test the functions inside of the module using the sample code below:
  #    To allow the `moduleCoverage` function to calculate unit test coverage
  #    level, it needs access to all functions directly.
  #    Use this approach when using any function within the simList object
  #    (i.e., one version as a direct call, and one with `simList` object prepended).

  if (exists(\"", name, "Event1\", envir = .GlobalEnv)) {
    simOutput <- ", name, "Event1(mySim)
  } else {
    simOutput <- mySim$", name, "Event1(mySim)
  }

  expectedOutputEvent1Test1 <- \" this is test for event 1. \" # please define your expection of your output
  expect_is(class(simOutput$event1Test1), \"character\")
  expect_equal(simOutput$event1Test1, expectedOutputEvent1Test1) # or other expect function in testthat package.
  expect_equal(simOutput$event1Test2, as.numeric(999)) # or other expect function in testthat package.

  if (exists(\"", name, "Event2\", envir = .GlobalEnv)) {
    simOutput <- ", name, "Event2(mySim)
  } else {
    simOutput <- mySim$", name, "Event2(mySim)
  }

  expectedOutputEvent2Test1 <- \" this is test for event 2. \" # please define your expection of your output
  expect_is(class(simOutput$event2Test1), \"character\")
  expect_equal(simOutput$event2Test1, expectedOutputEvent2Test1) # or other expect function in testthat package.
  expect_equal(simOutput$event2Test2, as.numeric(777)) # or other expect function in testthat package.
})",
      file = testTemplate, fill = FALSE, sep = "")
})

################################################################################
#' Open all modules nested within a base directory
#'
#' This is just a convenience wrapper for opening several modules at once, recursively.
#' A module is defined as any file that ends in \code{.R} or \code{.r} and has a
#' directory name identical to its filename. Thus, this must be case sensitive.
#'
#' @param name  Character vector with names of modules to open. If missing, then
#'              all modules will be opened within the basedir.
#'
#' @param path  Character string of length 1. The base directory within which
#'              there are only module subdirectories.
#'
#' @return Nothing is returned. All file are open via \code{file.edit}.
#'
#' @note On Windows there is currently a bug in RStudio that prevents the editor
#' from opening when \code{file.edit} is called:
#'
#' \code{Error in editor(file = file, title = title) :}
#' \code{argument "name" is missing, with no default}
#'
#' The workaround is to browse to the file and open it manually.
#'
#' @export
#' @docType methods
#' @rdname openModules
# @importFrom utils file.edit
#'
#' @author Eliot McIntire
#'
#' @examples
#' \dontrun{openModules("~\SpaDESModules")}
#'
setGeneric("openModules", function(name, path) {
  standardGeneric("openModules")
})

#' @export
#' @rdname openModules
setMethod("openModules",
          signature = c(name = "character", path = "character"),
          definition = function(name, path) {
            basedir <- checkPath(path, create = FALSE)
            origDir <- getwd()
            setwd(basedir)
            if (any(name == "all")) {
              Rfiles <- dir(pattern = "[\\.][rR]$", recursive = TRUE)
            } else {
              Rfiles <- dir(pattern = "[\\.][rR]$", recursive = TRUE)
              Rfiles <- Rfiles[pmatch(name, Rfiles)]
            }
            Rfiles <- Rfiles[grep(pattern = "[/\\\\]",Rfiles)]
            Rfiles <- Rfiles[sapply(strsplit(Rfiles,"[/\\\\\\.]"),
                                    function(x) any(duplicated(x)))]
            lapply(Rfiles, file.edit)
            setwd(origDir)
})

#' @export
#' @rdname openModules
setMethod("openModules",
          signature = c(name = "missing", path = "missing"),
          definition = function() {
            openModules(name = "all", path = ".")
})

#' @export
#' @rdname openModules
setMethod("openModules",
          signature = c(name = "missing", path = "character"),
          definition = function(path) {
            openModules(name = "all", path = path)
})

#' @export
#' @rdname openModules
setMethod("openModules",
          signature = c(name = "character", path = "missing"),
          definition = function(name) {
            openModules(name = name, path = ".")
})

################################################################################
#' Create a zip archive of a module subdirectory
#'
#' The most common use of this would be from a "modules" directory, rather than
#' inside a given module.
#'
#' @param name    Character string giving the module name.
#' @param path    A file path to a directory containing the module subdirectory.
#' @param version The module version.
#' @param ...     Additional arguments to \code{\link{zip}}:
#'                e.g., add \code{"-q"} using \code{flags="-q -r9X"}
#'                (the default flags are \code{"-r9X"}).
#'
#' @author Eliot McIntire and Alex Chubaty
#'
#' @export
#' @rdname zipModule
#'
setGeneric("zipModule", function(name, path, version, ...) {
  standardGeneric("zipModule")
})

#' @export
# @importFrom utils zip
#' @rdname zipModule
setMethod(
  "zipModule",
  signature = c(name = "character", path = "character", version = "character"),
  definition = function(name, path, version, ...) {

    path <- checkPath(path, create = FALSE)

    callingWd <- getwd()
    on.exit(setwd(callingWd))
    setwd(path)
    zipFileName = paste0(name, "_", version, ".zip")
    print(paste("Zipping module into zip file:", zipFileName))
    zip(zipFileName, files = file.path(name), extras = c("-x","*.zip"), ...)
    file.copy(zipFileName, to = paste0(name, "/", zipFileName), overwrite = TRUE)
    file.remove(zipFileName)
})

#' @rdname zipModule
#' @export
setMethod("zipModule",
          signature = c(name = "character", path = "missing", version = "character"),
          definition = function(name, version, ...) {
            zipModule(name = name, path = ".", version = version, ...)
})

#' @export
#' @rdname zipModule
setMethod("zipModule",
          signature = c(name = "character", path = "missing", version = "missing"),
          definition = function(name, ...) {
            vers <- moduleMetadata(name, ".")$version %>% as.character
            zipModule(name = name, path = ".", version = vers, ...)
})

#' @export
#' @rdname zipModule
setMethod("zipModule",
          signature = c(name = "character", path = "character", version = "missing"),
          definition = function(name, path, ...) {
            vers <- moduleMetadata(name, path)$version %>% as.character
            zipModule(name = name, path = path, version = vers, ...)
})
