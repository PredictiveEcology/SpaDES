################################################################################
#' Open a file for editing
#'
#' RStudio's \code{file.edit} behaves differently than \code{utils::file.edit}.
#' The workaround is to have the user manually open the file if they are using
#' RStudio, as suggested in the RStudio support ticket at
#' \url{https://support.rstudio.com/hc/en-us/community/posts/206011308-file-edit-vs-utils-file-edit}.
#'
#' @param file  Character string giving the file path to open.
#'
#' @return  Invoked for its side effect of opening a file for editing.
#'
#  @importFrom utils file.edit
#'
#' @rdname fileEdit
#' @author Alex Chubaty
#' @keywords internal
#'
.fileEdit <- function(file) {
  if (Sys.getenv("RSTUDIO") == "1") {
    file <- gsub(file, pattern = "\\./", replacement = "")
    message("Using RStudio, open file manually with:\n",
            paste0("file.edit('", file, "')")
    )
  } else {
    file.edit(file)
  }
}

################################################################################
#' Create new module from template
#'
#' Autogenerate a skeleton for a new SpaDES module, a template for a
#' documentation file, a citation file, a license file, a \file{README.txt} file,
#' and a folder that contains unit tests information.
#' The \code{newModuleDocumentation} will not generate the module file, but will
#' create the other files.
#'
#' All files will be created within a subdirectory named \code{name} within the
#' \code{path}:
#'
#' \itemize{
#'   \item \code{path/}
#'     \itemize{
#'       \item \code{name/}
#'       \item \code{R/               # contains additional module R scripts}
#'       \item \code{data/            # directory for all included data}
#'       \itemize{
#'         \item \code{CHECKSUMS.txt  # contains checksums for data files}
#'       }
#'       \item \code{tests/           # contains unit tests for module code}
#'       \item \code{citation.bib     # bibtex citation for the module}
#'       \item \code{LICENSE.txt      # describes module's legal usage}
#'       \item \code{README.txt       # provide overview of key aspects}
#'       \item \code{name.R           # module code file (incl. metadata)}
#'       \item \code{name.Rmd         # documentation, usage info, etc.}
#'     }
#' }
#'
#' @param name  Character string specifying the name of the new module.
#'
#' @param path  Character string. Subdirectory in which to place the new module code file.
#'              The default is the current working directory.
#'
#' @param ...   Additional arguments. Currently, only the following are supported:\cr\cr
#'
#'              \code{open}. Logical. Should the new module file be opened after creation?
#'              Default \code{TRUE}.\cr\cr
#'
#'              \code{unitTests}. Logical. Should the new module include unit test files?
#'              Default \code{TRUE}. Unit testing relies on the \code{testthat} package.\cr\cr
#'
#'              \code{type}. Character string specifying one of \code{"child"} (default),
#'              or \code{"parent"}.\cr\cr
#'
#'              \code{children}. Required when \code{type = "parent"}. A character vector
#'              specifying the names of child modules.
#'
#' @return Nothing is returned. The new module file is created at
#' \file{path/name.R}, as well as ancillary files for documentation, citation,
#' \file{LICENSE}, \file{README}, and \file{tests} directory.
#'
#' @note On Windows there is currently a bug in RStudio that prevents the editor
#' from opening when \code{file.edit} is called.
#' Similarly, in RStudio on macOS, there is an issue opening files where they
#' are opened in an overlayed window rather than a new tab.
#' \code{file.edit} does work if the user types it at the command prompt.
#' A message with the correct lines to copy and paste is provided.
#'
#' @export
#' @docType methods
#' @rdname newModule
# @importFrom utils file.edit
#' @author Alex Chubaty and Eliot McIntire
#'
#' @family module creation helpers
#'
#' @examples
#' \dontrun{
#'   ## create a "myModule" module in the "modules" subdirectory.
#'   newModule("myModule", "modules")
#'
#'   ## create a new parent module in the "modules" subdirectory.
#'   newModule("myParentModule", "modules", type = "parent", children = c("child1", "child2"))
#' }
#'
setGeneric("newModule", function(name, path, ...) {
  standardGeneric("newModule")
})

#' @export
#' @rdname newModule
#' @importFrom reproducible checkPath
setMethod(
  "newModule",
  signature = c(name = "character", path = "character"),
  definition = function(name, path, ...) {
    args <- list(...)

    stopifnot((names(args) %in% c('open', 'unitTests', 'type', "children")))

    open <- args$open
    unitTests <- args$unitTests
    type <- args$type
    children <- args$children

    # define defaults for ... args
    if (is.null(open)) open <- TRUE
    if (is.null(unitTests)) unitTests <- TRUE
    if (is.null(type)) type <- "child"
    if (is.null(children)) children <- NA_character_

    stopifnot(
      is(open, "logical"),
      is(unitTests, "logical"),
      is(type, "character"),
      is(children, "character"),
      type %in% c("child", "parent")
    )

    path <- checkPath(path, create = TRUE)
    nestedPath <- file.path(path, name) %>% checkPath(create = TRUE)
    dataPath <- file.path(nestedPath, "data") %>% checkPath(create = TRUE)
    RPath <- file.path(nestedPath, "R") %>% checkPath(create = TRUE)

    # empty data checksum file
    cat("", file = file.path(dataPath, "CHECKSUMS.txt"))

    # module code file
    newModuleCode(name = name, path = path, open = open, type = type, children = children)

    if (type == "child" && unitTests) {
      newModuleTests(name = name, path = path, open = open)
    }

    ### Make R Markdown file for module documentation
    newModuleDocumentation(name = name, path = path, open = open, type = type, children = children)
})

#' @export
#' @rdname newModule
setMethod(
  "newModule",
  signature = c(name = "character", path = "missing"),
  definition = function(name, ...) {
    newModule(name = name, path = getOption("spades.modulePath"), ...)
})

################################################################################
#' Create new module code file
#'
#' @param name  Character string specifying the name of the new module.
#'
#' @param path  Character string. Subdirectory in which to place the new module code file.
#'              The default is the current working directory.
#'
#' @param open  Logical. Should the new module file be opened after creation?
#'              Default \code{TRUE}.
#'
#' @param type  Character string specifying one of \code{"child"} (default),
#'              or \code{"parent"}.
#'
#' @param children   Required when \code{type = "parent"}. A character vector
#'                   specifying the names of child modules.
#'
#' @export
#' @docType methods
#' @rdname newModuleCode
# @importFrom utils capture.output file.edit
#' @author Eliot McIntire and Alex Chubaty
#'
setGeneric("newModuleCode", function(name, path, open, type, children) {
  standardGeneric("newModuleCode")
})

#' @export
#' @rdname newModuleCode
#' @family module creation helpers
#' @importFrom reproducible checkPath
#'
setMethod(
  "newModuleCode",
  signature = c(name = "character", path = "character", open = "logical",
                type = "character", children = "character"),
  definition = function(name, path, open, type, children) {
    stopifnot(type %in% c("child", "parent"))

    path <- checkPath(path, create = TRUE)
    nestedPath <- file.path(path, name) %>% checkPath(create = TRUE)
    filenameR <- file.path(nestedPath, paste0(name, ".R"))

    children_char <- if (is.na(children) || length(children) == 0L) {
      "character(0)"
    } else {
      capture.output(dput(children))
    }

    cat("
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = \"", name, "\",
  description = \"insert module description here\",
  keywords = c(\"insert key words here\"),
  authors = ", getOption("devtools.desc.author",
                         "c(person(c(\"First\", \"Middle\"), \"Last\", email = \"email@example.com\", role = c(\"aut\", \"cre\")))"), ",
  childModules = ", children_char, ",
  version = list(SpaDES = \"", as.character(packageVersion("SpaDES")), "\", ", name, " = \"0.0.1\"",
        if (type == "parent") paste0(", ", children, " = \"0.0.1\""),
        "),
  ",
  if (type == "child") "spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  ",
  "timeunit = \"year\"," ,"
  citation = list(\"citation.bib\"),
  documentation = list(\"README.txt\", \"", name, ".Rmd\")",
  if (type == "child") ",
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter(\"paramName\", \"paramClass\", value, min, max, \"parameter description\"),
    defineParameter(\".plotInitialTime\", \"numeric\", NA, NA, NA, \"This describes the simulation time at which the first plot event should occur\"),
    defineParameter(\".plotInterval\", \"numeric\", NA, NA, NA, \"This describes the simulation time interval between plot events\"),
    defineParameter(\".saveInitialTime\", \"numeric\", NA, NA, NA, \"This describes the simulation time at which the first save event should occur\"),
    defineParameter(\".saveInterval\", \"numeric\", NA, NA, NA, \"This describes the simulation time interval between save events\"),
    defineParameter(\".useCache\", \"numeric\", FALSE, NA, NA, \"Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant\")
  ),
  inputObjects = bind_rows(
    #expectsInput(\"objectName\", \"objectClass\", \"input object description\", sourceURL, ...),
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput(\"objectName\", \"objectClass\", \"output object description\", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )","
))\n",
      file = filenameR, fill = FALSE, sep = "")

    if (type == "child") {
      cat("
## event types
#   - type `init` is required for initialiazation

doEvent.", name, " = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- sim$", name, "Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, \"", name, "\", \"plot\")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, \"", name, "\", \"save\")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      #Plot(objectFromModule) # uncomment this, replace with object to plot
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, \"", name, "\", \"plot\")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, \"", name, "\", \"save\")

      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, \"", name, "\", \"templateEvent\")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, \"", name, "\", \"templateEvent\")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste(\"Undefined event type: \'\", current(sim)[1, \"eventType\", with = FALSE],
                  \"\' in module \'\", current(sim)[1, \"moduleName\", with = FALSE], \"\'\", sep = \"\"))
  )
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
", name, "Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event2Test1 <- \" this is test for event 2. \" # for dummy unit test
  sim$event2Test2 <- 777  # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects = function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData(\"LCC2005\", modulePath(sim)).
  # Nothing should be created here that does not create an named object in inputObjects.
  # Any other initiation procedures should be put in \"init\" eventType of the doEvent function.
  # Note: the module developer can use 'sim$.userSuppliedObjNames' in their function below to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call. e.g.,
  # if (!('defaultColor' %in% sim$.userSuppliedObjNames)) {
  #  sim$defaultColor <- 'red'
  # }
  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above\n",
        file = filenameR, fill = FALSE, sep = "", append = TRUE)
    }

    if (open) openModules(name, nestedPath)
})

################################################################################
#' Create new module documentation
#'
#' @inheritParams newModuleCode
#'
#' @export
#' @docType methods
#' @rdname newModuleDocumentation
# @importFrom utils file.edit
#'
#' @author Eliot McIntire and Alex Chubaty
#' @family module creation helpers
#'
setGeneric("newModuleDocumentation", function(name, path, open, type, children) {
  standardGeneric("newModuleDocumentation")
})

#' @export
#' @rdname newModuleDocumentation
#' @importFrom reproducible checkPath
setMethod(
  "newModuleDocumentation",
  signature = c(name = "character", path = "character", open = "logical",
                type = "character", children = "character"),
  definition = function(name, path, open, type, children) {
    path <- checkPath(path, create = TRUE)
    nestedPath <- file.path(path, name) %>% checkPath(create = TRUE)
    filenameRmd <- file.path(nestedPath, paste0(name, ".Rmd"))
    filenameCitation <- file.path(nestedPath, "citation.bib")
    filenameLICENSE <- file.path(nestedPath, "LICENSE")
    filenameREADME <- file.path(nestedPath, "README.txt")

    ### Make R Markdown file for module documentation
    cat(
"---
title: \"", name, "\"
author: \"", Sys.getenv('USER'), "\"
date: \"", format(Sys.Date(), "%d %B %Y"), "\"
output: pdf_document
---

# Overview

Provide an overview of what the module does / how to use the module.

Module documentation should be written so that others can use your module.
This is a template for module documentation, and should be changed to reflect your module.

## R Markdown

R Markdown syntax allows R code, outputs, and figures to be rendered in the documentation.

For help writing in R Markdown, see http://rmarkdown.rstudio.com/.

# Usage

```{r module_usage}
library(igraph)
library(SpaDES)

moduleDir <- file.path(\"", path, "\")
inputDir <- file.path(moduleDir, \"inputs\") %>% reproducible::checkPath(create = TRUE)
outputDir <- file.path(moduleDir, \"outputs\")
cacheDir <- file.path(outputDir, \"cache\")
times <- list(start = 0, end = 10)
parameters <- list(
  #.progress = list(type = \"text\", interval = 1), # for a progress bar
  ## If there are further modules, each can have its own set of parameters:
  #module1 = list(param1 = value1, param2 = value2),
  #module2 = list(param1 = value1, param2 = value2)
)
modules <- list(\"", name, "\")
objects <- list()
paths <- list(
  cachePath = cacheDir,
  modulePath = moduleDir,
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
      # use tryCatch: RStudio bug causes file open to fail on Windows (#209)
      openModules(basename(filenameRmd), nestedPath)

      # tryCatch(file.edit(filenameRmd), error = function(e) {
      #   warning("A bug in RStudio for Windows prevented the opening of the file:\n",
      #           filenameRmd, "\nPlease open it manually.")
      # })
    }

    return(invisible(NULL))
})

#' @export
#' @rdname newModuleDocumentation
setMethod("newModuleDocumentation",
          signature = c(name = "character", path = "missing", open = "logical"),
          definition = function(name, open) {
            newModuleDocumentation(name = name, path = ".", open = open)
})

#' @export
#' @rdname newModuleDocumentation
setMethod("newModuleDocumentation",
          signature = c(name = "character", path = "character", open = "missing"),
          definition = function(name, path) {
            newModuleDocumentation(name = name, path = path, open = TRUE)
})

#' @export
#' @rdname newModuleDocumentation
setMethod("newModuleDocumentation",
          signature = c(name = "character", path = "missing", open = "missing"),
          definition = function(name) {
            newModuleDocumentation(name = name, path = ".", open = TRUE)
})

#' Create template testing structures for new modules
#'
#' @param name  Character string specifying the name of the new module.
#'
#' @param path  Character string. Subdirectory in which to place the new module code file.
#'              The default is the current working directory.
#'
#' @param open  Logical. Should the new module file be opened after creation?
#'              Default \code{TRUE}.
#'
#' @export
#' @docType methods
#' @rdname newModuleTests
# @importFrom utils file.edit
#'
#' @family module creation helpers
#'
#' @author Eliot McIntire and Alex Chubaty
#'
setGeneric("newModuleTests", function(name, path, open) {
  standardGeneric("newModuleTests")
})

#' @export
#' @rdname newModuleTests
#' @importFrom reproducible checkPath
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
#' from opening when \code{file.edit} is called. \code{file.edit} does work if the user
#' types it at the command prompt. A message with the correct lines to copy and paste
#' is provided.
#'
#' @export
#' @docType methods
#' @rdname openModules
#' @importFrom raster extension
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
#' @importFrom reproducible checkPath
setMethod("openModules",
          signature = c(name = "character", path = "character"),
          definition = function(name, path) {
            basedir <- checkPath(path, create = FALSE)
            fileExtension <- sub(extension(name), pattern = ".", replacement = "")
            if (length(unique(fileExtension)) > 1) stop("Can only open one file type at a time.")
            ncharFileExt <- unlist(lapply(fileExtension, nchar))
            origDir <- getwd()
            setwd(basedir)
            if (any(name == "all")) {
              Rfiles <- dir(pattern = "[\\.][rR]$", recursive = TRUE, full.names = TRUE)
            } else if (all(ncharFileExt > 0) & all(fileExtension != "R")) {
              Rfiles <- dir(pattern = name, recursive = TRUE, full.names = TRUE)
              Rfiles <- Rfiles[unlist(lapply(name, function(n) grep(pattern = n, Rfiles)))]
            } else {
              Rfiles <- dir(pattern = "[\\.][rR]$", recursive = TRUE, full.names = TRUE)
              Rfiles <- Rfiles[unlist(lapply(name, function(n) grep(pattern = n, Rfiles)))]
            }
            # remove tests
            hasTests <- grep(pattern = "tests", Rfiles)
            if (length(hasTests) > 0) Rfiles <- Rfiles[-hasTests]

            onlyModuleRFile <- unlist(lapply(file.path(name, name),
                                             function(n) grep(pattern = n, Rfiles)))
            if (length(onlyModuleRFile) > 0) Rfiles <- Rfiles[onlyModuleRFile]

            # Open Rmd file also
            RfileRmd <- dir(pattern = paste0(name, ".[rR]md$"), recursive = TRUE, full.names = TRUE)

            Rfiles <- c(Rfiles, RfileRmd)
            Rfiles <- Rfiles[grep(pattern = "[/\\\\]", Rfiles)]
            Rfiles <- Rfiles[sapply(strsplit(Rfiles,"[/\\\\\\.]"),
                                    function(x) any(duplicated(x)))]

            lapply(file.path(basedir, Rfiles), .fileEdit)
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

#' @export
#' @rdname openModules
setMethod("openModules",
          signature = c(name = "simList", path = "missing"),
          definition = function(name) {
            mods <- unlist(modules(name))
            openModules(name = mods, path = modulePath(name))
})

################################################################################
#' Create a copy of an existing module
#'
#' @param from  The name of the module to copy.
#'
#' @param to    The name of the copy.
#'
#' @param path  The path to a local module directory. Defaults to the path set by
#'              the \code{spades.modulePath} option. See \code{\link{setPaths}}.
#'
#' @param ...   Additional arguments to \code{file.copy}, e.g., \code{overwrite = TRUE}.
#'
#' @return Invisible logical indicating success (\code{TRUE}) or failure (\code{FALSE}).
#'
#' @author Alex Chubaty
#' @export
#' @docType methods
#' @rdname copyModule
#'
#' @examples
#' \dontrun{copyModule(from, to)}
#'
setGeneric("copyModule", function(from, to, path, ...) {
  standardGeneric("copyModule")
})

#' @export
#' @rdname copyModule
setMethod("copyModule",
          signature = c(from = "character", to = "character", path = "character"),
          definition = function(from, to, path, ...) {
            if (!dir.exists(to)) {
              dir.create(file.path(path, to))
              dir.create(file.path(path, to, "data"))
              dir.create(file.path(path, to, "tests"))
              dir.create(file.path(path, to, "tests", "testthat"))
            }

            files <- dir(file.path(path, from), full.names = TRUE, recursive = TRUE)

            ## files in base dir
            ids <- which(basename(dirname(files)) == from)
            result <- file.copy(from = files[ids],
                                to = file.path(path, to), ...)
            result <- c(result, file.rename(from = file.path(path, to, paste0(from, ".R")),
                                            to = file.path(path, to, paste0(to, ".R"))))
            result <- c(result, file.rename(from = file.path(path, to, paste0(from, ".Rmd")),
                                            to = file.path(path, to, paste0(to, ".Rmd"))))
            if (file.exists(file.path(path, to, paste0(from, ".pdf")))) {
              result <- c(result, file.rename(from = file.path(path, to, paste0(from, ".pdf")),
                                              to = file.path(path, to, paste0(to, ".pdf"))))
            }

            ## files in "data" dir
            ids <- which(basename(dirname(files)) == "data")
            if (length(ids) > 0) {
              result <- c(result, file.copy(from = files[ids],
                                to = file.path(path, to, "data"), ...))
            }

            ## files in "tests" dir
            ids <- which(basename(dirname(files)) == "test")
            if (length(ids) > 0) {
              result <- c(result, file.copy(from = files[ids],
                                            to = file.path(path, to, "tests"), ...))
            }

            ## files in "testthat" subdir
            ids <- which(basename(dirname(files)) == "testthat")
            if (length(ids) > 0) {
              result <- c(result, file.copy(from = files[ids],
                                            to = file.path(path, to, "tests", "testthat"), ...))
            }

            if (!all(result)) warning("some module files could not be copied.")

            return(invisible(all(result)))
})

#' @export
#' @rdname copyModule
setMethod("copyModule",
          signature = c(from = "character", to = "character", path = "missing"),
          definition = function(from, to, ...) {
            copyModule(from, to, path = getOption('spades.modulePath'), ...)
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
#' @param data    Logical. If \code{TRUE}, then the data subdirectory will be included in the zip.
#'                Default is \code{FALSE}.
#' @param ...     Additional arguments to \code{\link{zip}}:
#'                e.g., add \code{"-q"} using \code{flags="-q -r9X"}
#'                (the default flags are \code{"-r9X"}).
#'
#' @author Eliot McIntire and Alex Chubaty
#'
#' @export
#' @rdname zipModule
#'
setGeneric("zipModule", function(name, path, version, data=FALSE, ...) {
  standardGeneric("zipModule")
})

#' @export
# @importFrom utils zip
#' @rdname zipModule
#' @importFrom reproducible checkPath
setMethod(
  "zipModule",
  signature = c(name = "character", path = "character", version = "character"),
  definition = function(name, path, version, data, ...) {
    path <- checkPath(path, create = FALSE)
    callingWd <- getwd()
    on.exit(setwd(callingWd), add = TRUE)
    setwd(path)
    zipFileName = paste0(name, "_", version, ".zip")
    print(paste("Zipping module into zip file:", zipFileName))

    allFiles <- dir(path = file.path(name), recursive = TRUE, full.names = TRUE)
    allFiles <- grep(paste0(name, "_+.+.zip"), allFiles, value = TRUE, invert = TRUE) # moduleName_....zip only
    if (!data)
      allFiles <- grep(file.path(name, "data"),  allFiles, invert = TRUE, value = TRUE)

    zip(zipFileName, files = allFiles, ...)#, extras = c("-x"), ...)
    file.copy(zipFileName, to = paste0(name, "/", zipFileName), overwrite = TRUE)
    file.remove(zipFileName)
})

#' @rdname zipModule
#' @export
setMethod("zipModule",
          signature = c(name = "character", path = "missing", version = "character"),
          definition = function(name, version, data, ...) {
            zipModule(name = name, path = ".", version = version, data = data, ...)
})

#' @export
#' @rdname zipModule
setMethod("zipModule",
          signature = c(name = "character", path = "missing", version = "missing"),
          definition = function(name, data, ...) {
            vers <- moduleVersion(name, path) %>% as.character()
            zipModule(name = name, path = ".", version = vers, data = data, ...)
})

#' @export
#' @rdname zipModule
setMethod("zipModule",
          signature = c(name = "character", path = "character", version = "missing"),
          definition = function(name, path, data, ...) {
            vers <- vers <- moduleVersion(name, path) %>% as.character()
            zipModule(name = name, path = path, version = vers, data = data, ...)
})
