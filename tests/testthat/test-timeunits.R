test_that("timeunit works correctly", {
  times <- list(start = 0.0, end = 10)
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
    caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA),
    fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
  )
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES"))

  mySim <- simInit(times, params, modules, objects = list(), paths = paths)

  expect_equal(maxTimeunit(sim = mySim), "year")

  x1 <- list(
    name = "testModule",
    description = "this is a test.",
    keywords = c("test"),
    authors = c(person(c("Alex", "M"), "Chubaty",
                       email = "alexander.chubaty@canada.ca",
                       role = c("aut", "cre"))),
    version = numeric_version("0.0.1"),
    spatialExtent = raster::extent(rep(NA_real_, 4)),
    timeframe = as.POSIXlt(c(NA, NA)),
    timeunit = NA_character_,
    citation = list(),
    reqdPkgs = list("grid", "raster", "sp"),
    parameters = rbind(
      defineParameter("dummyVal", "numeric", 1.0, NA, NA, "vague description")
    ),
    inputObjects = data.frame(objectName = "testInput", objectClass = "list",
                              sourceURL = "", other = NA_character_,
                              stringsAsFactors = FALSE),
    outputObjects = data.frame(objectName = "testOutput", objectClass = "list",
                               other = NA_character_, stringsAsFactors = FALSE)
  )

  # Test for numerics, or character strings that are not recognized
  expect_message(timeunit(mySim) <- 1, "^unknown timeunit provided:")
  expect_message(timeunit(mySim) <- "LeapYear", "^unknown timeunit provided:")

  # Test for user defined timeunits, in .GlobalEnv
  expect_message(timeunit(mySim) <- "fortNight", "^unknown timeunit provided:")
  assign("dfortNight", function(x) lubridate::duration(dday(14)),
         envir=.GlobalEnv)
  expect_match(timeunit(mySim) <- "fortNight", "")
  expect_match(timeunit(mySim), "fortNight")
  expect_equivalent(as.numeric(dfortNight(1)), 1209600)
  rm(dfortNight, envir=.GlobalEnv)

  # check for new unit being put into simList
  assign("dfortNight", function(x) lubridate::duration(dday(14)),
         envir=envir(mySim))
  expect_match(timeunit(mySim) <- "fortNight", "")
  expect_match(timeunit(mySim), "fortNight")
  expect_equivalent(as.numeric(mySim$dfortNight(1)), 1209600)
  rm(dfortNight, envir=envir(mySim))

  # test that NA_real_ gets coerced to NA_character_
  timeunit(mySim) <- NA_real_
  expect_identical(timeunit(mySim), NA_character_)

  # check that the minTimeunit captures one of the timestepUnits in the loaded modules
  expect_true(
    any(match(minTimeunit(mySim),
              sapply(depends(mySim)@dependencies, function(x) {
                x@timeunit
              })
             )
       )
  )

  # check that minTimeunit finds the smallest timeunit of the modules loaded
  whNotNA <- sapply(depends(mySim)@dependencies,
                    function(x) !is.na(x@timeunit))
  expect_equivalent(as.numeric(eval(parse(
    text=paste0("d", minTimeunit(mySim), "(1)")))),
    min(sapply(depends(mySim)@dependencies[whNotNA],
               function(x) {
                 eval(parse(text = paste0("d", x@timeunit,"(1)")))
               }
    )))
  expect_equal(as.numeric(inSeconds(NA_character_)), 0)
  expect_equal(as.numeric(inSeconds(NULL)), 0)

  exampleTime <- 1:10
  attributes(exampleTime)$unit <- NA_character_
  expect_equal(as.numeric(convertTimeunit(exampleTime, "seconds")), 0)

  exampleTime <- 1:10
  attributes(exampleTime)$unit <- "hour"
  expect_equal(as.numeric(convertTimeunit(exampleTime)), 1:10*3600)

  mySim <- simInit()
  expect_equal(maxTimeunit(mySim), NA_character_)

  expect_equal(c("year", "month", "week", "day", "hour", "second"),
                 spadesTimes())

  expect_equal(as.numeric(dNA()), 0)
  expect_equal(as.numeric(dhour(1)), 60*60)
  expect_equal(as.numeric(dday(1)), 60*60*24)
  expect_equal(as.numeric(dweeks(1)), 60*60*24*365.25/52)

  expect_equal(as.numeric(dweek(1)), 60*60*24*365.25/52)
})


test_that("timeunits with child and parent modules work correctly", {
  library(igraph)
  tmpdir <- file.path(tempdir(), "test_timeunits") %>% checkPath(create = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  })

  newModule("grandpar1", ".", type = "parent", children = c("child1", "child2", "par1"))
  newModule("par1", ".", type = "parent", children = c("child4", "child3"))
  newModule("child1", ".")
  newModule("child3", ".")
  newModule("child4", ".")
  newModule("child5", ".")

  newModule("child2", ".")
  fileName <- 'child2/child2.R'
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = "day"')
  cat(xxx1, file = fileName, sep = "\n")

  fileName <- 'child3/child3.R'
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = "week"')
  cat(xxx1, file = fileName, sep = "\n")

  fileName <- 'child5/child5.R'
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = "second"')
  cat(xxx1, file = fileName, sep = "\n")

  fileName <- 'par1/par1.R'
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = "month"')
  cat(xxx1, file = fileName, sep = "\n")

  mySim <- simInit(modules = list("grandpar1","par1"))
  expect_equal(timeunit(mySim), "month")

  # If only listing the one module and it is a parent, then use it regardless of whether
  #  it is shortest or longest
  mySim <- simInit(modules = list("grandpar1"))
  expect_equal(timeunit(mySim), "year")

  fileName <- 'grandpar1/grandpar1.R'
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = "hour"')
  cat(xxx1, file = fileName, sep = "\n")

  # If only listing the one module and it is a parent, then use it regardless of whether
  #  it is shortest or longest
  mySim <- simInit(modules = list("grandpar1"))
  expect_equal(timeunit(mySim), "hour")

  mySim <- simInit(modules = list("grandpar1", "child5"))
  expect_equal(timeunit(mySim), "second")

  newModule("grandpar1", ".", type = "parent", children = c("child1", "child2", "par1"))
  fileName <- 'grandpar1/grandpar1.R'
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = NA')
  cat(xxx1, file = fileName, sep = "\n")

  # If parent has NA for timeunit, then take smallest of children
  mySim <- simInit(modules = list("grandpar1"))
  expect_equal(timeunit(mySim), "day")

  newModule("grandpar1", ".", type = "parent", children = c("child1", "child2", "par1"))
  fileName <- 'grandpar1/grandpar1.R'
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = NA')
  cat(xxx1, file = fileName, sep = "\n")

  newModule("child2", ".")
  fileName <- 'child2/child2.R'
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = NA')
  cat(xxx1, file = fileName, sep = "\n")

  # If parent has NA for timeunit, then take smallest of children
  mySim <- simInit(modules = list("grandpar1"))
  expect_equal(timeunit(mySim), "month") # because par1 is month, grandpar1 is NA

})
