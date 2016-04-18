test_that("saving files does not work correctly", {
  savePath <- file.path(tempdir(), "test_save")
  on.exit(unlink(savePath, recursive = TRUE))

  times <- list(start = 0, end = 6, "month")
  parameters <- list(
    .globals = list(stackName = "landscape"),
    caribouMovement = list(
      .plotInitialTime = NA, torus = TRUE, .saveObjects = "caribou",
      .saveInitialTime = 1, .saveInterval = 1
    ),
    randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20))

  outputs <- data.frame(
    expand.grid(objectName = c("caribou","landscape"),
                saveTime = 1:2,
                stringsAsFactors = FALSE)
  )

  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(
    modulePath = system.file("sampleModules", package = "SpaDES"),
    outputPath = savePath
  )
  mySim <- simInit(times = times, params = parameters, modules = modules,
                   paths = paths, outputs = outputs)

  mySim <- spades(mySim)

  # test spades-level mechanism
  expect_true(file.exists(file.path(savePath,"caribou_month1.rds")))
  expect_true(file.exists(file.path(savePath,"landscape_month2.rds")))
  expect_false(file.exists(file.path(savePath,"landscape_month3.rds")))

  # test module-level mechanism
  expect_true(file.exists(file.path(savePath,"caribou_month3.rds")))
  expect_true(file.exists(file.path(savePath,"caribou_month5.rds")))

  outputs <- data.frame(
    expand.grid(objectName = c("caribou", "landscape")),
    stringsAsFactors = FALSE
  )
  times <- list(start = 0, end = 7, "month")
  parameters <- list(
    .globals = list(stackName = "landscape"),
    caribouMovement = list(.plotInitialTime = NA),
    randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20)
  )
  mySim <- simInit(times = times, params = parameters, modules = modules,
                   paths = paths, outputs = outputs)

  mySim <- spades(mySim)

  # test that if no save times are stated, then it is at end time
  expect_true(file.exists(file.path(savePath,"caribou_month7.rds")))
  expect_true(file.exists(file.path(savePath,"landscape_month7.rds")))
  rm(mySim)
})


test_that("saving csv files does not work correctly", {
   savePath <- file.path(tempdir(), "test_save")
   on.exit(unlink(savePath, recursive = TRUE))

   tempObj <- 1:10
   tempObj2 <- paste("val",1:10)
   df1 <- data.frame(col1 = tempObj, col2 = tempObj2)
   sim <- simInit(objects=c("tempObj", "tempObj2", "df1"),
     paths=list(outputPath=savePath))
   outputs(sim) = data.frame(
        objectName = c(rep("tempObj",2), rep("tempObj2", 3), "df1"),
        saveTime = c(c(1,4), c(2,6,7), end(sim)),
        fun = c(rep("saveRDS", 5), "write.csv"),
        package = c(rep("base", 5), "utils"),
        stringsAsFactors = FALSE)
   # since write.csv has a default of adding a column, x, with rownames, must add additional
   #   argument for 6th row in data.frame (corresponding to the write.csv function)
   sim2 <- copy(sim)
   outputArgs(sim2)[[6]] <- list(row.names=FALSE)
   sim2 <- spades(sim2)
   outputs(sim2)

   # read one back in just to test it all worked as planned
   newObj <- read.csv(dir(savePath, pattern="second10.csv", full.name=TRUE))
   expect_true(identical(df1, newObj))

   # Confirm that arguments are actually being passed in by changing row.names to TRUE
   sim2 <- copy(sim)
   outputArgs(sim2)[[6]] <- list(row.names=TRUE)
   sim2 <- spades(sim2)
   outputs(sim2)
   # read one back in just to test it all worked as planned
   newObj <- read.csv(dir(savePath, pattern="second10.csv", full.name=TRUE))
   expect_false(identical(df1, newObj))

})

