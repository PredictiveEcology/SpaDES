test_that("test-dwdData", {

  outdir<-"C:/temp"
  setwd(outdir)

  datasetName ="AHCCD_daily"
  dfile = c("ZMekis_Vincent_2011.pdf")
  dbHash = "dbHash.sqlite"
  algo = "xxhash64"
  sim <- SpaDES::simInit(times = list(start = 2010.0, end = 2020.0, timeunit = "year"),
                         params = list(),
                         modules = list(),
                         paths = list(outputPath=outdir))

  # Make sur the file do not exists before testing
  if(file.exists(file.path(outdir, datasetName, dfile))){
    file.remove(file.path(outdir, datasetName, dfile))
  }
  expect_that(!file.exists(file.path(outdir, datasetName, dfile)), is_true())

  # Run dwdData
  mySim<-SpaDES::Cache(cacheRepo= outdir, dwdData, urls,
                       datasetName = datasetName,
                       dfile = dfile,
                       outdir = outdir,
                       Copy(sim),
                       checkhash = TRUE,
                       dbHash = "dbHash.sqlite",
                       cascade= FALSE,
                       sideEffect = TRUE, makeCopy = TRUE, quick = TRUE)

  #check if download occured
  expect_that(file.exists(file.path(outdir, datasetName, dfile)), is_true())

  # check if copy was created
  copyFolder <- file.path(outdir,"gallery")
  expect_that(file.exists(file.path(copyFolder, datasetName, dfile)), is_true())

  # Remove and check if file is brought back using the copy
  file.remove(file.path(outdir, datasetName, dfile))
  expect_that(!file.exists(file.path(outdir, datasetName, dfile)), is_true())
  mySim<-SpaDES::Cache(cacheRepo= outdir, dwdData, urls,
                       datasetName = datasetName,
                       dfile = dfile,
                       outdir = outdir,
                       Copy(sim),
                       checkhash = TRUE,
                       dbHash = "dbHash.sqlite",
                       cascade= FALSE,
                       sideEffect = TRUE, makeCopy = TRUE, quick = FALSE)
  expect_that(file.exists(file.path(outdir, datasetName, dfile)), is_true())

  # Remove copy and check if copy is recover from original file
  file.remove(file.path(copyFolder, datasetName, dfile))
  expect_that(!file.exists(file.path(copyFolder, datasetName, dfile)), is_true())
  mySim<-SpaDES::Cache(cacheRepo= outdir, dwdData, urls,
                       datasetName = datasetName,
                       dfile = dfile,
                       outdir = outdir,
                       Copy(sim),
                       checkhash = TRUE,
                       dbHash = "dbHash.sqlite",
                       cascade= TRUE,
                       sideEffect = TRUE, makeCopy = TRUE, quick = FALSE)
  expect_that(file.exists(file.path(copyFolder, datasetName, dfile)), is_true())

  # Check if dbSQLite is created
  expect_that(file.exists(file.path(outdir, dbHash)), is_true())

  # check if file is logged in checksum db
  con = DBI::dbConnect(RSQLite::SQLite(), dbname=dbHash)
  hfile <-DBI::dbReadTable(con, "checksum")
  cachedFilename <- hfile[hfile$Filename == dfile,1]
  expect_that(isTRUE(cachedFilename==dfile), is_true())

  # Extract cached checksum
  cachedChecksum <- hfile[hfile$Filename == dfile,2]
  cachedFile <- paste0(cachedFilename,":",cachedChecksum)

  # Hash current download
  currentChecksum <- digest::digest(file = file.path(outdir, datasetName, dfile), algo = algo)
  currentFile <-paste0(dfile,":",currentChecksum)

  # Crosscheck checksum on file with cached value
  expect_that(isTRUE(currentFile==cachedFile), is_true())

})
