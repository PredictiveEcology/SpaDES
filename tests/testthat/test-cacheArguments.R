test_that("test-cacheArguments", {

  outdir<-tempdir()
  setwd(outdir)

  url <- "http://ftp.geogratis.gc.ca/pub/nrcan_rncan/archive/vector/cli_itc_50k/land_use/L040J03.zip"
  sim <- SpaDES::simInit(times = list(start = 2010.0, end = 2020.0, timeunit = "year"),
                         params = list(),
                         modules = list(),
                         paths = list(outputPath=outdir))

  # Make sur the file do not exists before testing
  if(file.exists(file.path(outdir, basename(url)))){
    file.remove(file.path(outdir, basename(url)))
  }
  expect_false(file.exists(file.path(outdir, basename(url))))

  # Cache download first run. File is downloaded. checksum is logged in backpack.
  mySim<-SpaDES::Cache(download.file, url = url,
                       destfile = file.path(outdir, basename(url)),
                       method = "auto",
                       quiet = TRUE,
                       mode = "wb",
                       cacheOK = TRUE,
                       objects = SpaDES::Copy(sim),
                       cacheRepo= outdir,
                       sideEffect = TRUE, makeCopy = FALSE, quick = TRUE)

  #check if download occured
  expect_true(file.exists(file.path(outdir, basename(url))))

  #Compare checksum from file with checksum stored in backpack
  urlfile_size <- list(basename(url), file.size(file.path(outdir, basename(url))))
  urlfile_chcksum <- digest::digest(urlfile_size, algo = "xxhash64")

  con = DBI::dbConnect(RSQLite::SQLite(), dbname= file.path(outdir,"backpack.db"))
  cachedchcksum <- DBI::dbReadTable(con, "tag")
  expect_true(paste0(file.path(basename(outdir), basename(url)), ":",urlfile_chcksum)
              %in% cachedchcksum$tag)
  DBI::dbDisconnect(con)

  # rerun download. Shouldn't run --ERROR IT RE-RUN
  mySim<-SpaDES::Cache(download.file, url = url,
                       destfile = file.path(outdir, basename(url)),
                       method = "auto",
                       quiet = TRUE,
                       mode = "wb",
                       cacheOK = TRUE,
                       objects = SpaDES::Copy(sim),
                       cacheRepo= outdir,
                       sideEffect = TRUE, makeCopy = FALSE, quick = TRUE)


  # Test MakeCopy = TRUE
  mySim<-SpaDES::Cache(download.file, url = url,
                       destfile = file.path(outdir, basename(url)),
                       method = "auto",
                       quiet = TRUE,
                       mode = "wb",
                       cacheOK = TRUE,
                       objects = SpaDES::Copy(sim),
                       cacheRepo= outdir,
                       sideEffect = TRUE, makeCopy = TRUE, quick = TRUE)

  # check if copy was created
  copyFolder <- file.path(outdir,"gallery")
  expect_true(file.exists(file.path(copyFolder, basename(url))))

  # Remove and check if file is brought back using the copy
  file.remove(file.path(outdir, basename(url)))
  expect_false(file.exists(file.path(outdir, basename(url))))
  mySim<-SpaDES::Cache(download.file, url = url,
                       destfile = file.path(outdir, basename(url)),
                       method = "auto",
                       quiet = TRUE,
                       mode = "wb",
                       cacheOK = TRUE,
                       objects = SpaDES::Copy(sim),
                       cacheRepo= outdir,
                       sideEffect = TRUE, makeCopy = TRUE, quick = TRUE)

  expect_true(file.exists(file.path(outdir, basename(url))))

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
  expect_that(file.exists(file.path(copyFolder, basename(url))), is_true())
})
