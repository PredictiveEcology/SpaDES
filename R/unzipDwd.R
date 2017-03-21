################################################################################
#' Extract files from a zip archive downloaded
#'
#' This function unzip dataset previously downloaded from url. Prior to unzip, the function can check if unzip
#' was previously performed (unzip file exists locally).
#'
#' @param zipfile A character vector containing path to a zipfile.
#' @param dir A character string representing the output directory path.
#' @param env The environment to search for objects.
#' @param checkhash A logical argument. If TRUE, check if unzip file is found locally and cross-check its checksum value
#'        with value logged in dbHash from previous unzip event. When checksums match, a message raises indicating the file
#'        is already properly unzip. When checksums doesn't match or unzip file doesn't exist locally, unzip occurs and
#'        checksum compiles. If FALSE, unzip occurs even if the files exist locally. Default is FALSE.
#' @param dbHash A character string. The path to the database file where checksum values are logged. If the named
#'        database does not yet exist, one is created. Default is "dbHash.sqlite".
#' @param quick A logical argument. If TRUE, checksum is compiled using the combination of the filename and its size.
#'        If FALSE, cheksum is compiled using the object. Default is FALSE.
#'
#' @importFrom utils unzip
#'
#' @export
#' @docType methods
#'
#' @author Melina Houle
#' @examples
#' datasetName ="AHCCD_daily"
#' dfile = c("Homog_daily_mean_temp_v2015.zip")
#' algo = "xxhash64"
#' outdir <- outdir<-"C:/temp"
#' sim <- SpaDES::simInit(times = list(start = 2010.0, end = 2020.0, timeunit = "year"), params = list(),
#' modules = list(), paths = list(outputPath=outdir))
#' dwdData(urls, datasetName, dfile = dfile, outdir, sim, module, checkhash = FALSE, cascade = FALSE)
#' zip <- file.path(outdir, datasetName, dfile)
#' unzipDwd(zip, outdir, checkhash = FALSE, dbHash = "dbHash.sqlite", quick = FALSE)
unzipDwd <-function(zipfile, dir, env= env, checkhash = TRUE, dbHash = "dbHash.sqlite", quick=FALSE){
  if(!missing(env)) {checkhash <- env$checkhash
                    quick <- env$quick
                    dbHash <-env$dbHash
                    outdir <- env$outdir
  }

  subdir <- dirname(zipfile)
  fn<- file_path_sans_ext(basename(zipfile))

  if(checkhash){
    con = dbConnect(RSQLite::SQLite(), file.path(outdir,dbHash))
    hfile<-hfile <-dbReadTable(con, "checksum")


    if (dir.exists(file.path(dir, subdir, fn))){
      # Unzip folder already exists. Check if files exist.
      fileNames<-list.files(file.path(dir, subdir,fn))
      dataPath <- file.path(dir, subdir,fn)
      if(!length(fileNames)==0) {hashdata<-hList(basename(fileNames), dataPath, quick)
      # Files exist. Check if they were properly unzip using checksum.
        if (quick) {
          # List unzip files with missing checksum
          unlogged<- subset(hashdata, !(hashdata$Filename %in% hfile$Filename))
          # List unzip files with wrong checksum
          hfile <- subset(hfile, (hfile$Filename %in% hashdata$Filename))
          errorlog <-hfile[which(hfile$Filename == hashdata$Filename), 3]!=
            hashdata[which(hfile$Filename == hashdata$Filename), 3]
          # Need to unzip if any missing or wrong checksum
          needUnzip <- any(errorlog, na.rm = TRUE)|| nrow(unlogged)>0 || is.na(hfile)

        }else{
          # List unzip files with missing checksum
          unlogged<- subset(hashdata, !(hashdata$Filename %in% hfile$Filename))
          # List unzip files with wrong checksum
          hfile <- subset(hfile, (hfile$Filename %in% hashdata$Filename))
          errorlog <-hfile[which(hfile$Filename == hashdata$Filename), 2]!=
            hashdata[which(hfile$Filename == hashdata$Filename), 2]
          # Need to unzip if any missing or wrong checksum
          needUnzip <- any(errorlog, na.rm = TRUE)|| nrow(unlogged)>0 || is.na(hfile)
        }
      }else{
        # No file exists in unzip folder. Need to unzip.
        needUnzip <- TRUE
      }
    }else{
      #Unzip folder does not exist. Need to unzip.
      needUnzip <- TRUE
    }

    if(needUnzip){
      # Unzip
      message("Unzipping ", zipfile, "\n")
      unzip(zipfile, exdir=file.path(subdir,fn))
      # Log checksum
      fileNames<-list.files(file.path(subdir,fn))
      dataPath <- file.path(subdir,fn)
      checksum<-hList(fileNames, dataPath, quick)
      logChecksum(checksum, dbHash)
    }else{
      message(basename(zipfile), "is already unzipped \n")
    }
  }else{
    #Checkhash = FALSE. Unzip only.
    message("Unzipping ", zipfile, "\n")
    unzip(zipfile, exdir=file.path(subdir,fn))
  }
}
