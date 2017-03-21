################################################################################
#' Extract files from a tar archive downloaded
#'
#' This function untar and unzip dataset previously downloaded from url. Prior to untar, the function can check if untar
#' was previously performed (untar/unzip files exist locally).
#'
#' @param tarfile A character vector containing path to tarfile.
#' @param dir A character string representing path to output directory.
#' @param env The environment to search for objects.
#' @param checkhash A logical argument. If TRUE, check if untar/unzip files are found locally and cross-check their checksum value
#'        with value logged in dbHash from previous untar/unzip event. When checksums match, a message raises indicating the files
#'        are already properly untar/unzip. When checksums don't match or untar/unzip files don't exist locally, untar/unzip occurs and
#'        checksum compiles. If FALSE, untar/unzip occurs even if the files exist locally. Default is FALSE.
#' @param dbHash A character string. The path to the database file where checksum values are logged If the named
#'        database does not yet exist, one is created. Default is "dbHash.sqlite".
#' @param quick A logical argument. If TRUE, checksum is compiled using the combination of the filename and its size.
#'        If FALSE, cheksum is compiled using the object. Default is FALSE.
#' @importFrom utils unzip untar
#'
#' @export
#' @docType methods
#'
#' @author Melina Houle
#' @examples
#' datasetName <-"KNN"
#' dfile <- c("kNN-LandCover.tar")
#' algo <- "xxhash64"
#' outdir <- "C:/temp"
#' sim <- SpaDES::simInit(times = list(start = 2010.0, end = 2020.0, timeunit = "year"), params = list(),
#' modules = list(), paths = list(outputPath=outdir))
#' dwdData(urls, datasetName, dfile = dfile, outdir, sim, module, checkhash = FALSE, cascade = FALSE)
#' tar <- file.path(outdir, datasetName, dfile)
#' untarDwd(tar, outdir, checkhash = FALSE, dbHash = "dbHash.sqlite", quick = FALSE)
untarDwd <-function(tarfile, dir, env= env, checkhash = FALSE, dbHash = "dbHash.sqlite", quick=FALSE){
  if(!missing(env)) {checkhash <- env$checkhash
                    quick <- env$quick
                    dbHash <-env$dbHash
                    outdir <- env$outdir
  }
  subdir <- dirname(tarfile)
  fx<- file_path_sans_ext(basename(tarfile))
  envi_tar <- sys.frame(which = 1)
  if(checkhash){
    # Crosscheck with previous download
    con = dbConnect(RSQLite::SQLite(), dbname=dbHash)
    hfile<-hfile <-dbReadTable(con, "checksum")

    if(dir.exists(file.path(subdir, fx))){
      # output directory exists
      fileNames<-list.files(file.path(subdir,fx))
      dataPath <- file.path(subdir,fx)

      if(!length(fileNames)==0) {
        hashdata<-hList(basename(fileNames), dataPath, quick)
      # Files exist. Check if they were properly untar using checksum.
        if(quick){
          # List untar files with missing checksum
          needUntar<- subset(hashdata, !(hashdata$Filename %in% hfile$Filename))
          # List untar files with wrong checksum
          hfile <- subset(hfile, (hfile$Filename %in% hashdata$Filename))
          errorlog <-hfile[which(hfile$Filename == hashdata$Filename), 3]!=
            hashdata[which(hfile$Filename == hashdata$Filename), 3]

          # Need to untar if any missing or wrong checksum
          needUntar <- any(errorlog, na.rm = TRUE)|| nrow(needUntar)>0 || is.na(hfile)
        }else{
          # List untar files with missing checksum
          needUntar<- subset(hashdata, !(hashdata$Filename %in% hfile$Filename))
          hfile <- subset(hfile, (hfile$Filename %in% hashdata$Filename))
          # List untar files with wrong checksum
          errorlog <-hfile[which(hfile$Filename == hashdata$Filename), 2]!=
            hashdata[which(hfile$Filename == hashdata$Filename), 2]

          # Need to untar if any missing or wrong checksum
          needUntar <- any(errorlog, na.rm = TRUE)|| nrow(needUntar)>0 || is.na(hfile)
        }
      }else{
      # No file exists in untar folder. Need to untar.
      needUntar <- TRUE
      }
    }else{
      #Untar folder does not exist. Need to untar and unzip.
      needUntar <- TRUE
    }

    if(needUntar){
      # Untar
      message("Untar ", basename(tarfile), "\n")
      untar(file.path(subdir,basename(tarfile)), exdir = file.path(subdir, fx))
      # Log checksum
      fileNames<-list.files(file.path(subdir,fx))
      dataPath <- file.path(subdir,fx)
      checksum<-hList(fileNames, dataPath, quick)
      logChecksum(checksum, dbHash)
      # Unzip
      file.list <- unlist(lapply(fileNames, function(x){file.path(dataPath, x)}))
      lapply(file.list, function(i) if (file_ext(i) == "zip") {
        unzipDwd(i, subdir, env)})
    }else{
      message(basename(tarfile), "is already untarred\n")
    }

  }else{
    #Checkhash = FALSE. Untar and unzip only. No cheksum.
    message("Untar ", basename(tarfile), "\n")
    untar(file.path(subdir,basename(tarfile)), exdir = file.path(subdir, fx))
    fileNames<-list.files(file.path(subdir,fx))
    dataPath <- file.path(subdir,fx)
    file.list <- unlist(lapply(fileNames, function(x){file.path(dataPath, x)}))

    lapply(file.list, function(i) if (file_ext(i) == "zip") {
      unzipDwd(i, dir, envi_tar)})
  }
}



