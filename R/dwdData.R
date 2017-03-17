################################################################################
#' Download, unzip, untar data from an url
#'
#' This function download and untar and/or unzip dataset using url link from available dataset. Prior to download, the function
#' can check if the files are already locally by using the checksum value available in dbHash.sqlite database.
#'
#' @param urltble A data.table that stores available dataset name, url, password and filenames found within each dataset.
#'        urltble is provided within the package as urls object.
#' @param datasetName A character string. Represent the dataset of interest for download. datasetName allow to derived
#'        url and password from the urltble.
#' @param dfile A vector, given as a character string. Represent filename of interest to download. Default value is
#'        to download everything using the argument "all".
#' @param outdir A character string. Indicate output directory where the files will be stored. Default will use the
#'               modulePath from the sim object, if supplied with a module name, or a temporary location based on the
#'               url of the file(s).
#' @param sim A simList simulation object, generally produced by simInit.
#' @param module A character string representing the names of the module to be loaded for the simulation.
#' @param checkhash A logical argument. If TRUE, check if file is found locally and cross-check its checksum value
#'        with value logged in dbHash from previous download. When checksums match, a message raises indicating the file
#'        is properly downloaded. No download occurs. When checksums don't match or file doesn't exist locally
#'        (first download), download occurs and checksum compiles. If FALSE, download occurs even if the file exists
#'        locally. Default is FALSE.
#' @param dbHash A character string. The path to the database file where checksum values are logged. If the named
#'        database does not yet exist, one is created. Default is "dbHash.sqlite".
#' @param cascade A logical argument. If TRUE, file is untar and/or unzip. Default is FALSE.
#' @param quick A logical argument. If TRUE, checksum is compiled using the combination of the filename and its size.
#'        If FALSE, cheksum is compiled using the object. Default is FALSE.
#'
#' @return Data will be stored in the subfolder using the basename of the url in a data folder.
#'
#' @importFrom RCurl getURL
#' @importFrom downloader download
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom XML readHTMLTable
#' @importFrom DBI dbConnect dbWriteTable dbReadTable dbExistsTable dbDisconnect
#' @importFrom dplyr src_sqlite
#' @importFrom data.table setkey
#'
#' @export
#' @docType methods
#'
#' @author Melina Houle
#' @examples
#' dsName <- "AHCCD_daily"
#' sim <- SpaDES::simInit(times = list(start=0.0, end=5.0),
#'                        objects=list(dt),
#'                        params = list(),
#'                        modules = list(),
#'                        paths = list(outputPath=outdir))
#' dt<- data.table(dataset = c("AHCCD_daily"),
#'                url = c("ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/"),
#'                password= c(NA),
#'                files= list(c("ZMekis_Vincent_2011.pdf", Adj_Daily_Rain_v2015.zip","Adj_Daily_Snow_v2015.zip")))
#' dwdData(dt, datasetName= dsName, dfile = "ZMekis_Vincent_2011.pdf", outdir = tempdir(), sim, module, checkhash = FALSE, cascade = FALSE)
dwdData <-function(urltble, datasetName, dfile = "all", outdir, sim, module, checkhash = FALSE,
                    dbHash = "dbHash.sqlite", cascade = FALSE, quick = FALSE){

  # Save environment to recover params in untarDwd and unzipDwd function
  env = sys.frame(sys.nframe())

  if(missing(urltble)) stop("You must provide a datatable that contain the link
                            between url, datasetName and password")
  if(missing(datasetName)) stop("You must provide dataset name to access url of interest")

  setkey(urltble, "dataset")
  url<- as.character(urltble[.(datasetName)][, 'url', with=FALSE])
  password<- as.character(urltble[.(datasetName)][, 'password', with=FALSE])

  if(missing(outdir)) {
    if(!missing(sim)) {
      if(missing(module)) stop("You must provide a module name, if you provide a simList")
      outdir <- file.path(modulePath(sim), module, "data")
    } else {
      outdir <- file.path(dirname(tempdir()),"data",file_path_sans_ext(basename(url)))
    }
  }

  # Create a output dir using datasetName
  subdir <- file.path(outdir, datasetName, sep="")
  dir.create(subdir, recursive = TRUE, showWarnings = FALSE)

  # Split url into typeConn(ftp, http) and address.
  typeConn <- paste(unlist(strsplit(url, "//"))[1],"//", sep ="")
  address<- unlist(strsplit(url, "//"))[2]

  # Create list of file to download
  # FTP connection
  if (typeConn == "ftp://"){
    if (isTRUE(dfile == "all")){
      # all files to download. No password for the ftp site
      if (is.na(password)) { file.list <- getURL(url, ftp.use.epsv = FALSE,
                                                 dirlistonly = TRUE)
                            file.list <- paste(url, strsplit(file.list,
                                                 "\r*\n")[[1]], sep = "")
      }else{
        # all files to download. Password needed for the ftp site
        file.list <- getURL(url, userpwd=password, ftp.use.epsv = FALSE, dirlistonly = TRUE)
        file.list <- paste(typeConn, password, "@", address,
                           strsplit(file.list, "\r*\n")[[1]], sep = "")
      }
    }else{
      # Specific files to download on the ftp site no password required
      if (is.na(password)) {file.list<-paste(url, dfile, sep="")
      }else{
        # Specific files to download on the ftp site, password is required
        file.list<-paste(typeConn, password, "@", address, dfile, sep="")
      }
    }
  }
  # HTTP connection
  if (typeConn == "http://"){
    if (isTRUE(dfile == "all")){
      file.list <- readHTMLTable(url, skip.rows=1:2)[[1]]$Name
      file.list <- paste(url, file.list[!is.na(file.list)], sep="")
    }else{
      file.list <- paste(url, dfile, sep="")
    }
  }
  # Crosscheck checksum value fromtable where checksum values from previous download are compiled.
  if(checkhash){
    # Connect to checksum db
    con = dbConnect(RSQLite::SQLite(), file.path(outdir,dbHash))

    # Create checksum table if it doesn't exist
    if (!dbExistsTable(con, "checksum")){
      dbWriteTable(con, "checksum", data.frame(Filename= character(),checksumFile= character(),
                                               checksumSize= character(), algorithm = character(),
                                               stringsAsFactors=FALSE),
                                               overwrite = TRUE, field.types = NULL)
      message("hashDwd.txt has been created. ")
    }

    hfile <-dbReadTable(con, "checksum")

    # Compile checksum on local file. Empty the first time the function is used.
    hashdata<-hList(basename(file.list), subdir, quick)

    # Crosscheck checksum from previous download with chekcsum from local files.
    # Matching results indicate a proper downloaded. When first download, all files will be identified as needDownload.
    if(quick){
      # Use checksum on filename and filesize
      dwdfile<-subset(hashdata, (hashdata$Filename %in% basename(file.list)) &
                        (hashdata$checksumSize %in% hfile$checksumSize))
    }else{
      # use checksum from file. Takes more time
      dwdfile<-subset(hashdata, (hashdata$Filename %in% basename(file.list))  &
                        (hashdata$checksumFile %in% hfile$checksumFile))
    }
    #initialize needDownload
    needDownload <- FALSE
    # Print files already properly downloaded
    if (nrow(dwdfile) !=0) cat(paste0(dwdfile$Filename, " is already downloaded\n"))

    # Files exist locally, but checksum is missing
    if (nrow(hashdata) > 0 & nrow(hfile) ==0){needDownload <- TRUE
                                              message("Files were found locally but checksum was missing.",
                                              "The script is proceeding to download and checksum will be logged")

    # Checksum was recorded but files don't exist locally
    }else if (nrow(hashdata) ==0 & nrow(hfile) >0) {needDownload <- TRUE
                                              message("Files were previously downloaded but were not found locally.",
                                              "The script is proceeding to download")

    # First time download
    }else if (nrow(hashdata) ==0 & nrow(hfile) ==0 ){needDownload <- TRUE
                                              message("Proceeding to download. checksum will be recorded")

    }else{
      # List file to download : files not found locally or checksum missing
      file.list <- subset(file.list, !(basename(file.list) %in% dwdfile$Filename))
      if(!is.null(nrow(file.list))) needDownload <- TRUE
    }
    #DOWNLOAD
    if(needDownload){
      # Download files and compile checksum values
      lapply(file.list, function(x) message(basename(x), " will be downloaded"))
      lapply(file.list, function(x) download(x, file.path(subdir, basename(x)), mode="wb"))
      # Logged checksum value
      hashdata<-hList(basename(file.list), subdir, quick)
      logChecksum(hashdata, file.path(outdir,dbHash))
    }else{
      message("All files were previously downloaded. No download will occur.")
    }

  }else{
    # checksum = FALSE -> Download data only
    lapply(file.list, function(x) download(x, file.path(subdir, basename(x)), mode="wb"))
  }
  file.list <- file.path(subdir,basename(file.list))

  if (cascade){
    # Cascade = TRUE will untar / unzip all files
    invisible(lapply(file.list,  function(x) if (file_ext(x) == "zip"){
      # Unzip
      unzipDwd(x, outdir, env)
    }))

    invisible(lapply(file.list, function(x) if (file_ext(x) == "tar") {
      # Untar and unzip
      untarDwd(x, outdir,env)
    }))
  }
  return(sim)
}
