################################################################################
#' Extract checksum value
#'
#' The function hList extract checksum value from a list of file and return the value its respective filename as an R
#' object. The function is for now using the algorithm 'xxhash64'.Checksum is compiled using the digest function.
#'
#' @param flist A vector listing object from which we want to extract chekcsum value.
#' @param dir A character string representing path to file.
#' @param quick A logical argument. If TRUE, checksum is compiled using the combination of the filename and its size.
#'        If FALSE, cheksum is compiled using the object. Default is FALSE.
#' @param csalgorithm A character string representing the algorithms used in the digest function. Default is "xxhash64".
#'
#' @return The hList function returns a dataframe where filename, checksum value from the object (checksumFile), checksum
#'         value from the combination of filename and file size and the algorithm used to compute checksum values are stored.
#'
#' @importFrom digest digest
#'
#' @export
#' @docType methods
#'
#' @author Melina Houle
#' @examples
#' file.list<- list.files(tempdir())
#' hfile <-hList(file.list, dir = tempdir(), quick = TRUE)
hList <-function(flist, dir, quick = TRUE, csalgorithm = "xxhash64"){
  fileNames <- unlist(lapply(flist, function(x){file.path(dir, x)}))
  fileNames<- fileNames[!file.info(fileNames)$isdir]
  flist <- fileNames[file.exists(fileNames), drop = FALSE]

  if (length(flist)==0){
    hdata<- data.frame(Filename=character(),  checksumFile=character(),
                       checksumSize=character(), algorithm=character(), stringsAsFactors=FALSE)
  } else {
    # Extract filenames and hash value from files downloaded locally.
    if (!quick){
      # checksum on file
      htag <- lapply(basename(flist), function(i) digest(file =file.path(dir, i), algo = csalgorithm))

      # checksum on folder using folder name and size
      flst <- list.files(dir)
      flst <- file.path(dir,flst)
      allFiles <- lapply(flst, function(x) {list(basename(x), file.info(x)[,"size"])})

      hfolder <- digest::digest(allFiles, algo = csalgorithm)
      checkfolder<-data.frame(Filename=basename(dir), checksumFile= NA,
                              checksumSize=unlist(hfolder), algorithm = csalgorithm, stringsAsFactors=FALSE)

      # checksum on files using filename and size
      fls<- subset(allFiles, unlist(allFiles)[ c(TRUE,FALSE) ] %in% basename(flist))
      hfile <- lapply(fls, function(i) digest::digest(i, algo= csalgorithm))

      # Fill hdata data.frame
      checkfiles<-data.frame(Filename=basename(flist), checksumFile= unlist(htag),
                             checksumSize=unlist(hfile), algorithm = csalgorithm, stringsAsFactors=FALSE)
      hdata <- rbind(checkfiles, checkfolder)
    } else{
      # checksum on folder using folder name and size
      flst <- list.files(dir)
      flst <- file.path(dir,flst)
      allFiles <- lapply(flst, function(x) {list(basename(x), file.info(x)[,"size"])})

      hfolder <- digest::digest(allFiles, algo= csalgorithm)
      checkfolder<-data.frame(Filename=basename(dir), checksumFile= NA,
                              checksumSize=unlist(hfolder), algorithm = csalgorithm, stringsAsFactors=FALSE)
      # checksum on files using ilename and size
      fls<- subset(allFiles, unlist(allFiles)[ c(TRUE,FALSE) ] %in% basename(flist))
      hfile <- lapply(fls, function(i) {digest::digest(i, algo= csalgorithm)})

      # Fill hdata data.frame
      checkfiles<-data.frame(Filename=basename(flist), checksumFile= NA,
                             checksumSize=unlist(hfile), algorithm = csalgorithm, stringsAsFactors=FALSE)
      hdata <- rbind(checkfiles, checkfolder)
    }
  }
  return(hdata)
}



