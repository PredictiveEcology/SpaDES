##################################################################################
#' Log checksum from downloaded files into a SQLite database
#'
#' Log compiled checksum values from a specific file into a predetermine dbHash sqLite database following SQL language
#' requirements.
#'
#' @param hashdata A dataframe containing filename, checksumFile, checksumSize and algorithm for each file are stored.
#'
#' @param dbHash A character string indicating the name of the database where checksum values are stored. If the database
#'                  does not exists, the function will create it and checksum will be compiled. Default is "dbHash.sqlite".
#' @rd
#' @return
#'
#' @importFrom DBI  dbWriteTable  dbSendQuery dbDisconnect dbClearResult dbFetch
#' @export
#' @docType methods
#'
#' @author Melina Houle
#' @examples
#' setwd(tempdir())
#' dbHash <- "dbHash.sqlite"
#' file.list<- list.files()
#' hfile <-hList(file.list, dir = tempdir())
#' logChecksum(hfile,dbHash)
logChecksum<-function(hashdata,dbHashPath) {
  con = dbConnect(RSQLite::SQLite(), dbHashPath)
  if (!dbExistsTable(con, "checksum")){
    dbWriteTable(con, "checksum", data.frame(Filename= character(),checksumFile= character(),
                                             checksumSize= character(), algorithm = character(),
                                             stringsAsFactors=FALSE),
                 overwrite = TRUE, field.types = NULL)
  }
  w<-hashdata$Filename
  # Some file names use quote (') in there names.
  # To fill SQL language requirements, quote should be double ('').
  w<-gsub("'","''",w)
  # Build a string using filename from current download.
  t<-paste0("'",w, "'", collapse=",")
  # Delete duplicate rows from database table
  options(warn=-1)
  l<-dbSendQuery(con, paste0(" DELETE FROM checksum WHERE Filename IN (", t, ")"))
  dbFetch(l)
  dbClearResult(l)
  options(warn=0)
  dbWriteTable(con, "checksum", hashdata, append = TRUE)
  dbDisconnect(con)
}
