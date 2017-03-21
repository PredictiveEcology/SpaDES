##################################################################################
#' List available dataset using dataset name.
#'
#' Browse http and ftp site of interest using dataset name and store available dataset in a list. To list the data using
#' dataset name, a data.table containing datasetName, URL of interest and password need to be previously created.
#' @rdname webdataset
#' @param urltble A data.table that stores available dataset name, url, password and filenames found within each dataset.
#'        urltble is provided within the package as urls object.
#' @param datasetName A character string. Represent the dataset of interest for download. datasetName allow to derived
#'        url and password from the urltble.
#' @rd
#' @return List available data from a specific dataset
#'
#' @importFrom RCurl getURL url.exists
#' @importFrom data.table setkey
#' @export
#' @docType methods
#'
#' @author Melina Houle
#' @examples
#'
#' dt<- data.table(dataset = c("NFDB"),
#'                url = c("http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/"),
#'                password= c(NA))
#'
#' setwd(tempdir())
#' listWebData(dt, datasetName = "NFDB")
listWebData<-function(urltble,datasetName){

  if(missing(datasetName)) stop("You must provide dataset name to derive url of interest")

  setkey(urltble, "dataset")
  password<- as.character(urltble[.(datasetName)][, 'password', with=FALSE])
  path2data<- as.character(urltble[.(datasetName)][, 'url', with=FALSE])

    if (!is.na(password)) {path2data <- paste(unlist(strsplit(path2data, "//"))[1],"//", password, "@",
                                            unlist(strsplit(path2data, "//"))[2], sep="")

                        if (!url.exists(as.character(path2data))) stop("url and/or password is erroneous")

                        file.list <- getURL(path2data, userpwd = password, ftp.use.epsv = FALSE, dirlistonly = TRUE)
                        file.list <- strsplit(file.list, "\r\n")[[1]]
  } else {

    if (!url.exists(as.character(path2data))) stop("url is erroneous")

    typeConn <- paste(unlist(strsplit(path2data, "//"))[1],"//", sep ="")
    if (typeConn == "http://") {file.list <- readHTMLTable(path2data, skip.rows=1:2)[[1]]$Name
                                file.list<- as.character(file.list)
    }
    if (typeConn == "ftp://")  {file.list <- getURL(path2data, userpwd = NULL,
                                                   ftp.use.epsv = FALSE, dirlistonly = TRUE)
                                 file.list <- strsplit(file.list, "\r\n")[[1]]
    }
  }
  return(file.list)
}

##################################################################################
#' Table of dataset accessible using url
#'
#' An R object that stores dataset with their respective url and username/password. This table makes the link between
#' dataset name, url and username/password. By only poviding the dataset name, listWebdata and dwdData functions extract url
#' username/password to access, extract and download data.
#'
#' @rdname webdataset
#' @rd
#' @return Object containing dataset available for download.
#'
#' @importFrom data.table data.table
#' @export
#' @docType methods
#'
#' @author Melina Houle
#' @examples
#' dt<- data.table(dataset = c("NFDB"),
#'                url = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/",
#'                password= NA,
#'                files = "NFDB_poly.zip")
urls<- data.table(dataset = c("AHCCD_daily",
                              "NFDB",
                              "KNN",
                              "BIOSIM"),
                  url = c("ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/",
                          "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/",
                          "ftp://tree.nfis.org/",
                          "ftp.nofc.cfs.nrcan.gc.ca/uploads/MPB/"),
                  password= c(NA,
                              NA,
                              "knn4ftp:knn4ftp",
                              NA),
                  files= list(c("Adj_Daily_Rain_v2015.zip","Adj_Daily_Snow_v2015.zip","Adj_Daily_TotalP_v2015.zip",
                                "Adj_Precipitation_Documentation_v2015.doc","Adj_Precipitation_Stations_v2015.xls",
                                "Homog_daily_max_temp_v2015.zip","Homog_daily_mean_temp_v2015.zip",
                                "Homog_daily_min_temp_v2015.zip","Homog_temperature_documentation_v2015.doc",
                                "Homog_temperature_stations_v2015.xlsx",
                                "Vincent_and_coauthors_Trends_in _Canada's_Climate_JClimate_June2015.pdf",
                                "Vincent_et al_Second_Generation_Homog_Temp_JGR_2012.pdf",
                                "Wang_Feng_Vincent_Extreme_Temp_AO_2013.pdf",
                                "ZMekis_Vincent_2011.pdf"),
                              c("NFDB_poly.zip","NFDB_poly_20160712_large_fires_metadata.pdf",
                                "NFDB_poly_20160712_metadata.pdf","NFDB_poly_large_fires.zip"),
                              c("FR_NFI_and_kNN_Mapping_20160628.docx","NFI_and_kNN_Mapping_20160628.docx",
                                "cjfr-2013-0401suppl.pdf", "kNN-EcozonesDomain.zip","kNN-Genus.tar",
                                "kNN-LandCover.tar", "kNN-Soils.tar","kNN-Species.tar","kNN-SpeciesDominant.tar",
                                "kNN-SpeciesGroups.tar","kNN-StructureBiomass.tar","kNN-StructureStandVolume.tar"),
                              c("m", "n", "v")))
