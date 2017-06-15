# IF YOU ARE HAVING PROBLEMS INSTALLING SPADES
# How to install SpaDES with RStudio
# Basically SpaDES uses many of the packages that are part of Rstudio's tools
# That means it may be difficult to install SpaDES within RStudio
# If you are having problems installing SpaDES,
#   the safest thing to do is open a vanilla R session, as long as
#   .libPaths() is the same in the vanilla session and your Rstudio
#   then a standard "install.packages('SpaDES')" should work
# Alternatively, you can try the code below which effectively unloads
# all dependencies of SpaDES, including the ones that RStudio holds onto

installSpaDES <- function(autoRestart = FALSE) {

  loadN <- loadedNamespaces()
  ip <- installed.packages(lib.loc = .libPaths()[1])
  #if(!("SpaDES" %in% ip[,"Package"])) {
    SpaDESDeps <- c("SpaDES", #"archivist",
                    "broom", #"backports",
                    "checkmate",
                    "tidyr",
                    "chron", "CircStats", "data.table", #"nlme",
                    "DEoptim", "DiagrammeR", "digest", "dplyr", "DT", "fastmatch",
                    "ff", "ffbase", "fpCompare", "ggplot2", "gridBase", "httr", "igraph",
                    "lazyeval", "lubridate", "miniUI", "R.utils", "RandomFields",
                    "raster", "RColorBrewer", "rstudioapi", "shiny", "sp", "stringi",
                    "stringr", "RCurl", "DBI", "RSQLite", "magrittr", "MASS", "boot",
                    "htmlwidgets", "influenceR", "rgexf", "scales", "tibble", "viridis",
                    "visNetwork", "assertthat", "R6", "Rcpp", "BH", "htmltools",
                    "bit", "gtable", "plyr", "reshape2", "jsonlite", "mime", "curl",
                    "openssl", "Matrix", "NMF", "irlba", "R.oo", "R.methodsS3", "RandomFieldsUtils",
                    "httpuv", "xtable", "sourcetools", #"lattice",
                    "yaml", "pkgmaker",
                    "registry", "rngtools", "cluster", "colorspace", "foreach", "doParallel",
                    "bitops", "XML", "Rook", "memoise", "plogr", "dichromat", "munsell",
                    "labeling", "viridisLite", "gridExtra", "iterators", "codetools",
                    "brew")
  # } else {
  #
  # # Need miniCRAN to get ALL the dependencies of SpaDES and other packages
  #   if(!require("miniCRAN"))
  #     install.packages("miniCRAN")
  #   message("Determining SpaDES dependencies")
  #   SpaDESDeps <- miniCRAN::pkgDep("SpaDES")
  # }
  # determine all dependencies of packages in the search path
  pkgsOnly <- grep(search(), pattern="package:", value = TRUE)
  otherPkgs <- gsub(pkgsOnly, pattern = "package:", replacement = "")
  message("Determining other dependencies in the search path")
  otherDeps <- unlist(lapply(otherPkgs, function(pack) {
    tryCatch(miniCRAN::pkgDep(pack), error = function(x) NULL)
  }))

  # define a function that detaches and unloads everything, including namespaces from tools:rstudio
  # This function adapted from SO http://stackoverflow.com/a/6979989 , but with namespace unloading added
  detach_package <- function(pkg)
  {
    search_item <- paste("package", pkg, sep = ":")
    while(search_item %in% search()) {
      a <- detach(search_item, unload = TRUE, character.only = TRUE, force = TRUE)
    }

    unloaded <- NULL
    if(pkg %in% loadN) {
      unloaded <- tryCatch(unloadNamespace(pkg), error = function(x) FALSE)

      if(is.null(unloaded))
        loadN <<- loadN[!(loadN %in% pkg)]
    }
    #unloaded <- tryCatch(unloadNamespace(pkg), error = function(x) FALSE)
    return(invisible(unloaded))
  }

  # 2 choice for updating packages:
  # 1 Update ALL in your library -- may take a while
  # update.packages()

  # 2 Update only dependencies of SpaDES
  op <- old.packages(lib.loc = .libPaths()[1])
  needUpdated <- SpaDESDeps[SpaDESDeps %in% unname(op[,"Package"])]

  ip <- installed.packages(lib.loc = .libPaths()[1])
  needInstalled <- SpaDESDeps[!(SpaDESDeps %in% unname(ip[,"Package"]))]

  if(length(needUpdated) | length(needInstalled)) {
    message("Update only those packages that are needed to be updated")
    # unload everything
    message("Unloading all dependencies and their dependencies and theirs etc.")
    packs <- unique(c("devtools", "miniCRAN", otherDeps, SpaDESDeps))
    packsLeft <- packs
    # depending on order of dependencies, unloading may not work. Use while to put
    #  errors to the bottom of the list to be unloaded at the end
    #packsLeft <- c(packsLeft, loadedNamespaces())
    #packsLeft <- packsLeft[!(packsLeft %in% c("base", "methods", "datasets", "utils", "grDevices", "graphics", "stats"))]
    shortest <- NROW(packsLeft)
    while(length(packsLeft)) {

      print(packsLeft[1])
      done <- detach_package(packsLeft[1])
      if(is.null(done))  {
        packsLeft <- packsLeft[-1]
      } else  {
        message("Couldn't unload ", packsLeft[1])
        packsLeft <- c(packsLeft[-1],packsLeft[1]) # put at end
      }

      if(shortest==NROW(packsLeft)) {
        retries = retries + 1
      } else {
        retries = 0
      }

      if(retries >= 10) {
        stillGoing <- packsLeft[1]
        while(nzchar(stillGoing)) {
          b <- tryCatch(unloadNamespace(stillGoing),
                        error = function(x) as.character(x))
          if(is.null(b)) {
            stillGoing <- ""
          } else {
            stillGoing <- strsplit(b, split = "imported by ‘|’")[[1]][3]
          }
        }

      }
      shortest <- NROW(packsLeft)
      print(NROW(packsLeft))
      #if(all(packsLeft %in% c("stringr", "stringi", "magrittr"))) {
      #  unloadNamespace("lubridate")
      #}
    }

    if(length(needUpdated)) {
      message("Updating ", paste(needUpdated, collapse = ", "))
      for(i in needUpdated) {
        print(i)
        browser()
        try(install.packages(i))
      }

    }

    if(length(needInstalled)) {
      message("Installing ", paste(needInstalled, collapse = ", "))
      install.packages(needInstalled)
    }


    # install SpaDES
    #message("Update only those packages that are needed to be updated")
    #install.packages(rev(SpaDESDeps[!needUpdated & !needInstalled])) # specifying lib means it will look for dependencies also in the install location{
    message("SpaDES and all its dependencies are up to date.")
    env <- tryCatch(as.environment("tools:rstudio"), error = function(x) FALSE)
    if(autoRestart & is.environment(env)) {
      message("Restarting R")
      env$.rs.restartR()
    } else {
      message("You MUST restart your R session (e.g., Ctrl-Shift-F10 in Rstudio)")
    }

  } else {
    message("SpaDES was already up to date")
  }


}


# install dev version of SpaDES

# install.packages("devtools")
# devtools::install_github("PredictiveEcology/SpaDES@development")
# source("https://gist.githubusercontent.com/eliotmcintire/bcf54af7bffab0c619196ba223e88e80/raw/13d500485a7978b20d82a18bb3b727b285efddbb/installSpaDES.R")
installSpaDES2 <- function() {
  libPath <- .libPaths()[1]
  #
  # assign('.First', function(x) {
  #   #require('plyr')
  #   options(repos=c(CRAN="https://cran.rstudio.com/"))
  #   library(utils)
  #   library(methods)
  #   library(stats)
  #   #library(devtools)
  #   #install.packages("SpaDES", lib = libPath)
  #   devtools::install_github("PredictiveEcology/SpadES@development")
  #   #and whatever other packages you're using
  #   file.remove(".RData") #already been loaded
  #   rm(".Last", pos=.GlobalEnv) #otherwise won't be able to quit R without it restarting
  # }, pos=.GlobalEnv)
  # save.image()
  if(!require(nothing))
    devtools::install.packages("romainfrancois/nothing")
  file.path(R.home("bin"),"Rscript")
  system(paste0(file.path(R.home("bin"),"Rscript"),' -e "update.packages(ask=FALSE, lib=\'',libPath,'\')"'), wait=TRUE)
  system(paste0(file.path(R.home("bin"),"Rscript"),' -e "install.packages(\'SpaDES\', lib=\'',libPath,'\')"'), wait=TRUE)
  message("Please restart R.\nIf you want the development version of SpaDES",
          "try devtools::install_github('PredictiveEcology/SpaDES@development')")
  #devtools::install_github('PredictiveEcology/SpaDES@development')
}
#

