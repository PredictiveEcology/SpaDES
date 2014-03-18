setwd("/Users/achubaty/Dropbox/POSTDOCS/CFS-MPB-2014/r-code/ABM/R")
source("ABM_code_files.R")
source("simulation.R")
dosim(globals.init, react.event, print.results, maxsimtime=10.00, modules=list("habitat", "caribou"))
