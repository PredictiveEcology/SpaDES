setwd("~/GitHub/ABM/R")
source("ABM_code_files.R")
source("simulation.R")
dosim(sim.init, do.event, print.results, maxsimtime=10.00, modules=list("habitat", "caribou"))
