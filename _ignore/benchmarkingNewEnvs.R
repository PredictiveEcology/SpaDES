library(raster)
library(data.table)
library(microbenchmark)
library(SpaDES)

N <- 1e4
sim <- new.env(parent = .GlobalEnv)
sim <- new.env()
sim$landscapeDT <- data.table(test = 1:(N^2), hello = sample(1:(N^2), (N^2)))
sim$ras <- raster(x = extent(0, N, 0, N), res = 1)
sim$ras[] <- sim$landscapeDT$test


landscapeDT <- data.table::copy(sim$landscapeDT)
ras <- raster(x = extent(0, N, 0, N), res = 1)
ras[] <- -sim$landscapeDT$test

vec <- getValues(ras)

ras2 <- 1:10

# Note return invisible empty ... means no copy is made. The object is actually modified in place
#  as long as it is the same name, i.e., sim, as the environment name
changeValDT  <- function(sim, index, val) {
  sim$landscapeDT[index,hello:=val]
  invisible()
}

changeValDTNoSim  <- function(sim, index, val) {
  landscapeDT[index,hello:=val]
  invisible()
}

changeValRas  <- function(sim, index, val) {
  sim$ras[index]  <- val
  invisible()
}

index=sample(N^2, 5); print(index)

# Note - exact same time, meaning no copying is happening
microbenchmark(times=2000,
               new=changeValDT(sim, index=index, val=index),
               old=#{landscape <- getGlobal("landscape");
                landscapeDT[index,hello:=index],
               alt=changeValDTNoSim(sim, index=index, val=index)
               # assignGlobal("landscape")}
               )
print(sim$landscapeDT[index,])
print(landscapeDT[index,])
print(landscapeDT[index,])

# Note - exact same time, meaning no copying is happening
microbenchmark(times = 10,
               changeValRas(sim, index, index),
               ras[index] <- index,
               vec[index] <- index)

sim <- new.env()
microbenchmark(times = 100,
               makeEnv = {sim$ras <- ras; sim$landscape <- landscape},
               makeList = {sim2 = list(ras, landscape)})

names(sim$ras) <- "Fires"
setColors(sim$ras) <- c("white", rev(heat.colors(9)))
sim$ras <- setValues(sim$ras, 1)
