require(data.table)
library(microbenchmark)
library(ggplot2)

devtools::install_github("lineprof")
devtools::install_github("pryr")
devtools::install_github("shiny-slickgrid", "wch")
library(lineprof)
library(pryr)
library(shiny)


N <- 1e4
#tmp <- cbind(x=1:N, y=rnorm(N), z=rnorm(N))
tmp <- cbind(x=1:N, y=N:1, z=sample(1:N))
tmp.mat <- as.matrix(tmp)[order(tmp[,"x"]),]
tmp.df <- as.data.frame(tmp); tmp.df <- tmp.df[order(tmp.df$x),]
tmp.dt <- as.data.table(tmp); setkey(tmp.dt, x)

# manual insert by parts
new1.mat <- function(tmp.mat) {
  r <- sample(1:N, size=1) + rnorm(1)
  new <- cbind(x=r, y=rnorm(1), z=rnorm(1))
  
  rbind(tmp.mat[tmp.mat[,"x"]<=r,], new, tmp.mat[tmp.mat[,"x"]>r,])
}

new1.df <- function(tmp.df) {
  r <- sample(1:N, size=1) + rnorm(1)
  new <- cbind(x=r, y=rnorm(1), z=rnorm(1))
  
  rbind(tmp.df[tmp.df$x<=r,], as.data.frame(new), tmp.df[tmp.df$x>r,])
}

new1.dt <- function(tmp.dt) {
  r <- sample(1:N, size=1) + rnorm(1)
  new <- cbind(x=r, y=rnorm(1), z=rnorm(1))
  
  rbindlist(list(tmp.dt[x<=r], as.data.table(new), tmp.dt[x>r]))
}

# add to bottom; resort
new2.mat <- function(tmp.mat) {
  r <- sample(1:N, size=1) + rnorm(1)
  new <- cbind(x=r, y=rnorm(1), z=rnorm(1))
  
  rbind(tmp.mat, new)[order(rbind(tmp.mat, new)[,"x"]),]
}

new2.df <- function(tmp.df) {
  r <- sample(1:N, size=1) + rnorm(1)
  new <- cbind(x=r, y=rnorm(1), z=rnorm(1))
  
  tmp.df <- rbind(tmp.df, as.data.frame(new)); tmp.df[order(tmp.df$x),]
}

new2.dt <- function(tmp.dt) {
  r <- sample(1:N, size=1) + rnorm(1)
  new <- cbind(x=r, y=rnorm(1), z=rnorm(1))
  
  tmp.dt <- rbindlist(list(tmp.dt, as.data.table(new))); setkey(tmp.dt, x)
}


####### BENCHMARKS
mb = microbenchmark(
  new1.mat(tmp.mat),
  new1.df(tmp.df),
  new1.dt(tmp.dt),
  new2.mat(tmp.mat),
  new2.df(tmp.df),
  new2.dt(tmp.dt),
  times=1e3L)
autoplot.microbenchmark(mb)
mb

mat1 <- lineprof(new1.mat(tmp.mat))
shine(mat1)
