#require(Rcpp, lib.loc="~/R-dev")
#require(dplyr, lib.loc="~/R-dev")
#require(igraph, lib.loc="~/R-dev")

outputPath <- file.path(tempdir(), "simOutputs")
times <- list(start=0.0, stop=10.01)
parameters <- list(.globals=list(.stackName="landscape", .outputPath=outputPath,
                                 burnStats="nPixelsBurned"),
                   .progress=list(NA),
                   randomLandscapes=list(nx=1e2, ny=1e2, inRAM=TRUE),
                   fireSpread=list(nFires= 1e1, spreadprob=0.225, its=1e6,
                                   persistprob=0, returnInterval=10, startTime=0,
                                   .plotInitialTime=0.1, .plotInterval=10),
                   caribouMovement=list(N=1e2, moveInterval=1,
                                        .plotInitialTime=1.01, .plotInterval=1)
)
modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
path <- system.file("sampleModules", package="SpaDES")

mySim <- simInit(times=times, params=parameters, modules=modules, path=path)

# see what it looks like
<<<<<<< HEAD
plot(simGraph.F)
plot(simGraph.T) # the version returned to user

# detect and prune cycles
M <- shortest.paths(simGraph.F, mode="out")

pth <- list()
for (row in 1L:(nrow(M)-1)) {
  for (col in (row+1L):ncol(M)) {
    current = M[row,col]
    partner = M[col,row]
    if (all((current>0), !is.infinite(current), (partner>0), !is.infinite(partner))) {
      pth1 = get.shortest.paths(simGraph.F, from=rownames(M)[row], to=colnames(M)[col])
      pth2 = get.shortest.paths(simGraph.F, from=colnames(M)[col], to=rownames(M)[row])
      pth = append(pth, unique(c(pth1$vpath, pth2$vpath)))
    }
  }
}
lapply(pth, function(x) { rownames(M)[x] })

# a <- {row in M}
# b <- {col in M}
#
# if (0 < M[a,b] < Inf) && (0 < M[b,a] < Inf) {
#   # cycle detected
#   pth1 = get.shortest.paths(test.graph, from=name(M[a,]), to=name(M[,b]))
#   pth2 = get.shortest.paths(test.graph, from=name(M[,b]), to=name(M[a,]))
#
#   # look at this these paths and see if we can ignore any of them
#
#   # REPEAT until something...yell at user if there are any we can't ignore
# }

=======
simGraph = depsGraph(mySim, plot=TRUE) # the version returned to user for plotting
plot(simGraph)
>>>>>>> origin/module-deps

