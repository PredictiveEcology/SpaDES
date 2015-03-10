n = 3
test.mod1.in <- data.frame(name=c("A","B","C"), class=rep("character", n), stringsAsFactors=FALSE)
test.mod1.out <- data.frame(name=c("C","W","X"), class=rep("character", n), stringsAsFactors=FALSE)

test.mod2.in <- data.frame(name=c("A","D","E"), class=rep("character", n), stringsAsFactors=FALSE)
test.mod2.out <- data.frame(name=c("C","Y","Z"), class=rep("character", n), stringsAsFactors=FALSE)

# merge deps into single list; add mod name; rm objects in both inputs & outputs
test.mod1.in$module <- "test1"
test.mod1.out$module <- "test1"

test.mod2.in$module <- "test2"
test.mod2.out$module <- "test2"

test.sim.in <- rbind(test.mod1.in, test.mod2.in)
test.sim.out <- rbind(test.mod1.out, test.mod2.out)

i <- which(test.sim.in$name %in% test.sim.out$name)
j <- which(test.sim.out$name %in% test.sim.in$name)
test.sim <- data.frame(from=c(rep("_IN_", nrow(test.sim.in[-i,])),
                              test.sim.out$module[j]),
                       to=c(test.sim.in$module[-i],
                            rep(test.sim.in$module[i], length(j))),
                       stringsAsFactors=FALSE)
## won't work with >1 module in j
test.sim <- test.sim[-which(test.sim$from==test.sim$to),]
test.sim <- test.sim[!duplicated(test.sim),]

# build deps graph
library(igraph)
test.graph <- graph.data.frame(test.sim)
plot(test.graph)

# resolve dependencies (topological sort)
tsort <- topological.sort(test.graph) # only works if acyclic!
loadOrder <- names(test.graph[[tsort,]])
loadOrder
