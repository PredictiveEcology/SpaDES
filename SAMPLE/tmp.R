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

x <- left_join(test.sim.in, test.sim.out, by="name") %>%
  mutate(module.y=replace(module.y, is.na(module.y), "_IN_"))

test.sim <- with(x, data.frame(from=module.y, to=module.x, objName=name, objClass=class.x, stringsAsFactors=FALSE))

plotting <- FALSE
if (plotting) {
  test.sim <- test.sim[!duplicated(test.sim[,1:2]),]
}

# build deps graph
library(igraph)
test.graph <- graph.data.frame(test.sim) # vertices=modules
plot(test.graph)

# detect cycles

v <- shortest.paths(test.graph, mode="out")

a <- {row in v}
b <- {col in v}

if (0 < v[a,b] < Inf) && (0 < v[b,a] < Inf) {
  # cycle detected
  pth1 = get.shortest.paths(test.graph, from=name(v[a,]), to=name(v[,b]))
  pth2 = get.shortest.paths(test.graph, from=name(v[,b]), to=name(v[a,]))

  # look at this these paths and see if we can ignore any of them

  # REPEAT until something...yell at user if there are any we can't ignore
}


# resolve dependencies (topological sort)
tsort <- topological.sort(test.graph) # only works if acyclic!
loadOrder <- names(test.graph[[tsort,]])
loadOrder
