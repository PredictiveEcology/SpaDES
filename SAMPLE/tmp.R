n = 5
test.mod1.in <- data.frame(name=LETTERS[1:n], class=rep("character", n))
test.mod1.out <- data.frame(name=LETTERS[23:(23-n+1)], class=rep("character", n))

test.mod2.in <- data.frame(name=LETTERS[23:(23-n+1)], class=rep("character", n))
test.mod2.out <- data.frame(name=LETTERS[26:(26-n+1)], class=rep("character", n))

# merge deps into single list; add mod name; rm objects in both inputs & outputs
test.mod1.in$module <- "test1"
test.mod1.out$module <- "test1"

test.mod2.in$module <- "test2"
test.mod2.out$module <- "test2"

test.sim.in <- rbind(test.mod1.in, test.mod2.in)
test.sim.out <- rbind(test.mod1.out, test.mod2.out)

test.sim <- data.frame(from=c(test.sim.in$module, test.sim.out$name),
                       to=c(test.sim.in$name, test.sim.out$module))

with(test.sim, from %in% to) # modules shouldn't be nodes. remove in/out for diff mods only!

# build deps graph
library(igraph)
test.graph <- graph.data.frame(test.sim)
plot(test.graph)

# resolve dependencies (topological sort)
loadOrder <- topological.sort(test.graph) # only works if acyclic!
test.graph[loadOrder]
