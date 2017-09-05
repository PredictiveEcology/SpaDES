library(fastdigest)


#.list
str(attributes(mySimOut),1)
a <- attributes(mySimOut)[["debugCache2"]] # The .e
sapply(a, fastdigest)
sapply(slotNames(a[[1]]), function(x) fastdigest(slot(a[[1]], x)))


fastdigest(a[[1]])


# Outer
a <- attributes(mySimOut)[[4]]
lapply(a, fastdigest)
sapply(slotNames(a[[1]]), function(x) fastdigest(x))

a <- ecoregionMap
sapply(slotNames(a), function(x) fastdigest(slot(a, x)))
