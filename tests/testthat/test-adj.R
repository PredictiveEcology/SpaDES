test_that("adj.R results not identical to adjacent", {
  library(sp) # for adjacent function
  library(raster) # for adjacent function

  on.exit({
    detach("package:raster")
  })

#   a <- raster::raster(raster::extent(0, 1e3, 0, 1e3), res = 1)
#
#   # smaller sample (should use matrix)
#   s <- sample(1:length(a), 3)
#
#   expect_equal(adj(a, s, directions = 4, sort = TRUE, match.adjacent = TRUE),
#                      adjacent(a, s, directions = 4, sorted = TRUE))
#
#   expect_equal(adj(a, s, directions = 8, sort = TRUE, match.adjacent = TRUE),
#                      adjacent(a, s, directions = 8, sorted = TRUE))
#
#   expect_equal(adj(a, s, directions = "bishop", sort = TRUE, match.adjacent = TRUE),
#                      adjacent(a, s, directions = "bishop", sorted = TRUE))
#
#   expect_equal(adj(a, s, directions = 4, sort = TRUE, match.adjacent = TRUE,
#                    include = TRUE),
#                adjacent(a, s, directions = 4, sorted = TRUE, include = TRUE))
#
#   expect_equal(adj(a, s, directions = 8, sort = TRUE, match.adjacent = TRUE,
#                    include = TRUE),
#                adjacent(a, s, directions = 8, sorted = TRUE, include = TRUE))
#
#   expect_equal(adj(a, s, directions = "bishop", sort = TRUE,
#                    match.adjacent = TRUE, include = TRUE),
#                adjacent(a, s, directions = "bishop", sorted = TRUE, include = TRUE))
#
#   # test match.adjacent - it is just a different order
#   # gets ths same cells
#   expect_equal(sum(adj(a, s, directions = 4, sort = FALSE, match.adjacent = FALSE) -
#                      adjacent(a, s, directions = 4, sorted = FALSE)), 0)
#
#   # but not in the same order
#   expect_gt(sum(
#     (adj(a, s, directions = 4, sort = FALSE, match.adjacent = FALSE) -
#        adjacent(a, s, directions = 4, sorted = FALSE))^2
#   ), 0)
#
#   # test match.adjacent - primarily for directions = 4, or bishop
#   # gets ths same cells
#   expect_equal(sum(
#     adj(a, s, directions = "bishop", sort = FALSE, match.adjacent = FALSE) -
#       adjacent(a, s, directions = "bishop", sorted = FALSE)
#   ), 0)
#
#   # but not in the same order
#   expect_gt(
#     sum((adj(a, s, directions = "bishop", sort = FALSE, match.adjacent = FALSE) -
#            adjacent(a, s, directions = "bishop", sorted = FALSE))^2),
#     0)
#
#   # gets ths same cells
#   expect_equal(
#     sum(adj(a, s, directions = 8, sort = FALSE, match.adjacent = FALSE) -
#           adjacent(a, s, directions = 8, sorted = FALSE)),
#     0)
#   # but not in the same order
#   expect_gt(
#     sum((adj(a, s, directions = 8, sort = FALSE, match.adjacent = FALSE) -
#            adjacent(a, s, directions = 8, sorted = FALSE))^2),
#     0)
#
#   # Test include = TRUE
#   expect_equal(
#     sum(adj(a, s, directions = 4, sort = TRUE, match.adjacent = FALSE, include = TRUE) -
#           adjacent(a, s, directions = 4, sorted = TRUE, include = TRUE)),
#     0)
#
#   expect_equal(
#     sum(adj(a, s, directions = 8, sort = TRUE, match.adjacent = FALSE, include = TRUE) -
#           adjacent(a, s, directions = 8, sorted = TRUE, include = TRUE)),
#     0)
#
#   expect_equal(
#     sum(adj(a, s, directions = "bishop", sort = TRUE, match.adjacent = FALSE, include = TRUE) -
#           adjacent(a, s, directions = "bishop", sorted = TRUE, include = TRUE)),
#     0)
#
#   # include = TRUE with match.adjacent = TRUE
#   expect_equal(
#     sum(adj(a, s, directions = 4, sort = TRUE, match.adjacent = TRUE, include = TRUE) -
#           adjacent(a, s, directions = 4, sorted = TRUE, include = TRUE)),
#     0)
#
#   expect_equal(
#     sum(adj(a, s, directions = 8, sort = TRUE, match.adjacent = TRUE, include = TRUE) -
#           adjacent(a, s, directions = 8, sorted = TRUE, include = TRUE)),
#     0)
#
#   expect_equal(
#     sum(adj(a, s, directions = "bishop", sort = TRUE, match.adjacent = TRUE, include = TRUE) -
#           adjacent(a, s, directions = "bishop", sorted = TRUE, include = TRUE)),
#     0)
#
# ################################ data.table portion
#   # larger sample (should use data.table)
#   s <- sample(1:length(a), 3)
#
#   expect_equal(adj(a, s, directions = 4, sort = FALSE, match.adjacent = TRUE,
#                    cutoff.for.data.table = 2),
#                adjacent(a, s, directions = 4, sorted = FALSE))
#
#   expect_equal(adj(a, s, directions = 8, sort = FALSE, match.adjacent = TRUE,
#                    cutoff.for.data.table = 2),
#                adjacent(a, s, directions = 8, sorted = FALSE))
#
#   expect_equal(adj(a, s, directions = "bishop", sort = FALSE, match.adjacent = TRUE,
#                    cutoff.for.data.table = 2),
#                adjacent(a, s, directions = "bishop", sorted = FALSE))
#
#   expect_equal(adj(a, s, directions = 4, sort = FALSE, match.adjacent = TRUE,
#                    include = TRUE, cutoff.for.data.table = 2),
#                adjacent(a, s, directions = 4, sorted = FALSE, include = TRUE))
#
#   expect_equal(adj(a, s, directions = 8, sort = FALSE, match.adjacent = TRUE,
#                    include = TRUE,cutoff.for.data.table  =  2),
#                adjacent(a, s, directions = 8, sorted = FALSE, include = TRUE))
#
#   expect_equal(adj(a, s, directions = "bishop", sort = FALSE, match.adjacent = TRUE,
#                    include = TRUE, cutoff.for.data.table = 2),
#                adjacent(a, s, directions = "bishop", sorted = FALSE, include = TRUE))
#
#   # test match.adjacent - it is just a different order
#   # gets ths same cells
#   expect_equal(
#     sum(adj(a, s, directions = 4, sort = FALSE, match.adjacent = FALSE,
#             cutoff.for.data.table = 2) -
#           adjacent(a, s, directions = 4, sorted = FALSE)),
#     0)
#
#   # but not in the same order
#   expect_gt(
#     sum((adj(a, s, directions = 4, sort = FALSE, match.adjacent = FALSE,
#              cutoff.for.data.table = 2) -
#            adjacent(a, s, directions = 4, sorted = FALSE))^2),
#     0)
#
#   # test match.adjacent - primarily for directions = 4, or bishop
#   # gets ths same cells
#   expect_equal(
#     sum(adj(a, s, directions = "bishop", sort = FALSE, match.adjacent = FALSE,
#             cutoff.for.data.table = 2) -
#           adjacent(a, s, directions = "bishop", sorted = FALSE)),
#     0)
#
#   # but not in the same order
#   expect_gt(
#     sum((adj(a, s, directions = "bishop", sort = FALSE, match.adjacent = FALSE,
#              cutoff.for.data.table = 2) -
#            adjacent(a, s, directions = "bishop", sorted = FALSE))^2),
#     0)
#
#   # gets ths same cells
#   expect_equal(
#     sum(adj(a, s, directions = 8, sort = FALSE, match.adjacent = FALSE,
#             cutoff.for.data.table = 2) -
#           adjacent(a, s, directions = 8, sorted = FALSE)),
#     0)
#
#   # but not in the same order
#   expect_gt(
#     sum((adj(a, s, directions = 8, sort = FALSE, match.adjacent = FALSE,
#              cutoff.for.data.table = 2) -
#            adjacent(a, s, directions = 8, sorted = FALSE))^2),
#     0)
#
#   # Test include = TRUE
#   expect_equal(
#     sum(adj(a, s, directions = 4, sort = TRUE, match.adjacent = FALSE,
#             include = TRUE, cutoff.for.data.table = 2) -
#           adjacent(a, s, directions = 4, sorted = TRUE, include = TRUE)),
#     0)
#
#   expect_equal(
#     sum(adj(a, s, directions = 8, sort = TRUE, match.adjacent = FALSE,
#             include = TRUE, cutoff.for.data.table = 2) -
#           adjacent(a, s, directions = 8, sorted = TRUE, include = TRUE)),
#     0)
#
#   expect_equal(
#     sum(adj(a, s, directions = "bishop", sort = TRUE, match.adjacent = FALSE,
#             include = TRUE, cutoff.for.data.table = 2) -
#           adjacent(a, s, directions = "bishop", sorted = TRUE, include = TRUE)),
#     0)
#
#   # include = TRUE with match.adjacent = TRUE
#   expect_equal(
#     sum(adj(a, s, directions = 4, sort = TRUE, match.adjacent = TRUE,
#             include = TRUE, cutoff.for.data.table = 2) -
#           adjacent(a, s, directions = 4, sorted = TRUE, include = TRUE)),
#     0)
#
#   expect_equal(
#     sum(adj(a, s, directions = 8, sort = TRUE, match.adjacent = TRUE,
#             include = TRUE, cutoff.for.data.table = 2) -
#           adjacent(a, s, directions = 8, sorted = TRUE, include = TRUE)),
#     0)
#
#   expect_equal(
#     sum(adj(a, s, directions = "bishop", sort = TRUE, match.adjacent = TRUE,
#             include = TRUE, cutoff.for.data.table = 2) -
#           adjacent(a, s, directions = "bishop", sorted = TRUE, include = TRUE)),
#     0)
#
# })
#
#
# test_that("compare matrix internals with data.table internals", {
#   # larger sample (should use data.table)

  a1 <- Sys.time()
  a <- raster::raster(raster::extent(0, 1e1, 0, 1e1), res = 1)
  sam <- sample(1:length(a), 4 )

  for(incl in c(TRUE, FALSE)) {
    for(ids in list(NULL, seq_len(length(sam)))) {
      for(sortTF in c(TRUE, FALSE)) {
        for(ma in c(TRUE, FALSE)) {
          for(dirs in list(4, 8, "bishop")) {
            for(prs in c(TRUE, FALSE)) {
              for(tor in c(TRUE, FALSE)) {
                adjDT <- adj.raw(a, sam, directions = dirs, sort = sortTF, match.adjacent = ma,
                                  include = incl,
                              cutoff.for.data.table = 2, id = ids, pairs = prs, torus = tor)
                adjMat <- adj.raw(a, sam, directions = dirs, sort = sortTF, match.adjacent = ma,
                                 include = incl,
                              id = ids, pairs = prs, torus = tor)
                browser(expr=!isTRUE(all.equal(adjMat, adjDT)))
                expect_true(isTRUE(all.equal(adjMat, adjDT)))
                #numTests <<- numTests+1
                if(!tor) {
                  adj2 <- raster::adjacent(a, sam, directions = dirs, sorted = sortTF, include = incl,
                                           id = !is.null(ids), pairs = prs)
                  if(!prs) {
                    if(ma) {
                      browser(expr=!isTRUE(all.equal(adjDT, adj2)))

                      expect_equal(adjDT, adj2, info = paste0("ma=",ma, ", dirs=",dirs, ", sortTF=",sortTF,
                                                              ", incl=",incl,", is.null(ids)=",is.null(ids),
                                                              ", prs=",prs))
                      #numTests <<- numTests+1
                    } else {
                      #browser()
                      expect_equal(unique(sort(adjDT[,"to"])), sort(adj2))
                      #numTests <<- numTests+1
                    }
                  } else {
                    colOrd <- if(is.null(ids)) 1:2 else c(2,3,1)
                    if(ma) {
                      if(!sortTF) {
                        expect_equal(adjDT, adj2[,colOrd])
                        #numTests <<- numTests+1
                      } else {
                        expect_equal(adjDT, adj2[order(adj2[,"from"], adj2[,"to"]),colOrd])
                        #numTests <<- numTests+1
                      }
                    } else {
                      if(!sortTF) {# if match.adjacent is FALSE, and sort is FALSE, then they mostly don't match
                        #if(adjDT[order(adjDT[,"from"], adjDT[,"to"]),]==adjDT)
                         if(sum((adjDT - adj2[,colOrd])^2)==0) {
                           expect_equal(adjDT, adj2[,colOrd])
                         } else {
                           expect_gt(sum((adjDT - adj2[,colOrd])^2), 0) # sum of squared difference should be positive
                           #numTests <<- numTests+1
                         }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  b1 <- Sys.time()

  skip("benchmarking only")
  microbenchmark(adjDT <- adj.raw(a, sam, directions = dirs, sort = sortTF, match.adjacent = ma,
                   include = incl,
                   cutoff.for.data.table = 2, id = ids, pairs = prs, torus = tor), times = 1e3)
  #     min      lq     mean median       uq      max neval
  #1.922093 1.98368 2.593587  2.046 2.258475 6.054565  1000

})

test_that("adj.R: torus does not work as expected", {
  # test data.table and matrix
  for(i in c(100,1)) {
    # a corner
    a <- raster::raster(raster::extent(0, 4, 0, 4), res = 1)
    s <- 4
    newCells <- adj(a, s, directions = 4, sort = TRUE, cutoff.for.data.table = i,
                    match.adjacent = TRUE, pairs = FALSE, torus = TRUE)
    expect_identical(sort(as.numeric(newCells)), c(1,3, 8,16))

    # a corner
    a <- raster::raster(raster::extent(0, 4, 0, 4), res = 1)
    s <- 1
    newCells <- adj(a, s, directions = 4, sort = TRUE,cutoff.for.data.table = i,
                    match.adjacent = TRUE, pairs = FALSE, torus = TRUE)
    expect_identical(sort(as.numeric(newCells)), c(2, 4,5, 13))

    # a side
    a <- raster::raster(raster::extent(0, 4, 0, 4), res = 1)
    s <- 12
    newCells <- adj(a, s, directions = 4, sort = TRUE, cutoff.for.data.table = i,
                    match.adjacent = TRUE, pairs = FALSE, torus = TRUE)
    expect_identical(sort(as.numeric(newCells)), c(8, 9,11, 16))

    # a corner
    a <- raster::raster(raster::extent(0, 4, 0, 4), res = 1)
    s <- 16
    newCells <- adj(a, s, directions = 4, sort = TRUE, cutoff.for.data.table = i,
                    match.adjacent = TRUE, pairs = FALSE, torus = TRUE)
    expect_identical(sort(as.numeric(newCells)), c(4, 12, 13, 15))

    # a corner with 8 neighbours
    a <- raster::raster(raster::extent(0, 4, 0, 4), res = 1)
    s <- 16
    newCells <- adj(a, s, directions = 8, sort = TRUE, cutoff.for.data.table = i,
                    match.adjacent = TRUE, pairs = FALSE, torus = TRUE)
    expect_identical(sort(as.numeric(newCells)), c(1, 3, 4, 9, 11, 12, 13, 15))

    # a corner with 8 neighbours
    a <- raster::raster(raster::extent(0, 4, 0, 4), res = 1)
    s <- 1
    newCells <- adj(a, s, directions = 8, sort = TRUE,cutoff.for.data.table = i,
                    match.adjacent = TRUE, pairs = FALSE, torus = TRUE)
    expect_identical(sort(as.numeric(newCells)), c(2, 4, 5, 6, 8, 13, 14, 16))

  }
  #expect_equal(
  #  sum(adj(a, s, directions = "bishop") - adjacent(a, s, directions = "bishop")),
  #  0)
})
