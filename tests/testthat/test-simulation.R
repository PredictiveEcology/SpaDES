test_that("simList object initializes correctly", {
  defaults <- list("checkpoint", "save", "progress", "load")

  times <- list(start=0.0, stop=10)
  params <- list(.globals=list(burnStats="npixelsburned", stackName="landscape"))
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  path <- system.file("sampleModules", package="SpaDES")

  mySim <- simInit(times, params, modules, objects=list(), path)

  expect_is(mySim, "simList")

  ### SLOT .envir
  expect_is(simEnv(mySim), "environment")
  expect_is(simObjects(mySim), "list")
  expect_equal(sort(names(simObjects(mySim))),
               sort(names(as(mySim, "simList_")@.list)))
  #expect_equal(simObjects(mySim), as(mySim, "simList_")@.list) # not sorted
  expect_equal(ls(mySim), sort(names(simObjects(mySim))))
  expect_equal(ls.str(mySim), ls.str(simObjects(mySim)))

  ### SLOT .loadOrder
  expect_is(simModulesLoadOrder(mySim), "character")
  expect_equal(simModulesLoadOrder(mySim), unlist(modules))

  ### SLOT .loaded
  expect_is(simModulesLoaded(mySim), "list")
  expect_equal(simModulesLoaded(mySim), as.list(c(defaults, modules)))

  ### SLOT modules
  expect_is(simModules(mySim), "list")
  expect_equal(simModules(mySim), as.list(c(defaults, modules)))

  ### SLOT params
  expect_is(simParams(mySim), "list")

  # checkpoint
  expect_true(is.null(simCheckpointFile(mySim)))
  expect_true(is.na(simCheckpointInterval(mySim)))

  # progress
  expect_true(is.na(simProgressGraphical(mySim)))
  expect_true(is.na(simProgressInterval(mySim)))

  ### SLOT events
  expect_is(simEvents(mySim), "data.table")
  expect_equal(nrow(simEvents(mySim)), length(simModulesLoaded(mySim)))

  ### SLOT completed
  expect_is(simCompleted(mySim), "data.table")
  expect_equal(nrow(simCompleted(mySim)), 0)

  ### SLOT depends
  expect_is(simDepends(mySim), ".simDeps")
  expect_is(simDepends(mySim)@dependencies, "list")
  expect_is(simDepends(mySim)@dependencies[[3]], ".moduleDeps")
  expect_equal(simDepends(mySim)@dependencies[[3]]@name, "fireSpread")
  # not going to go though each level...object validity checking does types

  ### SLOT simtimes
  expect_equal(simTimes(mySim), list(current=0.0, start=0.0, stop=10.0))
  expect_equal(simCurrentTime(mySim), 0.0)
  expect_equal(simStartTime(mySim), 0.0)
  expect_equal(simStopTime(mySim), 10.0)
})

test_that("simulation runs with simInit and spades", {
  times <- list(start=0.0, stop=10)
  params <- list(.globals=list(burnStats="npixelsburned", stackName="landscape"),
                 randomLandscapes=list(.plotInitialTime=NA, .plotInterval=NA),
                 caribouMovement=list(.plotInitialTime=NA, .plotInterval=NA),
                 fireSpread=list(.plotInitialTime=NA, .plotInterval=NA))
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  path <- system.file("sampleModules", package="SpaDES")

  set.seed(42)
  mySim <- simInit(times, params, modules, objects=list(), path)
  completed <- spades(mySim)

  # simtime
  expect_equal(simCurrentTime(completed), 10.0)
  expect_equal(simStartTime(completed), 0.0)
  expect_equal(simStopTime(completed), 10.0)

  # sim results
  burned <- 785L
  pos_x <- c(34.6289457319267, -43.2793178546226, -20.3936088171444, 5.67922179888406,
             41.9735594150236, 7.34987799013509, -37.9834867100111, 21.9961566490671,
             -39.1786706637474, 22.7620245669939, 0.639278390821264, 44.2452005567662,
             23.1197866140505, 35.9845470183223, 39.4244508449606, 26.4481959870015,
             -16.9319603537716, -43.8560534050598, -31.79757378467, -49.0846701349835,
             28.5663592372485, -36.3990153054854, -39.7530082889013, -32.9169520898446,
             -2.00772094814462, 6.23729869359948, 25.4941426604116, -32.3247618510832,
             -47.0287574075036, 13.7945608185815, 2.81344676423437, 27.8418287586509,
             -29.0438379722354, 11.707634710164, -2.20297327873627, -23.4629791558281,
             29.8931528829312, 10.5393992569332, 21.9594583625961, -32.2577023906145,
             -38.3377334361566, 46.8138143378683, -22.3159696217701, -22.6829037328729,
             43.6834970556788, 23.3506643002835, 11.227016400296, -16.4145635043117,
             15.3142780943373, -12.1492376653322, 19.7744447976996, 37.8122871320644,
             -11.1191017676275, -19.8206472994941, 4.35436032690686, 15.1441792442985,
             -39.5971233643962, -21.1061878347666, -46.7500133967516, 2.80559927348774,
             23.9274122075933, 24.3386085400402, 14.1046602429232, 28.0905434208471,
             41.5690096136786, 39.8516759027917, 38.4599047597201)

  pos_y <- c(3.04297525930788, -30.2867742941098, -36.1295761593789, 46.5862139832211,
             -9.60691763268749, -38.3467167117232, -36.2716732747549, -32.280606863747,
             -31.2486728856556, -21.2486267113892, 43.8499911321602, -44.1628319966242,
             48.631131593274, 11.8552181661594, 14.3004735668538, -22.1296042541268,
             -17.0534346393919, -12.7044385535963, -6.69772258400444, 25.4284810004158,
             6.81553430530295, -13.708721516309, -19.8938336686891, -14.098532291804,
             38.1484459096673, -3.98709045840552, 29.3668385220884, 12.0433508686709,
             6.62774212363335, 41.6214554113344, -36.3494515257043, 18.6185773265913,
             35.0183352686262, -15.5591819452852, -26.9534051507221, 32.8781311522819,
             -16.4491653386894, -41.8681693030517, -31.8019477558076, -5.47905783583136,
             2.58667269600658, 33.0717539564875, -44.8773364585229, 24.4346805507192,
             -32.321419414254, 49.096560448403, -0.487326496211879, -35.2024360435035,
             44.4519140275989, -15.1779421304197, -40.1097217865957, 29.2018063557889,
             -21.8314177446971, -40.6644985062224, 18.3517902263833, -22.0225771017639,
             14.8549178660374, -27.5119304206573, 39.4537079306314, 46.3126907388249,
             -16.7176357823238, -25.3701674144822, -25.4725816359998, -20.3661767600425,
             -27.8991504485108, 9.70049796022525, 26.4773798337827)

  expect_equal(completed$npixelsburned, burned)
  expect_equivalent(rbind(completed$caribou$x1, completed$caribou$y1), rbind(pos_x, pos_y))
})
