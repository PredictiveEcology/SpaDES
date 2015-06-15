test_that("simList object initializes correctly", {
  defaults <- list("checkpoint", "save", "progress", "load")

  times <- list(start=0.0, stop=10)
  params <- list(.globals=list(burnStats="npixelsburned", stackName="landscape"))
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  path <- system.file("sampleModules", package="SpaDES")

  mySim <- simInit(times, params, modules, objects=list(), path)

  expect_is(mySim, "simList")

  w <- getOption("width")
  options(width=100L)
  out <- capture.output(show(mySim))
  expect_equal(length(out), 79)
  options(width=w)

  ### SLOT .envir
  expect_is(simEnv(mySim), "environment")
  expect_is(simObjects(mySim), "list")
  expect_equal(sort(names(simObjects(mySim))),
               sort(names(as(mySim, "simList_")@.list)))
  expect_equivalent(mySim, as(as(mySim, "simList_"), "simList"))
  expect_equal(ls(mySim), sort(names(simObjects(mySim))))
  expect_equivalent(ls.str(mySim), ls.str(simObjects(mySim)))
  expect_equivalent(ls.str(pos=mySim), ls.str(simObjects(mySim)))
  expect_equivalent(ls.str(name=mySim), ls.str(simObjects(mySim)))

  mySim$test1 <- TRUE
  mySim[["test2"]] <- TRUE

  expect_true(mySim$test1)
  expect_true(mySim[["test2"]])

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
  expect_equal(simTimes(mySim), list(current=0.0, start=0.0, stop=10.0, timestepUnit="week"))
  expect_equal(simCurrentTime(mySim), 0.0)
  expect_equal(simStartTime(mySim), 0.0)
  expect_equal(simStopTime(mySim), 10.0)
  expect_equal(simTimestepUnit(mySim), "week")

  ### required packages
  pkgs <- c("grid", "methods", "RandomFields", "raster", "RColorBrewer", "sp",
            "SpaDES", "tkrplot")
  expect_equal(sort(simReqdPkgs(mySim)), sort(pkgs))
})

test_that("simulation runs with simInit and spades", {
  times <- list(start=0.0, stop=10, timestepUnit="year")
  params <- list(.globals=list(burnStats="npixelsburned", stackName="landscape"),
                 randomLandscapes=list(.plotInitialTime=NA, .plotInterval=NA),
                 caribouMovement=list(.plotInitialTime=NA, .plotInterval=NA, torus=TRUE),
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
  expect_equal(simTimestepUnit(mySim), "year")

  # sim results
  burnedLast <- 1893L
  pos_x <- c(-2.87454665449095, 48.0935099933344, 30.0143944670451,
             -38.4918828418324, 5.66381367657891, -1.33081646213299, -12.0714059173303,
             -32.0492634200707, 38.5060191188693, -14.5813900484182, -17.5404547563477,
             21.3833611445931, -5.94942384165676, -16.4833308888651, 33.7295603779531,
             -24.941391466017, -4.78954653919181, -32.2399412423608, 25.5539752195304,
             28.4881222492411, -37.6910721068201, 23.9901950520167, 21.2252498074277,
             27.8122120507533, 25.0950878403336, 34.5801435885286, -32.9767009907215,
             27.5866633720754, -20.6253327847497, 33.5402640259331, 39.1952558872756,
             -14.5419129512446, -40.7614393759695, 22.6385595712718, -16.1280822084358,
             49.5481185504061, -49.3125440706634, 32.9956426996742, 7.99113911343382,
             -43.5423311365738, -0.616725222349316, 49.418069427154, -38.5706101950476,
             -49.5448061022264, -0.296713656222728, -26.2924694668497, 23.2612676470533,
             -31.8751297866579, -16.3122661059375, 40.9278353603027, 30.3802266433747,
             -0.866051015178719, -17.8005895522856, 5.20866079120641, -13.0282673260958,
             20.2778790220555, 7.85089620286232, 27.0659215661551, 12.974706901319,
             36.9575435951238, 0.200162451244282, -19.3613920922762, -28.3676031292024,
             -18.6609302806433, 30.8026823334422, -0.183895942203968, 36.1321642642305,
             -27.1256134693072, 32.3439978034323, -35.2599393881649, 13.2743362775863,
             43.1056034408541, -7.96228973684946, -30.1855350236887, 20.4582703268826,
             -40.5922672025892, -38.7045202905931, 42.7412776971982, -2.55115281971686,
             -25.0873218820082, 35.9514728122242, 31.9469679523881, 2.78142907425111,
             4.47977890481152, 4.7060478640703, 27.5768203692877, -8.27345551808954,
             36.1531993659867, 40.9535666408864, 14.104281956016, 26.7429594473031,
             13.0770563072756, -42.1607187695279, -45.0945811042679, 16.0675385765023,
             30.6442159857169, 43.6698438503344, -0.215913150928785, 29.115125805245,
             -7.07095742965496)

  pos_y <- c(17.8006957921079, -31.8918161987373, -14.5307650060319,
             -18.0640248167051, -16.6928610956603, 47.1502783924791, -7.13596258042978,
             -11.1400616656515, 37.508311075321, 22.302877890612, -36.6668497594993,
             -2.8639460439959, 16.9167209143149, -22.8504200710594, 34.4973924302976,
             27.9383690501703, -15.7649416574947, -26.1711821998877, 11.7690226153923,
             -12.6085320300175, 47.0644626770483, -33.2391691569368, 15.582844247608,
             -28.2647426677175, -20.773437901712, 40.8880300759304, -48.9753904769094,
             -11.0713155659705, 38.1871986171378, -32.8453874425784, 0.694930334022985,
             15.4396646508537, 44.7626741055762, 20.5066003255901, 41.7479261710651,
             38.7330391490446, 18.261562065782, -43.7638796970869, -34.8515334044122,
             39.9785989222167, 6.53117212098366, -49.9657311531037, 9.07301481700259,
             -31.9233179552982, 41.8625254347852, -26.0156069672057, 19.0236911295497,
             20.2468897338057, -32.5444180900805, -23.2850413413566, 48.6422265252755,
             32.4657160880381, 38.4689084715019, -35.9817276833804, -8.97695233220036,
             -41.8630724004492, -1.47815276689042, -45.2146525448948, -10.761467627437,
             -6.89265663421187, 34.3873651699097, 23.2741945969134, -44.811197205715,
             12.0133365912308, -24.5390260923558, -33.4082088223091, -33.4839817750468,
             -9.86089536385494, 49.3859299032776, 18.0434806577847, 4.2912218924351,
             19.2005540600939, -36.6757347977763, 20.1411533455012, 3.06327406565664,
             -46.7917408966891, -20.5949435000821, -2.12695213841839, 4.081454804192,
             -27.0964252794801, 44.4379067474702, -40.0693067408268, 17.4359015072317,
             13.5736273822885, 12.4002225768151, 34.500284281536, -14.1067902072775,
             15.8711631061509, 43.8807610124899, -14.1766543119255, -4.13581340524223,
             -15.4268503608445, -15.0642500894036, 27.6555721114352, 48.1277487332406,
             -42.4249572825954, 7.05821930751326, 0.359595811947116, -17.4944397684922,
             -14.5871286796061)

  expect_equal(tail(completed$npixelsburned,1), burnedLast)
  expect_equivalent(rbind(completed$caribou$x, completed$caribou$y), rbind(pos_x, pos_y))
})
