## Test environments

* OS X Yosemite     (local), R 3.1.3
* Ubuntu 12.04  (travis-ci), R 3.1.3
* Ubuntu 14.04      (local), R 3.1.3
* Windows 7         (local), R 3.1.2
* Windows 7         (local), R 3.1.3
* Windows     (win-builder), R 3.1.3
* Windows     (win-builder), R 3.2.0 beta

## R CMD check results

There were no ERRORs or WARNINGs

There were 2 NOTES:

1. This is a new CRAN package submission. The `fastshp` package in Suggests is not required, though instructions for installation via GitHub are provided in the description, README, and via a message to the user.

        * checking CRAN incoming feasibility ... NOTE
        Maintainer: ‘Alex M Chubaty <achubaty@NRCan.gc.ca>,
        Eliot J B McIntire <emcintir@NRCan.gc.ca>’
        New submission
        Suggests or Enhances not in mainstream repositories:
          fastshp

2. We provide helper functions that assist with loading user data into the global environment.

        * checking R code for possible problems ... NOTE
        Found the following assignments to the global environment:
        File 'SpaDES/R/environment.R':
          assign(x, value, envir = .GlobalEnv, ...)
          assign(x, get(x), envir = .GlobalEnv, ...)
        File 'SpaDES/R/load.R':
          assign(objectNames[x], do.call(get(loadFun[x]), args = argument), 
            envir = .GlobalEnv)
          assign(uniqueStacki, localStacks[[uniqueStacki]], envir = .GlobalEnv)
        File 'SpaDES/R/plotting.R':
          assign(objName, SpatialLines(lapply(seq_len(length(from)), function(x) {
            Lines(list(Line(coords = rbind(coordinates(from)[x, ], coordinates(to)[x, 
                ]))), ID = x)
        })), envir = .GlobalEnv)
          assign(objName, SpatialLines(lapply(seq_len(length(from)), function(x) {
            Lines(list(Line(coords = rbind(coordinates(from)[x, ], coordinates(to)[x, 
                ]))), ID = x)
        })), envir = .GlobalEnv)

## Downstream dependencies

There are currently no downstream dependencies of this package.
