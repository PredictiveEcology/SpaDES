## Test environments
* OS X Yosemite install (local), R 3.1.3
* Ubuntu 12.04 (on travis-ci), R 3.1.2
* Ubuntu 14.04 (local), R 3.1.3
* Windows 7 install (local), R 3.1.2
* Windows 7 install (local), R 3.1.3

## R CMD check results
There were no ERRORs or WARNINGs

There were 2 NOTES:

1. This is a new CRAN package submission. The `fastshp` package in Suggests is not required, though instructions for installation via GitHub are provided in the description, README, and via a message to the user.

        * checking CRAN incoming feasibility ... NOTE
        Maintainer: ‘Alex M Chubaty <Alexander.Chubaty@NRCan.gc.ca>,
        Eliot J B McIntire <Eliot.McIntire@NRCan.gc.ca>’
        New submission
        Suggests or Enhances not in mainstream repositories:
          fastshp

2. We provide helper functions that assist with loading user data into the global environment.

        * checking R code for possible problems ... NOTE
        Found the following assignments to the global environment:
        File ‘SpaDES/R/load.R’:
          assign(objectNames[x], do.call(get(loadFun[x]), args = argument),
            envir = .GlobalEnv)
          assign(objectNames[x], get(objectNames[x], envir = .GlobalEnv),
            , envir = .GlobalEnv)
          assign(uniqueStacki, localStacks[[uniqueStacki]], envir = .GlobalEnv)

## Downstream dependencies
There are currently no downstream dependencies of this package.
