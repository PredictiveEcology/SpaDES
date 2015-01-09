## Test environments
* local OS X install, R 3.1.2
* local Windows 7 install, R 3.1.2
* local Ubuntu 14.04, R 3.1.2
* Ubuntu 12.04 (on travis-ci), R 3.1.2

## R CMD check results
There were no ERRORs or WARNINGs

- automated builds on headless servers (i.e., without X11) may generate warning when loading dependencies (tcltk) due to no DISPLAY variable being set. This is not a problem otherwise.

There were 2 NOTES:

1. This is a new CRAN package submission. The `fastshp` package in Suggests is not required, though instructions for installation via GitHub are provided to the user via a message.

        * checking CRAN incoming feasibility ... NOTE
        Maintainer: ‘Alex M Chubaty <Alexander.Chubaty@NRCan.gc.ca>,
        Eliot J B McIntire <Eliot.McIntire@NRCan.gc.ca>’
        New submission
        Suggests or Enhances not in mainstream repositories:
          fastshp
          Availability using Additional_repositories specification:
          fastshp   no
                         https://github.com/s-u/fastshp
        Package has a VignetteBuilder field but no prebuilt vignette index.

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
none (this is a new CRAN package)
