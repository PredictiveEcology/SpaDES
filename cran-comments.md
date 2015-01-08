## Test environments
* local OS X install, R 3.1.2
* local Windows 7 install, R 3.1.2
* local Ubuntu 14.04, R 3.1.2
* Ubuntu 12.04 (on travis-ci), R 3.1.2

## R CMD check results
There were no ERRORs or WARNINGs

- automated builds on headless servers (i.e., without X11) may generate warning when loading dependencies (tcltk) due to no DISPLAY variable being set. This is not a problem otherwise.

There was 1 NOTE:

    * checking R code for possible problems ... NOTE
    Found the following assignments to the global environment:
    File ‘SpaDES/R/load.R’:
      assign(objectNames[x], do.call(get(loadFun[x]), args = argument), 
        envir = .GlobalEnv)
      assign(objectNames[x], get(objectNames[x], envir = .GlobalEnv), 
        , envir = .GlobalEnv)
      assign(uniqueStacki, localStacks[[uniqueStacki]], envir = .GlobalEnv)

These are helper functions that assist with loading user data into the global environment.

## Downstream dependencies
none (this is a new CRAN package)
