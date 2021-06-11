## Updated release

This release is to restore this package on CRAN following removal of dependencies `reproducible`, `SpaDES.core`, ash `SpaDES.tools` (which have now been restored on CRAN).
Please note, the CRAN URL to a package vignette will work once the package is restored.
See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 3.6.3
* Ubuntu 20.04                 (GitHub), R 4.0.5
* Windows                      (GitHub), R 3.6.3
* Windows                      (GitHub), R 4.0.5
* Windows                 (win-builder), R 4.0.5

### Current R versions
* macOS 10.15.7 Catalina       (GitHub), R 4.1.0
* macOS 11.1 Big Sur            (local), R 4.1.0
* Ubuntu 20.04                 (GitHub), R 4.1.0
* Ubuntu 20.04                  (local), R 4.1.0
* Windows                      (GitHub), R 4.1.0
* Windows                       (local), R 4.1.0
* Windows                 (win-builder), R 4.1.0

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2021-06-07 r80458)
* Ubuntu 20.04                  (local), R-devel (2021-05-31 r80426)
* Windows                      (GitHub), R-devel (2021-06-07 r80458)
* Windows                 (win-builder), R-devel (2021-06-07 r80458)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

1. Some words were flagged as possibly misspelled, but they are false positives:

        Possibly mis-spelled words in DESCRIPTION:
          DES (8:77)
          Matloff (9:9)
          Metapackage (5:14)
          modularity (10:32)
          rasters (12:73)

## Downstream dependencies

There are currently no downstream dependencies of this package.
