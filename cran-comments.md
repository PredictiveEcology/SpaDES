## Updated release

This release is to restore this package on CRAN following removal of dependency `SpaDES.tools` (which has now been restored on CRAN).
See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 3.6.3
* Ubuntu 20.04                 (GitHub), R 4.0.5
* Windows                      (GitHub), R 3.6.3
* Windows                      (GitHub), R 4.0.5
* Windows                 (win-builder), R 4.0.5

### Current R versions
* macOS 10.15.7 Catalina       (GitHub), R 4.1.2
* macOS 11.6 Big Sur            (local), R 4.1.2
* macOs (m1) Big Sur             (rhub), R 4.1.2
* Ubuntu 20.04                 (GitHub), R 4.1.2
* Ubuntu 20.04                  (local), R 4.1.2
* Windows                      (GitHub), R 4.1.2
* Windows                       (local), R 4.1.2
* Windows                 (win-builder), R 4.1.2

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2022-02-12 r81722)
* Windows                      (GitHub), R-devel (2022-02-13 r81727 ucrt)
* Windows                 (win-builder), R-devel (2022-02-15 r81747 ucrt)

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
