## Updated release

This release fixes some issues with CRAN checks and other package inconsistencies.
See `NEWS.md` for a full description of changes.

Since this is a metapackage, we have not included any examples as these are better suited for their respective packages.

## Test environments

### Previous R versions
* Ubuntu 18.04                 (GitHub), R 3.6.3
* Windows                      (GitHub), R 3.6.3
* Windows                 (win-builder), R 3.6.3

### Current R versions
* macOS 10.15.6 Catalina       (GitHub), R 4.0.3
* macOS 11.1 Big Sur            (local), R 4.0.3
* Ubuntu 18.04                 (GitHub), R 4.0.3
* Ubuntu 20.04                  (local), R 4.0.3
* Windows                      (GitHub), R 4.0.3
* Windows                 (win-builder), R 4.0.3

### Development R version
* Ubuntu 18.04                 (GitHub), R 4.1.0 (2021-01-03 r79781)
* Ubuntu 20.04                  (local), R 4.1.0 (2021-01-05 r79797)
* Windows                      (GitHub), R 4.1.0 (2021-01-03 r79781)
* Windows                 (win-builder), R 4.1.0 (2021-01-02 r79767)

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
