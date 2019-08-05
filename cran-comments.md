## Updated release

This release drops suuport for R 3.3 and 3.4, as these are longor supported by several dependencies.

Since this is a metapackage, we have not included any examples as these are better suited for their respective packages.

## Test environments

### Previous R versions
* Ubuntu 16.04       (travis-ci), R 3.5.3
* Windows             (appveyor), R 3.5.3
* Windows          (win-builder), R 3.5.3

### Current R versions
* macOS Mojave       (travis-ci), R 3.6.1
* macOS Mojave           (local), R 3.6.1
* Ubuntu 16.04       (travis-ci), R 3.6.1
* Ubuntu 18.04           (local), R 3.6.1
* Windows             (appveyor), R 3.6.1
* Windows          (win-builder), R 3.6.1
* Windows 7              (local), R 3.6.1

### Development R version
* Ubuntu 16.04       (travis-ci), R 3.7.0 (2019-07-29 r76904)
* Ubuntu 18.04           (local), R 3.7.0 (2019-08-02 r76911)
* Windows             (appveyor), R 3.7.0 (2019-07-26 r76894)
* Windows          (win-builder), R 3.7.0 (2019-07-05 r76784)

## R CMD check results

There were no ERRORs or WARNINGs

There was 1 NOTE:

1. Some words were flagged as possibly mispelled, but they are false positives:

        Possibly mis-spelled words in DESCRIPTION:
          DES (8:77)
          Matloff (9:9)
          Metapackage (5:14)
          modularity (10:32)
          rasters (12:73)

## Downstream dependencies

There are currently no downstream dependencies of this package.
