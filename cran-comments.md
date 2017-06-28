## Updated release

This is a susbstantial update to the package, which has seen many enhancements and improvements since the previous release.
The size of the package had grown too large, so we have split the package into several, and have turned this package into a metapackage that, for backwards compatibility, attaches the new spinoff packages.

We have dropped support of R 3.2.x, following the changes to the `RandomFieldsUtils` package.

## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.3.3
* Windows              (appveyor), R 3.3.3
* Windows 7               (local), R 3.3.3

### Current R versions
* macOS Sierra         (local), R 3.4.0
* OS X El Capitan  (travis-ci), R 3.4.0
* Ubuntu 14.04     (travis-ci), R 3.4.0
* Ubuntu 16.04         (local), R 3.4.0
* Windows           (appveyor), R 3.4.0
* Windows        (win-builder), R 3.4.0
* Windows 7            (local), R 3.4.0

### Development R version
* Debian:testing  (rocker/drd), R 3.4.0 (2017-06-26 r72857)
* Ubuntu 14.04     (travis-ci), R 3.4.0 (2017-06-26 r72857)
* Windows           (appveyor), R 3.4.0 (2017-06-26 r72857)
* Windows        (win-builder), R 3.4.0 (2017-06-26 r72857)

## R CMD check results

There were no ERRORs or WARNINGs

There were 2 NOTEs:

1. There are multiple parts to this note:

    a. Some words were flagged as possibly mispelled, but they are not. 
     
            Possibly mis-spelled words in DESCRIPTION: 
              DES (8:23) 
              modularity (8:55) 

    b. The `fastshp` package in Suggests is optionally installed from Rforge and not required to use the package. Instructions for installation are provided in the Description, README, and via a message to the user. We believe this should satisfy the CRAN policy requirement regarding additional dependencies.

            Suggests or Enhances not in mainstream repositories:
              fastshp
          
            Availability using Additional_repositories specification:
              fastshp   yes   http://rforge.net

2. As noted above, we provide instructions for the user to install the suggested `fastshp` package.

        Package suggested but not available for checking: 'fastshp'

## Downstream dependencies

There are currently no downstream dependencies of this package.
