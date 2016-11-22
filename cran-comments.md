## Updated release

This is an update to fix unintentional writes to a user's home directory.
Previously, a default path to a directory in the user's home folder was set via an option.
This has been changed to use a temporary directory by default instead, with a package startup message telling the user how to change it.

## Test environments

### Previous R versions
* Ubuntu 12.04        (travis-ci), R 3.2.5
* Windows              (appveyor), R 3.2.2, 3.2.5
* Windows 7               (local), R 3.2.2, 3.2.3, 3.2.4, 3.2.5

### Current R versions
* OS X El Capitan      (local), R 3.3.0
* OS X El Capitan  (travis-ci), R 3.3.1
* Ubuntu 12.04     (travis-ci), R 3.3.1
* Ubuntu 14.04     (travis-ci), R 3.3.1
* Ubuntu 16.04         (local), R 3.3.1
* Windows           (appveyor), R 3.3.1
* Windows        (win-builder), R 3.3.1
* Windows 7            (local), R 3.3.0, 3.3.1

### Development R version
* Debian:testing  (rocker/drd), R 3.4.0 (2016-11-20 r71670)
* Ubuntu 12.04     (travis-ci), R 3.4.0 (2016-11-18 r71668)
* Windows           (appveyor), R 3.4.0 (2016-11-17 r71664)
* Windows        (win-builder), R 3.4.0 (2016-10-07 r71462)

## R CMD check results

There were no ERRORs or WARNINGs

There were 2 NOTEs:

1. There are multiple parts to this note:

    a. Several words were flagged as possibly mispelled, but they are not.
    
            Possibly mis-spelled words in DESCRIPTION:
              DES (8:23)
              fastshp (12:24)
              modularity (8:55)
              repos (12:34)

    b. The `fastshp` package in Suggests is optionally installed from Rforge and not required to use the package. Instructions for installation are provided in the Description, README, and via a message to the user. We believe this should satisfy the CRAN policy requirement regarding additional dependencies.

            Suggests or Enhances not in mainstream repositories:
              fastshp
          
            Availability using Additional_repositories specification:
              fastshp   yes   http://rforge.net

2. As noted above, we provide instructions for the user to install the suggested `fastshp` package.

        Package suggested but not available for checking: 'fastshp'

## Downstream dependencies

There are currently no downstream dependencies of this package.
