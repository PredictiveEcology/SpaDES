## Maintainance 

In this version we:

* update maintainer's and authors' email addresses
* fix several bugs from the previous release

## Test environments

### Previous R versions
* Windows 7               (local), R 3.1.2
* Windows 7               (local), R 3.1.3
* Windows 7               (local), R 3.2.0
* Windows 7               (local), R 3.2.1

### Current R versions
* Debian Jessie           (local), R 3.2.2
* OS X Yosemite           (local), R 3.2.2
* Ubuntu 12.04        (travis-ci), R 3.2.2
* Ubuntu 14.04            (local), R 3.2.2
* Windows 7               (local), R 3.2.2
* Windows           (win-builder), R 3.2.2

### Development R version
* Debian:testing (rocker/r-devel), R 3.3.0 (2015-08-26 r69190)
* Windows           (win-builder), R 3.3.0 (2015-09-01 r69241)

## R CMD check results

There were no ERRORs or WARNINGs

There was 1 NOTE:

1. There are three parts to this note:

    a. The maintainer's email address has changed.
      
        Maintainer: 'Alex M Chubaty <alexander.chubaty@canada.ca>'
        
        New maintainer:
          Alex M Chubaty <alexander.chubaty@canada.ca>
        Old maintainer(s):
          Alex M Chubaty <achubaty@NRCan.gc.ca>


    b. Several words were flagged as possibly mispelled, but they are not.
    
        Possibly mis-spelled words in DESCRIPTION:
          fastshp (11:61, 12:39)
          modularity (9:5)
          repos (12:49)

    c. The `fastshp` package in Suggests is optionally installed from Rforge and not required to use the package. Instructions for installation are provided in the Description, README, and via a message to the user. We believe this should satisfy the CRAN policy requirement regarding additional dependencies.

        Suggests or Enhances not in mainstream repositories:
          fastshp
        
        Availability using Additional_repositories specification:
          fastshp   yes   http://rforge.net

## Downstream dependencies

There are currently no downstream dependencies of this package.
