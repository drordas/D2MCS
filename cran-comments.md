## Resubmission
This is a resubmission. In this version I have:
* Ensured no more than 2 cores are used in the examples, vignettes, etc. 
* Ensured that the functions of examples/vignettes/tests do not write in the user's home filespace.
* Removed the use of the installed.packages() function.
* Changed the process to install the packages of the models to be trained.
* Added the CITATION file but the articles have not been included in the description field of DESCRIPTION file because they are not associated with any method in the package.

## Test environments
* Ubuntu 20.04.2 LTS (on local and travis-ci), R 4.0.5
* local Windows 10 Pro, R 4.0.5
* win-builder (devel, release)
* r-hub:
  * macOS 10.13.6 High Sierra, R-release, brew
  * macOS 10.13.6 High Sierra, R-release, CRAN's setup
  * Debian Linux, R-release, GCC
  * Ubuntu Linux 20.04.1 LTS, R-release, GCC

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
