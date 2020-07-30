#' @title sample data
#' @description sport measures from 15 athletes
#' @name d.sport
#' @docType data
#' @format
#' \itemize{
#'  \item weit
#'  \item kugel
#'  \item hoch
#'  \item disc
#'  \item stab
#'  \item speer
#'  \item punkte
#' }
#' @references "http://stat.ethz.ch/Teaching/Datasets/WBL/sport.dat" accessed 2016-06-03
#' @keywords data
#' @examples
#'     data(d.sport)
#'     print(d.sport)
NULL

#' @title Some R-helpers
#' @description Some helpers of R to process data smoothly
#' @details The main function are the 'grep' functions for quickly accessing information of data
#' objects and the descrTable() function to generate descriptive statistics of a data object.
#' @name rrMisc-package
#' @import latticeExtra
# #' @aliases rrMisc-package rrMisc
# #' @docType package
# #' @author Author and Maintainer: roland.rapold@@alumni.ethz.ch
# #' @seealso other functions in this R-packge
# #' @keywords package
# "_PACKAGE"
NULL


# --------------------------------------------------------------------------------------------------
# Administrative tasks for creating the package
#
# --------------------------------------------------------------------------------------------------
# Use Roxygen to create/update the documentation files .............................................
#
# -->> delete help-files so that they can be rebuilt by roxygen2
if (0 == 1) {
  # set path to Code
  setwd("/home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/pkg")
  getwd()
  #
  # delete help-files so that they can be rebuilt by roxygen2
  unlink("./man/*.Rd")
  # .libPaths()
  # getOption("repos")
  # install.packages("roxygen2", dependencies = TRUE)
  #
  library(roxygen2)
  roxygen2::roxygenise()
}
#
# --------------------------------------------------------------------------------------------------
# Compile package ..................................................................................
#
# -- adjust the version number and date in the 'DESCRIPTION' file
# -- update NEWS file
# rrMiscVers <- "0.33"
# setwd("/home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/")
# getwd()
# system(       "R --vanilla CMD build pkg")
# system(paste0("R --vanilla CMD check rrMisc_", rrMiscVers, ".tar.gz"))
# system("gvim /home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/rrMisc.Rcheck/00check.log &")
# system("gvim /home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/rrMisc.Rcheck/00install.out &")
#
# system("gv /home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/rrMisc.Rcheck/rrMisc-manual.pdf &")
#
# --------------------------------------------------------------------------------------------------
# Remove old version and install new package .......................................................
#
# detach("package:rrMisc", character.only=TRUE)
# unloadNamespace("rrMisc")
# remove.packages("rrMisc")
#
# -- install local copy of package
# rrMiscVers <- "0.33"
# install.packages(paste0("/home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/rrMisc_",
#                         rrMiscVers, ".tar.gz"), repos=NULL)
# library(rrMisc)
#
# -- install copy from r-forge
# install.packages("rrMisc", repos="http://R-Forge.R-project.org")
#
# -- test installation
# library(rrMisc)
# ??rrMisc
# ?cols7.theme
# ?div01.theme
#
# --------------------------------------------------------------------------------------------------
# Send to r-forge.r-project.org via SVN ............................................................
# --> change to promt
# cd /home/roland/Statistik/R_rrMisc/rrmisc
# svn commit            << pw: 'r-forge'
#
# svn log
# svn log -v -r HEAD:40 << commit 40 to last commit
# svn status
# svn add pkg/file.R    << only to add new files to version control
# svn update            (can help if problems occur)
#
# --------------------------------------------------------------------------------------------------
