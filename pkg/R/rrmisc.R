#' sample data
#' 
#' sport measures from 15 athletes
#' 
#' 
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
#' 
#'     data(d.sport)
#'     print(d.sport)
#' 
NULL

#' Some R-helpers
#' 
#' Some helpers of R to process data smoothly
#' 
#' The main function are the 'grep' functions for quickly accessing information of data objects and
#' the descrTable() function to generate descriptive statistics of a data object.
#' 
#' @name rrMisc-package
#' @import latticeExtra
#' 
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
# setwd("/home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/pkg")
# getwd()
# roxygen2::roxygenise()
#
# --------------------------------------------------------------------------------------------------
# Compile package ..................................................................................
#
# rrMiscVers <- "0.25"
# setwd("/home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/")
# getwd()
# system(       "R --vanilla CMD build pkg")
# system(paste0("R --vanilla CMD check rrMisc_", rrMiscVers, ".tar.gz"))
#
# system("gv /home/roland/Statistik/R_rrMisc/rrmisc/rrMisc.Rcheck/rrMisc-manual.pdf &")
#
# --------------------------------------------------------------------------------------------------
# Remove old version and install new package .......................................................
#
# rrMiscVers <- "0.26"
# detach("package:rrMisc", character.only=TRUE)
# unloadNamespace("rrMisc")
# remove.packages("rrMisc")
#
# -- install local copy of package
# install.packages(paste0("/home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/rrMisc_",
#                         rrMiscVers, ".tar.gz"),
#                  repos=NULL)
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
# svn commit
#
# svn log
# svn status
# svn add pkg/file.R
# svn update            (can help if problems occur)
#
# --------------------------------------------------------------------------------------------------
