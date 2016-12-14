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
# --------------------------------------------------------------------------------------------------

# setwd("/home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/pkg")
# getwd()
# roxygen2::roxygenise()

# --------------------------------------------------------------------------------------------------
# system("gv /home/roland/Statistik/R_rrMisc/rrmisc/rrMisc.Rcheck/rrMisc-manual.pdf &")

# --------------------------------------------------------------------------------------------------

# version <- "0.23"
# detach("package:rrMisc", character.only=TRUE)
# unloadNamespace("rrMisc")
# remove.packages("rrMisc")
# install.packages(paste0("/home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/rrMisc_",
#                         version, ".tar.gz"),
#                  repos=NULL)
# library(rrMisc)
#
# install.packages("rrMisc", repos="http://R-Forge.R-project.org")
# library(rrMisc)
#
# --------------------------------------------------------------------------------------------------
