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
#' objects.
#' @name rrMisc-package
#' @import latticeExtra
#' @import data.table
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
if (0 == 1) {
  rrMiscVers <- "0.36"
  setwd("/home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/")
  getwd()
  system(       "R --vanilla CMD build pkg")
  system(paste0("R --vanilla CMD check rrMisc_", rrMiscVers, ".tar.gz"))

  # wenn Probleme mit Erstellung von PDF-Datei, ev.
  # apt install texinfo
  # apt install texlive-fonts-extra

  list.files("rrMisc.Rcheck")
  system(paste0("gvim /home/roland/Desktop/Dokumente_verteilt/Statistik/",
                "R_rrMisc/rrmisc/rrMisc.Rcheck/00check.log &"))
  system(paste0("gvim /home/roland/Desktop/Dokumente_verteilt/Statistik/",
                "R_rrMisc/rrmisc/rrMisc.Rcheck/00install.out &"))

  system(paste0("gv /home/roland/Desktop/Dokumente_verteilt/Statistik/",
                 "R_rrMisc/rrmisc/rrMisc.Rcheck/rrMisc-manual.pdf &"))
}
#
# --------------------------------------------------------------------------------------------------
# Remove old version and install new package .......................................................
#
if (0 == 1) {
  setwd("/home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/")
  getwd()
  #
  # test if new version is available
  list.files()
  #
  # remove old version
  detach("package:rrMisc", character.only = TRUE)
  unloadNamespace("rrMisc")
  remove.packages("rrMisc")
  #
  # install local copy of package
  rrMiscVers <- "0.36"
  install.packages(paste0("/home/roland/Desktop/Dokumente_verteilt/Statistik/",
                          "R_rrMisc/rrmisc/rrMisc_", rrMiscVers, ".tar.gz"),
                   repos = NULL)
  library(rrMisc)
  example(testGranularity)
  example(getContStat)
  example(encodeUTF8)
}
#
if (0 == 1) {
  # -- install copy from r-forge
  install.packages("rrMisc", repos = "http://R-Forge.R-project.org")

  # -- test installation
  library(rrMisc)
  ??rrMisc
  example(getContStat)
  example(encodeUTF8)
  ?cols7.theme
  ?div01.theme
}
#
# --------------------------------------------------------------------------------------------------
# Send to r-forge.r-project.org via SVN ............................................................
#
# --> Im Terminal                                     <--
#
# --> bei neuen Funktionen die Hilfe-Datei hinzufügen <--
# cd /home/roland/Statistik/R_rrMisc/rrmisc/pkg/man   <- Achtung korrektes Verzeichnis
# svn add encodeUTF8.Rd
#
# --> Änderungen commiten       << pw: 'r-forge'      <--
# cd /home/roland/Statistik/R_rrMisc/rrmisc           <- Achtung korrektes Verzeichnis
# svn commit
#
#
#
#
# svn log
# svn log -v -r HEAD:40 << commit 40 to last commit
# svn status
# cd /home/roland/Statistik/R_rrMisc/rrmisc/pkg/R
# svn add file.R        << only to add new files to version control
# svn update            (can help if problems occur)
#
# cd /home/roland/Statistik/R_rrMisc/rrmisc/pkg/man
# svn delete descrTable.R
# svn delete descrTable.Rd
# svn commit            << pw: 'r-forge'
#
# --------------------------------------------------------------------------------------------------
