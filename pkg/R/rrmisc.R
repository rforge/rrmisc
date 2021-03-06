#' @title Sample data
#' @name d.sport
#' @description sport measures from 15 athletes

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
#' @import stats
#' @import graphics
#' @import grDevices
#' @import lattice
#' @import data.table
#' @name rrMisc-package
# #' @aliases rrMisc-package rrMisc
# #' @docType package
# #' @author Author and Maintainer: roland.rapold@@alumni.ethz.ch
# #' @seealso other functions in this R-packge
# #' @keywords package
# "_PACKAGE"
NULL


# --------------------------------------------------------------------------------------------------
# Administrative Schritte zur Erstellung des Paketes
# ===>                                                                        <===
# ===> Achtung -- Verwendung von Computer mit aktueller R-Version -- 'backup' <===
# ===>                                                                        <===
# --------------------------------------------------------------------------------------------------
# Verwendung von Roxygen zur automatischen Erstellung der Hilfe-Dateien ............................
#
# -->> Löschen der Hilfe-Dateien bevor sie neu erzeugt werden durch roxygen2
if (0 == 1) {
  # Laptop lokal
  setwd("/home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/pkg")
  # Zentral You
  setwd("~/R_rrMisc/rrmisc/pkg")
  setwd("~/tt/rrmisc/pkg")
  #
  getwd()
  list.files()
  unlink("./man/*.Rd")
  #
  library(roxygen2)
  roxygen2::roxygenise()
}
#
# --------------------------------------------------------------------------------------------------
# Paket kompilieren ................................................................................
#
# System-Dateien aktualisieren
# -- 'DESCRIPTION'    Paketversion, Datum
# -- 'NEWS'           geänderte Bausteine
if (0 == 1) {
  rrMiscVers <- "0.48"
  # Laptop lokal
  setwd("/home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/")
  # Zentral You
  setwd("~/R_rrMisc/rrmisc/")
  setwd("~/tt/rrmisc/")
  #
  getwd()
  system(       "R --vanilla CMD build pkg")
  system(paste0("R --vanilla CMD check rrMisc_", rrMiscVers, ".tar.gz"))

  # wenn Probleme mit Erstellung von PDF-Datei, ev.
  # apt install texinfo
  # apt install texlive-fonts-extra

  list.files("rrMisc.Rcheck")
  # system(paste0("gvim /home/roland/Desktop/Dokumente_verteilt/Statistik/",
  #               "R_rrMisc/rrmisc/rrMisc.Rcheck/00check.log &"))
  system("gvim ~/R_rrMisc/rrmisc/rrMisc.Rcheck/00check.log &")
  # system(paste0("gvim /home/roland/Desktop/Dokumente_verteilt/Statistik/",
  #               "R_rrMisc/rrmisc/rrMisc.Rcheck/00install.out &"))
  system("gvim ~/R_rrMisc/rrmisc/rrMisc.Rcheck/00install.out &")

  # system(paste0("gv /home/roland/Desktop/Dokumente_verteilt/Statistik/",
  #                "R_rrMisc/rrmisc/rrMisc.Rcheck/rrMisc-manual.pdf &"))
  system("gv ~/R_rrMisc/rrmisc/rrMisc.Rcheck/rrMisc-manual.pdf &")
}
#
# --------------------------------------------------------------------------------------------------
# Entfernen der alten Version und installieren der neuen Paket-Version .............................
#
if (0 == 1) {
  # setwd("/home/roland/Desktop/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/")
  setwd("~/R_rrMisc/rrmisc/")
  getwd()
  #
  # ist neue Version vorhanden?
  list.files()
  #
  # alte, installierte Version entfernen
  detach("package:rrMisc", character.only = TRUE)
  unloadNamespace("rrMisc")
  remove.packages("rrMisc")
  #
  # Installation der lokalen Paket-Datei
  rrMiscVers <- "0.4.6"
  install.packages(paste0("~/R_rrMisc/rrmisc/rrMisc_", rrMiscVers, ".tar.gz"), repos = NULL)

  # Installation von R-Forge.R-project.org
  install.packages("rrMisc", repos = "http://R-Forge.R-project.org")

  # neue Installation testen
  library(rrMisc)
  profileDataShiny()
  news(package = "rrMisc")
  ??rrMisc
  example(testGranularity)
  example(getContStat)
  example(encodeUTF8)
  ?encodeUTF8

  profileData()

  news(package = "rrMisc")
  # news(package = "callr")
  # news(package = "Hmisc")
}
#
# --------------------------------------------------------------------------------------------------
# Send to r-forge.r-project.org via SVN ............................................................
#
# --> Im Terminal                                     <--
#
# --> bei neuen Funktionen die Hilfe-Datei hinzufügen <--
# cd /home/roland/Statistik/R_rrMisc/rrmisc/pkg/man   <- Achtung korrektes Verzeichnis
# cd ~/R_rrMisc/rrmisc/pkg/man                        <- Achtung korrektes Verzeichnis
# svn add encodeUTF8.Rd
#
# --> Änderungen commiten       << pw: 'r-forge'      <--
# cd /home/roland/Statistik/R_rrMisc/rrmisc           <- Achtung korrektes Verzeichnis
# cd ~/R_rrMisc/rrmisc                                <- Achtung korrektes Verzeichnis
# svn status
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
# svn update            << git pull
#
# cd /home/roland/Statistik/R_rrMisc/rrmisc/pkg/man
# svn delete descrTable.R
# svn delete descrTable.Rd
# svn commit            << pw: 'r-forge'
#
# --------------------------------------------------------------------------------------------------
