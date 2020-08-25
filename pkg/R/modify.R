# ==================================================================================================
# --------------- naToZero                       Ersetzen von NAs                                 ..{{{
# RR 20150109     ------------------------------------------------------------------------------- --
# Manual          ------------------------------------------------------------------------------- --
#' @title       Substitution of NAs in data.frame or vector
#' @description Substitution of NAs in a data.frame or a vector. NAs in numeric fields (integer,
#' numeric) are substituted by 0, NAs in caracter fields are substituted by '-'.
#' @details utility function for treating NAs
#' @param dataf data.frame or vector
#' @param substit_numeric shall numeric fields (integer, numerical) be substituted?
#' @param substit_character shall character fields be substituted?
#' @param \dots arguments passed to further functions
#' @return corresponding data.frame or vector with substituted NAs
#' @author Roland Rapold
#' @keywords NA
#' @examples
#'         (x <- c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6))
#'         naToZero(x)
#'         naToZero(x, substit_numeric=TRUE)
#'         naToZero(x, substit_numeric=FALSE)
#'
#'         (x <- c("pi", NA, "4.000000", "5.000000", "5", "6", "5", "6"))
#'         naToZero(x)
#'         naToZero(x, substit_character=TRUE)
#'         naToZero(x, substit_character=FALSE)
#' @export
naToZero <- function(dataf, substit_numeric=TRUE, substit_character=FALSE, ...)
{
    if(is.data.frame(dataf))
    {
        for(i in 1:ncol(dataf))
        {
            dataf_i    <- dataf[, i]
            dataf_i_cl <- class(dataf_i)
            switch(dataf_i_cl
                 , "integer"= {
                        if(substit_numeric==TRUE)
                        {
                            dataf_i[is.na(dataf_i)] <- 0
                            dataf[, i] <- dataf_i
                        }
                    }
                 , "numeric"= {
                        if(substit_numeric==TRUE)
                        {
                            dataf_i[is.na(dataf_i)] <- 0
                            dataf[, i] <- dataf_i
                        }
                    }
                 , "character"= {
                        if(substit_character==TRUE)
                        {
                            dataf_i[is.na(dataf_i)] <- "-"
                            dataf[, i] <- dataf_i
                        }
                    })
            rm(dataf_i)
        }
        return(dataf)
    } else {
        dataf_cl <- class(dataf)
        switch(dataf_cl
             , "integer"= {
                    if(substit_numeric==TRUE)
                    {
                        dataf[is.na(dataf)] <- 0
                    }
                    return(dataf)
                }
             , "numeric"= {
                    if(substit_numeric==TRUE)
                    {
                        dataf[is.na(dataf)] <- 0
                    }
                    return(dataf)
                }
             , "character"= {
                    if(substit_character==TRUE)
                    {
                        dataf[is.na(dataf)] <- "-"
                    }
                    return(dataf)
                })
        warning("naToZero: please input a data.frame or a numeric vector")
        return(dataf)
    }
}
# --------------- naToZero ------------------------------------------
# ENDE DER FUNKTION -------------------------------------------------
# # }}}
# --------------- classifyByBreaks               bilde Gruppen/Klassen nach Klassengrenzen        ..{{{
# RR 20140808               ------------------------------------------
# Manual          ------------------------------------------------------------------------------- --
#' @title       Group numerical vector (enhanced cut())
#' @description Group numerical vector and allow for group with zero values as well as formating
#' @details utility function for better grouping numerical valures
#' @param x numieric vector
#' @param breaks first lower bound and all following upper bounds
#' @param i.class.zero indicator for a separate group vor zero values
#' @param format.level indicator for levels to be formatted
#' @param ordered.factor to be given to cut()
#' @return grouped factor vector
#' @author Roland Rapold
#' @keywords NA
#' @examples
#'  x <- as.integer(c(rep(0, 10), rep(1, 8), rep(2, 6), rep(3, 6), rep(4, 4), 5, 5, 5, 6, 6, 7, 8, 9))
#'  breaks <- c(0, 1, 2, 3, 5, 9)
#'
#'  y <- classifyByBreaks(x = x, breaks = breaks, format.level = FALSE)
#'  table(y, useNA = "always")
#'
#'  y <- classifyByBreaks(x = x, breaks = breaks, format.level = TRUE)
#'  table(y, useNA = "always")
#'
#'  y <- classifyByBreaks(x = x, breaks = breaks, i.class.zero = 1, format.level = FALSE)
#'  table(y, useNA = "always")
#'
#'  y <- classifyByBreaks(x = x, breaks = breaks, i.class.zero = 1, format.level = TRUE)
#'  table(y, useNA = "always")
#' @export
classifyByBreaks <- function(x, breaks, i.class.zero = FALSE, format.level = FALSE, ordered.factor = FALSE)
{
  #
  # Letzter Breakpoint kann beim Input weggelassen werden ..................................... ..
  if(length(which(breaks == max(x))) == 0) {
    breaks <- c(breaks, max(x))
  }
  #
  # Zusaetzliche Gruppe mit 0-Werten bilden                ..................................... ..
  if(i.class.zero == TRUE)
  {
    breaks <- unique(c(min(-0.001, min(x)), 0, breaks))
  }
  #
  # Unterteilung ausfuehren                                ..................................... ..
  y <- cut(x,
           breaks = breaks,
           include.lowest = TRUE,
           ordered_result = ordered.factor)
  # table(y, useNA = "always")
  if(i.class.zero == TRUE & min(x) >= 0) levels(y)[1] <- gsub("-0.001", "0", levels(y)[1])
  #
  # Faktorstufen formattieren                             ..................................... ..
  # format.level <- FALSE
  if (format.level)
  {
    # require(doBy)
    a <- unique(data.frame(x = x, y = y))
    # summary(x)
    # summary(y)
    # b <- as.data.frame(summaryBy(x~y, data = a, FUN = c(min, max)))

    b <- data.frame(y = unique(a$y),
                    x.min = tapply(a$x, a$y, FUN = min),
                    x.max = tapply(a$x, a$y, FUN = max))
    rownames(b) <- NULL

    b <- merge(data.frame(y = names(summary((y)))), b, all.x = TRUE)
    b <- b[order(b$y), ]
    b <- rbind(b[nrow(b), ], b[1:(nrow(b)-1), ])
    # b
    #
    # Zusätzliche Zeile wenn keine 0s vorkommen, aber als Gruppe erscheinen sollen .......... ..
    if(names(table(y)[1]) != "[0,0]" & i.class.zero == TRUE) {
      b[, "y"] <- as.character(b[, "y"])
      b        <- rbind(c("[0,0]", 0.0, 0.0), b)
    }
    b[, "x.min"] <- as.numeric(b[, "x.min"])
    b[, "x.max"] <- as.numeric(b[, "x.max"])
    b$x.min[is.na(b$x.min)] <- 0
    b$x.max[is.na(b$x.max)] <- 0
    # b
    #
    # Levels bilden                                     ..................................... ..
    range1 <- c(1:(nrow(b)-1))
    range2 <- nrow(b)
    digits = floor( log10( max(b[, "x.min"]) ) ) + 1
    b[range1, "level"] <- paste0(sprintf(paste("%0", digits, ".0f", sep = ""), b[range1, "x.min"]),
                                 "-",
                                 sprintf(paste("%0", digits, ".0f", sep = ""), b[range1, "x.max"]))
    b[range2, "level"] <- paste0(sprintf(paste("%0", digits, ".0f", sep = ""), b[range2, "x.min"]),
                                 "+",
                                 paste(rep(" ", digits), collapse = ""))
    # print(b)
    # print(levels(y))
    levels(y) <- b$level
    # table(y)
  }
  #
  # Rueckgabe Resultat                                     ..................................... ..
  return(y)
  #
}
#
# --------------- classifyByBreaks  -------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
# }}}
# --------------- classifyByQuantiles            bilde Gruppen/Klassen nach Quantilen             ..{{{
# RR 20140808               --------------------------------------------------------------------- --
# Manual          ------------------------------------------------------------------------------- --
#' @title       Group numerical vector (enhanced cut())
#' @description Group numerical vector and allow for group with zero values as well as formating
#' @details utility function for better grouping numerical valures
#' @param x numieric vector
#' @param n.main number of main groups
#' @param n.supp number of supplement groups - total size of which equals sizt of one main group
#' @param i.class.zero indicator for a separate group vor zero values
#' @param format.level indicator for levels to be formatted
#' @param ordered.factor to be given to cut()
#' @return grouped factor vector
#' @author Roland Rapold
#' @keywords NA
#' @examples
#'  data(swiss)
#'  summary(swiss$Agriculture)
#'  x            <- swiss$Agriculture
#'  x[c(3, 6, 9, 22)] <- NA
#'  n.main       <- 6
#'  n.supp       <- 2
#'  i.class.zero <- 1
#'  i.class.zero <- 0
#'  format.level <- TRUE
#'  y <- classifyByQuantiles(x = x, n.main = n.main, n.supp = n.supp,
#'                           i.class.zero = i.class.zero,
#'                           format.level = format.level)
#'  str(y)
#'  levels(y)
#'  table(y, useNA = "always")
#'  sum(table(y, useNA = "always"))
#'
#'
#'  format.level <- FALSE
#'  y <- classifyByQuantiles(x = x, n.main = n.main, n.supp = n.supp,
#'                           i.class.zero = i.class.zero,
#'                           format.level = format.level)
#'  str(y)
#'  levels(y)
#'  table(y, useNA = "always")
#' @export
#   --------------------------------------------------------------------------------------------- --
#
#   Numerische Werte in n.main+n.supp Gruppen aufteilen
#   Gruppen        1 - n.main               je 1/(n.main+1)      aller Beobachtungen
#   Gruppen n.main+1 - n.main + n.supp      je 1/(n.main*n.supp) aller Beobachtungen
#
#   D.h. zuerst werden die Beobachtungen in n.main+1 gleich grosse Gruppen aufgeteilt.
#   Danach wird die hoechste Gruppe nochmals in n.supp Gruppen unterteilt.
#
#   Resultat: n.main gleich grosse Gruppen und n.supp gleich grosse Gruppen, die zusammen gleich
#   gross sind wie eine der ersten n.main Gruppen.
#
#   Falls mehr Eintraege mit Werten von 0 vorhanden sind als 1/(n.main+1) der Beobachgungen, werden
#   diese in die erste Gruppe eingeteilt. Die Eintraege mit Werten groesser als 0 bilden die
#   restlichen Gruppen.
#
#   --------------------------------------------------------------------------------------------- --
classifyByQuantiles <- function(x, n.main = 10, n.supp = 4, i.class.zero = FALSE, format.level = FALSE,
                                ordered.factor = FALSE)
{
  # Testen NA in x                                          ..................................... ..
  id.x  <- which(!is.na(x))
  id.na <- which(is.na(x))
  x     <- x[id.x]
  #
  # n.supp = 0 ersetzen                                     ..................................... ..
  if(n.supp<1) n.supp <- 1
  #
  # Funktion fuer Wahrscheinlichkeiten für Quantile         ..................................... ..
  probs_f <- function(n.main, n.supp)
  {
    probs <- c(0,
               c(1:n.main) * 1 / (n.main+1),
               n.main / (n.main + 1) + c(1:n.supp) * 1 / ((n.main+1) * n.supp))
    return(probs)
  }
  #
  #
  # Quantile                                                ..................................... ..
  n.main_loc <- n.main
  probs      <- probs_f(n.main = n.main_loc, n.supp = n.supp)
  #
  # Werte der gegebenen Quantile berechnen                  ..................................... ..
  # summary(x)
  quants <- stats::quantile(x, probs = probs, na.rm = TRUE)
  breaks <- quants
  #
  # 'else' wenn mehrere Gruppen aus Werten = 0 bestehen     ..................................... ..
  if(length(quants) != length(unique(quants))) {
    i.class.zero <- TRUE
    # diff       <- length(quants) - length(unique(quants))
    # n.main_loc <- n.main - diff
    # probs      <- probs_f(n.main = n.main_loc, n.supp = n.supp)
    # breaks     <- c(quantile(x[x>0], probs = probs))
  }
  #
  # Werte der gegebenen Quantile berechnen                  ..................................... ..
  # für separate Gruppe von 0-Werten                        ..................................... ..
  if(i.class.zero == TRUE)
  {
    n.main_loc <- n.main - 1
    probs      <- probs_f(n.main = n.main_loc, n.supp = n.supp)
    quants     <- stats::quantile(x[x>0], probs = probs, na.rm = TRUE)
    breaks     <- c(0, quants)
    breaks[2]  <- breaks[2] - 0.001
  }
  #
  # Unterteilung ausführen                                  ..................................... ..
  breaks <- unique(breaks)
  y <- cut(x,
           breaks = breaks,
           include.lowest = TRUE,
           ordered_result = ordered.factor)
  # table(y, useNA = "always")
  #
  # Faktorstufen formattieren                               ..................................... ..
  if(format.level)
  {
    # require(doBy)
    a <- data.frame(x = x, y = y)
    a <- a[order(a$x), ]
    # summary(x)
    # summary(y)
    # b <- as.data.frame(summaryBy(x ~ y, data = a, FUN = c(min, max)))

    b <- data.frame(y = unique(a$y),
                    x.min = tapply(a$x, a$y, FUN = min),
                    x.max = tapply(a$x, a$y, FUN = max))
    rownames(b) <- NULL
    #
    # Zusaezliche Zeile wenn keine 0s vorkommen, aber als Gruppe erscheinen sollen
    if(table(y)[1] == 0 & i.class.zero == TRUE) {
      b[, "y"]     <- as.character(b[, "y"])
      b            <- rbind(c("0", 0.0, 0.0), b)
      b[, "x.min"] <- as.numeric(b[, "x.min"])
      b[, "x.max"] <- as.numeric(b[, "x.max"])
    }
    # b
    #
    # Levels bilden
    range1 <- c(1:(nrow(b)-1))
    range2 <- nrow(b)
    digits = floor( log10( max(b[, "x.min"]) ) ) + 1
    b[range1, "level"] <- paste0(sprintf(paste("%0", digits, ".0f", sep = ""), b[range1, "x.min"]),
                                 "-",
                                 sprintf(paste("%0", digits, ".0f", sep = ""), b[range1, "x.max"]))
    b[range2, "level"] <- paste0(sprintf(paste("%0", digits, ".0f", sep = ""), b[range2, "x.min"]),
                                 "+",
                                 paste(rep(" ", digits), collapse = ""))
    # print(b)
    # print(levels(y))
    levels(y) <- b$level
    # table(y)
  }
  #
  # Rückgabe Resultat                                       ..................................... ..
  yy        <- y
  yy[id.x]  <- y
  yy[id.na] <- NA
  return(yy)
  #
}
#
# --------------- classifyByQuantiles  -------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
# }}}
# ==================================================================================================
