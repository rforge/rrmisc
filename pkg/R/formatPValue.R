# ==================================================================================================
# --------------- formatPValaue                     format p-values                               --{{{
# RR 20130920     ------------------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#' @title Format p-values
#'
#' @description Format p-values
#'
#' @details utility function for formating
#'
#' @param x p-value
#' @param digits number of decimal-digits
#' @param \dots arguments passed to further functions
#' @return formatted string
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso other utility-functions in this R-package
#' @references none
#' @examples
#'     formatPValue(0.23478,     digits=3)
#'     formatPValue(0.023478,    digits=3)
#'     formatPValue(0.0023478,   digits=3)
#'     formatPValue(0.00023478,  digits=3)
#'     formatPValue(0.000023478, digits=3)
#'     formatPValue(0.000023478, digits=4)
#'     formatPValue(0.000023478, digits=5)
#'     formatPValue(0.000023478, digits=6)
#'
#'     x <- c(seq(0.0001, 0.6, 0.01), NA, seq(0.6001, 1.0, 0.01))
#'     formatPValue(x, digits=3)
#' @export
formatPValue <- function(x, digits = 3, ...) {
    #
    # method                                        ............................................. ..
    # -
    #
    # input                                         ............................................. ..
    # - numeric p-value
    # - number of digits to format to
    #
    # output                                        ............................................. ..
    # - formatted character string
    #
    # digits <- 3
    #
    if(length(x)==0)
    {
        return("--")
    } else {
        if (is.numeric(x))
        {
            lev_min <- 10^(-digits)
            i_small <- x < lev_min          # index for small values
            i_na    <- is.na(x)             # index for NAs
            pValue  <- sprintf(paste("%.", digits, "f", sep = ""), x)

            pValue[i_small] <- paste("<", lev_min)
            pValue[i_na]    <- "NA"

            return(pValue)
        }
        else {
            return("--")
        }
    }
    #
}
# END OF FUNCTION  ----------------------------------------------------------------------------- --
# --------------- formatPValue  ----------------------------------------------------------------- --
## }}}
# --------------- encodeUTF8                        encode text to UTF-8                          --{{{
# RR 20200813     ------------------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#' @title Encode text to UTF-8
#'
#' @description Encode text to UTF-8
#'
#' @details utility function for formating
#'
#' @param df data.frame or character vector
#' @param \dots arguments passed to further functions
#' @return formatted object
#' @note maybe of most use under Windows-Environment
#' @author Roland Rapold
#' @seealso other utility-functions in this R-package
#' @references none
#' @examples
#' d.test <- data.frame(a = 1:10,
#'                      b = letters[1:10],
#'                      c = c("ä", "ö", "ü", "è", "à", "%", "&", "¢", "@", "¬"))
#' d.test
#' encodeUTF8(d.test)
#'
#' d.test <- c("ä", "ö", "ü", "è", "à", "%", "&", "¢", "@", "¬", "#")
#' d.test
#' encodeUTF8(d.test)
#' @export
encodeUTF8 <- function(df, ...) {
  #
  if ("data.frame" %in% class(df)) {  # Behandlung von 'data.frame' -- auch 'data.table' -----------
    #
    # Lokale Kopie erstellen und als 'data.frame' klassieren (nicht 'data.table') ..................
    ME <- df
    class(ME) <- "data.frame"
    #
    # Liste aller Textspalten auslesen .............................................................
    ColClasses <- sapply(ME, class)
    CharCols   <- which(ColClasses == "character")
    #
    # Codierung aller Textspalten und einfüllen in ursprüngliches Objekt ...........................
    for (CharCol.loc in CharCols)
    {
      x <- ME[, CharCol.loc]
      Encoding(x) <- "UTF-8"
      df[, CharCol.loc] <- x
    }
  #
  } else {  # Behandlung von Text-Vektoren ---------------------------------------------------------
    if (class(df) == "character") {
      Encoding(df) <- "UTF-8"
    }
  }
  #
  # Rückgabe ---------------------------------------------------------------------------------------
  return(df)
}
# END OF FUNCTION  ------------------------------------------------------------------------------ --
# --------------- encodeUTF8    ----------------------------------------------------------------- --
## }}}
# --------------- getContStat                       get statistics of contingency table           --{{{
# RR 20200817     ------------------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#' @title Get statistics of contingency table
#'
#' @description Get statistics of contingency table
#'
#' @details utility function for analyzing contingency tables
#'
#' @param d.crosstab contincency table in class 'table
#' @param switchPosNeg switching positive and negative classes
#' @param \dots arguments passed to further functions
#' @return statistics of contingency table
#' @note to be refined
#' @author Roland Rapold
#' @seealso other utility-functions in this R-package
#' @references none
#' @examples
#' d.cont <- matrix(c(65.64, 1.95, 27.81, 4.6), ncol = 2, byrow = TRUE)
#' colnames(d.cont) <- c("true 0", "true 1")
#' rownames(d.cont) <- c("fit 0", "fit 1")
#' d.cont <- as.table(d.cont)
#' getContStat(d.cont)
#' @export
#
getContStat <- function(d.crosstab, switchPosNeg = FALSE, ...) {
  # Fawcett_2003, Sing 2005 (and ROCR documentation)
  # siehe auch Wikipedia
  #
  # true classes                 |             p          |            n           |
  # ^^^^^^^^^^^^                -|------------------------|------------------------|-
  #                            Y |  True Positives (TP)   |  False Positives (FP)  |
  # hypotzesized classes        -|------------------------|------------------------|-
  # ^^^^^^^^^^^^^^^^^^^^       N |  False Negatives (FN)  |  True Negatives (TN)   |
  #                             -|------------------------|------------------------|-
  # column totals                              P                       N
  # ^^^^^^^^^^^^^                              ^                       ^
  #
  # Umkehren von Positiven und Negativen Kennungen in Spalten und Reihen ...........................
  if (switchPosNeg) {
    temp.d.crosstab <- d.crosstab
    d.crosstab[2, 2] <- temp.d.crosstab[1, 1]
    d.crosstab[1, 1] <- temp.d.crosstab[2, 2]
    d.crosstab[1, 2] <- temp.d.crosstab[2, 1]
    d.crosstab[2, 1] <- temp.d.crosstab[1, 2]
    colnames(d.crosstab) <- rev(colnames(temp.d.crosstab))
    rownames(d.crosstab) <- rev(rownames(temp.d.crosstab))
    rm(temp.d.crosstab)
  }
  #
  # Berechnung der Kennzahlen ......................................................................
  TP <- d.crosstab[1, 1] # True Positives
  TN <- d.crosstab[2, 2] # True Negatives
  FP <- d.crosstab[1, 2] # False Positives
  FN <- d.crosstab[2, 1] # False Negatives

  P <- TP + FN # sum of true positives
  N <- TN + FP # sum of true negatives

  TP_rate <- TP / P # True positive rate -- Hit rate
  TN_rate <- TN / N # True negative rate
  FP_rate <- FP / N # False positive rate -- Type I error
  FN_rate <- FN / P # False negative rate -- Type II error

  sensitivity <- TP_rate
  specificity <- TN_rate

  accuracy   <- ( TP + TN ) / ( P + N )   # Korrektklassifikationsrate
  error_rate <- ( FP + FN ) / ( P + N )   # Falschklassifikationsrate
  precision  <- TP / ( TP + FP )          # Genauigkeit
  recall     <- TP_rate                   # Trefferquote
  ppv        <- precision                 # positive predictive value
  npv        <- TN / ( TN + FN )          # negative predictive value

  Phi <- ( TP * TN - FP * FN ) / sqrt( ( TP + FN ) * ( TN + FP ) * ( TP + FP ) * ( TN + FN ) )
  # Yields a number between -1 and 1, with 1 indicating a perfect prediction, 0 indicating a random
  # prediction. Values below 0 indicate a worse than random prediction.

  f_score <- 2 * (precision * recall) / (precision + recall)
  #
  # Ausgabe der Kennzahlen .........................................................................
  print(d.crosstab)
  print(">>> True positives in position [1, 1] <<<")
  print(sprintf("%-39s %.3f%%", "true positive rate (sensitivity)", TP_rate*100))
  print(sprintf("%-39s %.3f%%", "true negative rate (specificity)", TN_rate*100))
  print(sprintf("%-39s %.3f%%", "false positive rate (type I error)", FP_rate*100))
  print(sprintf("%-39s %.3f%%", "false negative rate (type II error)", FN_rate*100))
  print(sprintf("%-39s %.3f%%", "positive predictive value (ppv)", ppv*100))
  print(sprintf("%-39s %.3f%%", "negative predictive value (npv)", npv*100))
  print(sprintf("%-39s %.3f%%", "error rate", error_rate*100))
  print(sprintf("%-39s %.3f", "correlation (phi)", Phi))
  print(sprintf("%-39s %.3f%%", "F-value", f_score*100))

  return(NULL)
}
# END OF FUNCTION  ------------------------------------------------------------------------------ --
# --------------- getContStat ------------------------------------------------------------------- --
#}}}
# ==================================================================================================
