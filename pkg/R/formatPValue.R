# ==================================================================================================
# --------------- formatPValaue                     format p-values                               --# {{{
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
formatPValue <- function(x, digits=3, ...) {
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
# --------------- encodeUTF8                        encode text to UTF-8                          --# {{{
# RR 20200813     ------------------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#' @title Encode text to UTF-8
#'
#' @description Encode text to UTF-8
#'
#' @details utility function for formating
#'
#' @param df = data.frame or character vector
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
#' @export
encodeUTF8 <- function(df) {
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
    df <- 1:20
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
# ==================================================================================================
