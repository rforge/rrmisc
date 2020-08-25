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
# --------------- pFromT                            p-value from t-value                          ..{{{
# RR 20150109     ------------------------------------------------------------------------------- --
# Manual          ------------------------------------------------------------------------------- --
#' @title       Calculate p-values from t-statistics.
#' @description Calculate p-values from t-statistics.
#' @details utility function for conversion of p-values
#' @param t_value The t-statistics to be converted into a p-Value.
#' @param df Degree of freedom in t-statistics.
#' @param two.sided Indicator of the statistics be calculatede as two-sided (TURE) or one-sided (FALSE).
#' @return corresponding p-Value from the t-, z- or F-statistics
#' @author Roland Rapold
#' @keywords p-value
#' @seealso \code{Link{pFromZ}} and \code{Link{pFromF}}
#' @examples
#'         pFromT(t_value=1.96, df=10)
#'         pFromT(t_value=1.96, df=100)
#'         pFromT(t_value=1.96, df=1000)
#'         pFromT(t_value=1.96, df=1000, two.sided = TRUE)
#'
#'         x <- c(seq(0.0001, 0.6, 0.01), NA, seq(0.6001, 1.0, 0.01))
#'         p_value <- pFromT(t_value=x, df=100)
#'         p_value
#'         formatPValue(p_value, digits=3)
#' @export
pFromT <- function(t_value=1.96, df=10, two.sided=FALSE)
{
    #
    # P-Wert aus t-Wert:
    #
    ifelse(two.sided, factor <- 2, factor <- 1)
    t_value <- abs(t_value)
    #
    p_value <- factor * stats::pt(t_value, df=df, lower.tail=FALSE)
    return(p_value)
    #
}
# --------------- pFromT --------------------------------------------
# ENDE DER FUNKTION -------------------------------------------------
# # }}}
# --------------- pFromZ                            p-value from z-value (normal distribution)    ..{{{
# RR 20150109     ------------------------------------------------------------------------------- --
# Manual          ------------------------------------------------------------------------------- --
#' @title       Calculate p-values from z-statistics.
#' @description Calculate p-values from z-statistics.
#' @details utility function for conversion of p-values
#' @param z_value The z-statistics to be converted into a p-Value.
#' @param two.sided Indicator of the statistics be calculatede as two-sided (TURE) or one-sided (FALSE).
#' @return corresponding p-Value from the t-, z- or F-statistics
#' @author Roland Rapold
#' @keywords p-value
#' @seealso \code{Link{pFromT}} and \code{Link{pFromF}}
#' @examples
#'         pFromF(f_value=35.68, df1=2,  df2=5)
#'         pFromF(f_value=35.68, df1=2,  df2=5, two.sided=TRUE)
#'         pFromF(f_value= 1.96, df1=10, df2=10)
#'         pFromF(f_value= 1.96, df1=10, df2=100)
#'         pFromF(f_value= 1.96, df1=10, df2=1000)
#'         pFromF(f_value= 1.96, df1=10, df2=1000, two.sided=TRUE)
#'         pFromF(f_value= 1.96, df1=10, df2=1000, two.sided=TRUE)
#' @export
pFromZ <- function(z_value=1.96, two.sided=FALSE)
{
    #
    # P-Wert aus z-Wert:
    #
    ifelse(two.sided, factor <- 2, factor <- 1)
    z_value <- abs(z_value)
    #
    p_value <- factor * stats::pnorm(z_value, lower.tail=FALSE)
    return(p_value)
    #
}
# --------------- pFromT --------------------------------------------
# ENDE DER FUNKTION -------------------------------------------------
# # }}}
# --------------- pFromF                            p-value from F-value                          ..{{{
# RR 20150109     ------------------------------------------------------------------------------- --
# Manual          ------------------------------------------------------------------------------- --
#' @title       Calculate p-values from F-statistics.
#' @description Calculate p-values from F-statistics.
#' @details utility function for conversion of p-values
#' @param f_value The f-statistics to be converted into a p-Value.
#' @param df1,df2 Degrees of freedom in F-statistics.
#' @param two.sided Indicator of the statistics be calculatede as two-sided (TURE) or one-sided (FALSE).
#' @return corresponding p-Value from the t-, z- or F-statistics
#' @author Roland Rapold
#' @keywords p-value
#' @seealso \code{Link{pFromT}} and \code{Link{pFromZ}}
#' @examples
#'         pFromF(f_value=35.68, df1=2,  df2=5)
#'         pFromF(f_value=35.68, df1=2,  df2=5, two.sided=TRUE)
#'         pFromF(f_value= 1.96, df1=10, df2=10)
#'         pFromF(f_value= 1.96, df1=10, df2=100)
#'         pFromF(f_value= 1.96, df1=10, df2=1000)
#'         pFromF(f_value= 1.96, df1=10, df2=1000, two.sided=TRUE)
#'         pFromF(f_value= 1.96, df1=10, df2=1000, two.sided=TRUE)
#' @export
pFromF <- function(f_value=1.96, df1=10, df2=10, two.sided=FALSE)
{
    #
    # P-Wert aus F-Wert:
    #
    ifelse(two.sided, factor <- 2, factor <- 1)
    f_value <- abs(f_value)
    #
    p_value <- factor * stats::pf(f_value, df1=df1, df2=df2, lower.tail=FALSE)
    return(p_value)
    #
    # in analysis of variance table:
    # df_1      = df of variable
    # df_2      = df_res = df_tot - df1             ...
    # df_tot    = # measurements - 1                ...
    #
}
# --------------- pFromF --------------------------------------------
# ENDE DER FUNKTION -------------------------------------------------
# # }}}
# --------------- encodeUTF8                        encode text to UTF-8                          ..{{{
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
# ==================================================================================================
