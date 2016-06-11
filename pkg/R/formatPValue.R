# --------------- formatPValaue                     format p-values                               --
# RR 20130920     ------------------------------------------------------------------------------- --
#
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
#
