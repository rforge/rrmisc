#
# ==================================================================================================
#
# --------------- percentLabel                      create percentage labels for plots            ..
# RR 20130920     ------------------------------------------------------------------------------- --
#
percentLabel <- function (perc.dist = 1, min.perc = -100, max.perc = 100, ...) {
    #
    # method                                        ............................................. ..
    # -
    #
    # input                                         ............................................. ..
    # -
    #
    # output                                        ............................................. ..
    # -
    #
    pos <- seq(min.perc/100, max.perc/100, perc.dist/100)
    txt <- seq(min.perc, max.perc, perc.dist)
    sig_pos <- -floor(log10(perc.dist)) + 2
    sig_txt <- -floor(log10(perc.dist))
    if ((round(perc.dist, 0) != perc.dist) & (perc.dist > 1))
        sig_pos <- sig_pos + 1
    if ((round(perc.dist, 0) != perc.dist) & (perc.dist > 1))
        sig_txt <- sig_txt + 1
    txt <- format(txt, nsmall = max(0, sig_txt), trim = TRUE)
    percentLabel <- data.frame(position = pos, text = paste(txt,"%", sep = "")
                             , stringsAsFactors = FALSE)
    return(percentLabel)
    #
}
# END OF FUNCTION  ----------------------------------------------------------------------------- --
# --------------- percentLabel ------------------------------------------------------------------ --
#
# ==================================================================================================
#
