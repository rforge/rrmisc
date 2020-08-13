# ==================================================================================================
# --------------- percentLabel                      create percentage labels for plots            ..# {{{
# RR 20130920     ------------------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#' @title List of percent values, e.g. for axis displays.
#'
#' @description List of percent values, e.g. for axis displays.
#'
#' @details data.frame of number and corresponding representation as percentage values. E.g. for use
#' as axix label in graphics.
#'
#' @param perc.dist interval steps of percent list
#' @param min.perc minimum percent value
#' @param max.perc maximum percent value
#' @param \dots arguments passed to further functions
#' @return data-frame with percentage as number and character string
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso see other functions in this R-package
#' @references none
#' @examples
#'      # list from 0 to 1 in 5 - steps
#'      percentLabel(perc.dist=5, min.perc=0)
#'      #
#'      # example for usage in plot()
#'      p.lab <- percentLabel(perc.dist=20, min.perc=-100, max.perc=200)
#'      x.sam <- rnorm(40, 0.4, 0.4)
#'      y.sam <- rnorm(40, 0.4, 0.4)
#'      par(mfrow=c(1, 2))
#'      plot(x=x.sam, y=y.sam, yaxt="n", xlim=c(-0.5, 1.5), ylim=c(-0.5, 1.5))
#'      axis(2, at=p.lab$position ,labels=p.lab$text)
#'      plot(x=x.sam, y=y.sam, yaxt="n", xlim=c(-0.5, 1.5), ylim=c(-0.5, 1.5))
#'      axis(2, at=p.lab$position ,labels=p.lab$text, las=1, cex.axis=0.8)
#' @export
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
## }}}
# ==================================================================================================
#
