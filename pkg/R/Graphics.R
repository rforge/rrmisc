# ==================================================================================================
# --------------- color themes for lattice graphics                                               --{{{
# cols3.theme                                                                                     ..{{{
#
# Manual          ------------------------------------------------------------------------------- --
#
#' @title Custom color schemes for lattict graphics
#'
#' @description Lattice color schemes with sequences of colors from light to dark 'seq..', from one
#' color to another color 'div..' and some qualitative colors 'qual..'.
#'
#' @aliases cols3.theme cols4.theme cols7.theme seq01.theme seq02.theme seq03.theme seq04.theme div01.theme div02.theme div03.theme qual01.theme qual02.theme
#' @export  cols3.theme cols4.theme cols7.theme seq01.theme seq02.theme seq03.theme seq04.theme div01.theme div02.theme div03.theme qual01.theme qual02.theme
#' @param \dots arguments passed to further functions
#' @return List with definition of lattice color scheme.
#' @note under continuous developement
#' @author Roland Rapold
#' @references For seq0x.theme(), qal0x.theme, and div0x.theme() see https://datavisualization.ch/inside/how-we-created-color-scales/
#' @keywords graphics
#' @examples
#' if(!requireNamespace("latticeExtra", quietly = TRUE))
#' {
#'     print("please install packages 'lattice' and 'latticeExtra' for this example to work!")
#' } else {
#'     par(mfrow=c(4, 1),          # following parameters go c(bottom, left, top, right)
#'         oma=c(2,2,2,2)+0.1,     # two rows of text at the outer left and bottom margin
#'         mar=c(1,1,2,1)+0.1,     # space for one row of text at ticks and to separate plots
#'         ask=TRUE)
#'     image(x=c(1:3), y=1, z=matrix(1:3, ncol=1), col=cols3.theme()$superpose.polygon$col,
#'           xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="cols3.theme()", cex.main=2)
#'     image(x=c(1:4), y=1, z=matrix(1:4, ncol=1), col=cols4.theme()$superpose.polygon$col,
#'           xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="cols4.theme()", cex.main=2)
#'     image(x=c(1:7), y=1, z=matrix(1:7, ncol=1), col=cols7.theme()$superpose.polygon$col,
#'           xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="cols7.theme()", cex.main=2)
#'     image(x=c(1:5), y=1, z=matrix(1:5, ncol=1),
#'           col=latticeExtra::theEconomist.theme()$superpose.polygon$col,
#'           xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="theEconomist.theme()", cex.main=2)
#'
#'     par(mfrow=c(4, 1),          # following parameters go c(bottom, left, top, right)
#'         oma=c(2,2,2,2)+0.1,     # two rows of text at the outer left and bottom margin
#'         mar=c(1,1,2,1)+0.1,     # space for one row of text at ticks and to separate plots
#'         ask=TRUE)
#'     image(x=c(1:9), y=1, z=matrix(1:9, ncol=1), col=seq01.theme()$superpose.polygon$col,
#'           xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="seq01.theme()", cex.main=2)
#'     image(x=c(1:9), y=1, z=matrix(1:9, ncol=1), col=seq02.theme()$superpose.polygon$col,
#'           xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="seq02.theme()", cex.main=2)
#'     image(x=c(1:9), y=1, z=matrix(1:9, ncol=1), col=seq03.theme()$superpose.polygon$col,
#'           xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="seq03.theme()", cex.main=2)
#'     image(x=c(1:9), y=1, z=matrix(1:9, ncol=1), col=seq04.theme()$superpose.polygon$col,
#'           xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="seq04.theme()", cex.main=2)
#'
#'     par(mfrow=c(5, 1),          # following parameters go c(bottom, left, top, right)
#'         oma=c(2,2,2,2)+0.1,     # two rows of text at the outer left and bottom margin
#'         mar=c(1,1,2,1)+0.1,     # space for one row of text at ticks and to separate plots
#'         ask=TRUE)
#'     image(x=c(1:9), y=1, z=matrix(1:9, ncol=1), col=div01.theme()$superpose.polygon$col,
#'           xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="div01.theme()", cex.main=2)
#'     image(x=c(1:6), y=1, z=matrix(1:6, ncol=1), col=div02.theme()$superpose.polygon$col,
#'           xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="div02.theme()", cex.main=2)
#'     image(x=c(1:9), y=1, z=matrix(1:9, ncol=1), col=div03.theme()$superpose.polygon$col,
#'           xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="div03.theme()", cex.main=2)
#'     image(x=c(1:12), y=1, z=matrix(1:12, ncol=1), col=qual01.theme()$superpose.polygon$col,
#'           xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="qual01.theme()", cex.main=2)
#'     image(x=c(1:6), y=1, z=matrix(1:6, ncol=1), col=qual02.theme()$superpose.polygon$col,
#'           xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="qual02.theme()", cex.main=2)
#' }
#' @export
cols3.theme <- function (...) {
    # 7 Colors from palette 'H'
    # colors <- hc(nr=c(1:7), alpha=100)
    # colors <- c("#9A0941FF", "#F08100FF", "#FED037FF", "#CAB790FF", "#D35186FF", "#8296C4FF", "#B3BA12FF")
    # for alphas 80, 60, 40, 20 substitute last two letters (FF) with  "CC","99","66","33"
    #
    # colors <- hc(nr=c(1:7), alpha=80)
    colors <- c("#9A0941CC", "#F08100CC", "#FED037CC", "#CAB790CC", "#D35186CC", "#8296C4CC", "#B3BA12CC")
    #
    colsA  <- colors[5:7]
    colA.theme <- lattice::simpleTheme(col=colsA,
                                       col.line=colsA[1],
                                       alpha=1,
                                       pch=16,
                                       lwd=1,
                                       fill=colsA[1],
                                       border="black"
                                       )
    #
    colA.theme[["strip.shingle"]]
    colA.theme[["strip.shingle"]]$col   <- colsA
    colA.theme[["strip.shingle"]]$alpha <- 1
    colA.theme[["strip.background"]]
    colA.theme[["strip.background"]]$col   <- colsA
    colA.theme[["strip.background"]]$alpha <- 1
    colA.theme[["strip.border"]]
    colA.theme[["superpose.line"]]
    colA.theme[["superpose.line"]]$col <- colsA
    colA.theme[["superpose.line"]]$lwd <- 2
    colA.theme[["superpose.symbol"]]
    colA.theme[["superpose.symbol"]]$cex <- 1
    #
    return(colA.theme)
}
## }}}
# cols4.theme                                                                                     ..{{{
cols4.theme <- function (...) {
    # 7 Colors from palette 'H'
    # colors <- hc(nr=c(1:7), alpha=100)
    # colors <- c("#9A0941FF", "#F08100FF", "#FED037FF", "#CAB790FF", "#D35186FF", "#8296C4FF", "#B3BA12FF")
    # for alphas 80, 60, 40, 20 substitute last two letters (FF) with  "CC","99","66","33"
    #
    # colors <- hc(nr=c(1:7), alpha=80)
    colors <- c("#9A0941CC", "#F08100CC", "#FED037CC", "#CAB790CC", "#D35186CC", "#8296C4CC", "#B3BA12CC")
    #
    colsA  <- colors[c(5:7, 4)]
    colA.theme <- lattice::simpleTheme(col=colsA,
                                       col.line=colsA[1],
                                       alpha=1,
                                       pch=16,
                                       lwd=1,
                                       fill=colsA[1],
                                       border="black"
                                       )
    #
    colA.theme[["strip.shingle"]]
    colA.theme[["strip.shingle"]]$col   <- colsA
    colA.theme[["strip.shingle"]]$alpha <- 1
    colA.theme[["strip.background"]]
    colA.theme[["strip.background"]]$col   <- colsA
    colA.theme[["strip.background"]]$alpha <- 1
    colA.theme[["strip.border"]]
    colA.theme[["superpose.line"]]
    colA.theme[["superpose.line"]]$col <- colsA
    colA.theme[["superpose.line"]]$lwd <- 2
    colA.theme[["superpose.symbol"]]
    colA.theme[["superpose.symbol"]]$cex <- 1
    #
    return(colA.theme)
}
## }}}
# cols7.theme                                                                                     ..{{{
cols7.theme <- function (...) {
    # 7 Colors from palette 'H'
    # colors <- hc(nr=c(1:7), alpha=100)
    # colors <- c("#9A0941FF", "#F08100FF", "#FED037FF", "#CAB790FF", "#D35186FF", "#8296C4FF", "#B3BA12FF")
    # for alphas 80, 60, 40, 20 substitute last two letters (FF) with  "CC","99","66","33"
    #
    # colors <- hc(nr=c(1:7), alpha=80)
    colors <- c("#9A0941CC", "#F08100CC", "#FED037CC", "#CAB790CC", "#D35186CC", "#8296C4CC", "#B3BA12CC")
    #
    colsA  <- colors
    colA.theme <- lattice::simpleTheme(col=colsA,
                                       col.line=colsA[1],
                                       alpha=1,
                                       pch=16,
                                       lwd=1,
                                       fill=colsA[1],
                                       border="black"
                                       )
    #
    colA.theme[["strip.shingle"]]
    colA.theme[["strip.shingle"]]$col   <- colsA
    colA.theme[["strip.shingle"]]$alpha <- 1
    colA.theme[["strip.background"]]
    colA.theme[["strip.background"]]$col   <- colsA
    colA.theme[["strip.background"]]$alpha <- 1
    colA.theme[["strip.border"]]
    colA.theme[["superpose.line"]]
    colA.theme[["superpose.line"]]$col <- colsA
    colA.theme[["superpose.line"]]$lwd <- 2
    colA.theme[["superpose.symbol"]]
    colA.theme[["superpose.symbol"]]$cex <- 1
    #
    return(colA.theme)
}
## }}}
#
# Colors from: https://datavisualization.ch/inside/how-we-created-color-scales/
#
# SEQxx The Sequential Color Scheme ................................................................
# seq01.theme                                                                                     ..{{{
# seq01 -- 9 Colors blue from dark to light
seq01.theme <- function (...) {
    #
    seq01.rgb <- c(c( 54,  62,  73), c( 53,  77, 103), c( 54,  90, 129),
                   c( 55, 103, 153), c( 55, 117, 179), c( 97, 145, 199),
                   c(137, 173, 217), c(179, 201, 235), c(220, 232, 253))
    seq01 <- grDevices::rgb(red=seq01.rgb[seq(1, 25, 3)],
                            green=seq01.rgb[seq(2, 26, 3)],
                            blue= seq01.rgb[seq(3, 27, 3)],
                            names=paste0("seq01.", 1:9),
                            maxColorValue=255)

    seq01.theme <- lattice::simpleTheme(col=seq01,
                                        col.line=seq01[1],
                                        alpha=1,
                                        pch=16,
                                        lwd=1,
                                        fill=seq01[1],
                                        border="black"
                                        )
    #
    seq01.theme[["strip.shingle"]]
    seq01.theme[["strip.shingle"]]$col   <- seq01
    seq01.theme[["strip.shingle"]]$alpha <- 1
    seq01.theme[["strip.background"]]
    seq01.theme[["strip.background"]]$col   <- seq01
    seq01.theme[["strip.background"]]$alpha <- 1
    seq01.theme[["strip.border"]]
    seq01.theme[["superpose.line"]]
    seq01.theme[["superpose.line"]]$col <- seq01
    seq01.theme[["superpose.line"]]$lwd <- 2
    seq01.theme[["superpose.symbol"]]
    seq01.theme[["superpose.symbol"]]$cex <- 1
    #
    return(seq01.theme)
}
## }}}
# seq02.theme                                                                                     ..{{{
# seq02 -- 9 Colors red from dark to light
seq02.theme <- function (...) {
    #
    seq02.rgb <- c(c( 78,  53,  57), c(109,  64,  71), c(140,  73,  84),
                   c(173,  84,  97), c(204,  95, 111), c(217, 130, 142),
                   c(228, 166, 172), c(241, 199, 204), c(252, 235, 235))
    seq02 <- grDevices::rgb(red=seq02.rgb[seq(1, 25, 3)],
                            green=seq02.rgb[seq(2, 26, 3)],
                            blue= seq02.rgb[seq(3, 27, 3)],
                            names=paste0("seq02.", 1:9),
                            maxColorValue=255)

    seq02.theme <- lattice::simpleTheme(col=seq02,
                                        col.line=seq02[1],
                                        alpha=1,
                                        pch=16,
                                        lwd=1,
                                        fill=seq02[1],
                                        border="black"
                                        )
    #
    seq02.theme[["strip.shingle"]]
    seq02.theme[["strip.shingle"]]$col   <- seq02
    seq02.theme[["strip.shingle"]]$alpha <- 1
    seq02.theme[["strip.background"]]
    seq02.theme[["strip.background"]]$col   <- seq02
    seq02.theme[["strip.background"]]$alpha <- 1
    seq02.theme[["strip.border"]]
    seq02.theme[["superpose.line"]]
    seq02.theme[["superpose.line"]]$col <- seq02
    seq02.theme[["superpose.line"]]$lwd <- 2
    seq02.theme[["superpose.symbol"]]
    seq02.theme[["superpose.symbol"]]$cex <- 1
    #
    return(seq02.theme)
}
## }}}
# seq03.theme                                                                                     ..{{{
# seq03 -- 9 Colors green from dark to light
seq03.theme <- function (...) {
    #
    seq03.rgb <- c(c( 44,  59,  62), c( 51,  77,  78), c( 57,  94,  94),
                   c( 65, 110, 108), c( 71, 128, 123), c(107, 150, 146),
                   c(140, 174, 172), c(173, 197, 197), c(209, 222, 221))
    seq03 <- grDevices::rgb(red=seq03.rgb[seq(1, 25, 3)],
                            green=seq03.rgb[seq(2, 26, 3)],
                            blue= seq03.rgb[seq(3, 27, 3)],
                            names=paste0("seq03.", 1:9),
                            maxColorValue=255)

    seq03.theme <- lattice::simpleTheme(col=seq03,
                                        col.line=seq03[1],
                                        alpha=1,
                                        pch=16,
                                        lwd=1,
                                        fill=seq03[1],
                                        border="black"
                                        )
    #
    seq03.theme[["strip.shingle"]]
    seq03.theme[["strip.shingle"]]$col   <- seq03
    seq03.theme[["strip.shingle"]]$alpha <- 1
    seq03.theme[["strip.background"]]
    seq03.theme[["strip.background"]]$col   <- seq03
    seq03.theme[["strip.background"]]$alpha <- 1
    seq03.theme[["strip.border"]]
    seq03.theme[["superpose.line"]]
    seq03.theme[["superpose.line"]]$col <- seq03
    seq03.theme[["superpose.line"]]$lwd <- 2
    seq03.theme[["superpose.symbol"]]
    seq03.theme[["superpose.symbol"]]$cex <- 1
    #
    return(seq03.theme)
}
## }}}
# seq04.theme                                                                                     ..{{{
# seq04 -- 9 Colors brown from dark to light
seq04.theme <- function (...) {
    #
    seq04.rgb <- c(c( 75,  55,  53), c( 98,  72,  60), c(122,  90,  69),
                   c(144, 107,  79), c(165, 124,  87), c(183, 147, 119),
                   c(199, 172, 149), c(214, 197, 181), c(232, 222, 211))
    seq04 <- grDevices::rgb(red=seq04.rgb[seq(1, 25, 3)],
                            green=seq04.rgb[seq(2, 26, 3)],
                            blue= seq04.rgb[seq(3, 27, 3)],
                            names=paste0("seq04.", 1:9),
                            maxColorValue=255)

    seq04.theme <- lattice::simpleTheme(col=seq04,
                                        col.line=seq04[1],
                                        alpha=1,
                                        pch=16,
                                        lwd=1,
                                        fill=seq04[1],
                                        border="black"
                                        )
    #
    seq04.theme[["strip.shingle"]]
    seq04.theme[["strip.shingle"]]$col   <- seq04
    seq04.theme[["strip.shingle"]]$alpha <- 1
    seq04.theme[["strip.background"]]
    seq04.theme[["strip.background"]]$col   <- seq04
    seq04.theme[["strip.background"]]$alpha <- 1
    seq04.theme[["strip.border"]]
    seq04.theme[["superpose.line"]]
    seq04.theme[["superpose.line"]]$col <- seq04
    seq04.theme[["superpose.line"]]$lwd <- 2
    seq04.theme[["superpose.symbol"]]
    seq04.theme[["superpose.symbol"]]$cex <- 1
    #
    return(seq04.theme)
}
## }}}
#
# DIVxx The Diverging Color Scheme .................................................................
# div01.theme                                                                                     ..{{{
# div01 -- 9 Farben blau - weiss - rot
div01.theme <- function (...) {
    #
    div01.rgb <- c(c( 55, 117, 179), c(103, 146, 196), c(148, 178, 211),
                   c(196, 210, 226), c(241, 241, 241), c(233, 205, 210),
                   c(224, 169, 176), c(214, 131, 144), c(204,  95, 111))
    div01 <- grDevices::rgb(red=div01.rgb[seq(1, 25, 3)],
                            green=div01.rgb[seq(2, 26, 3)],
                            blue= div01.rgb[seq(3, 27, 3)],
                            names=paste0("div01.", 1:9),
                            maxColorValue=255)

    div01.theme <- lattice::simpleTheme(col=div01,
                                        col.line=div01[1],
                                        alpha=1,
                                        pch=16,
                                        lwd=1,
                                        fill=div01[1],
                                        border="black"
                                        )
    #
    div01.theme[["strip.shingle"]]
    div01.theme[["strip.shingle"]]$col   <- div01
    div01.theme[["strip.shingle"]]$alpha <- 1
    div01.theme[["strip.background"]]
    div01.theme[["strip.background"]]$col   <- div01
    div01.theme[["strip.background"]]$alpha <- 1
    div01.theme[["strip.border"]]
    div01.theme[["superpose.line"]]
    div01.theme[["superpose.line"]]$col <- div01
    div01.theme[["superpose.line"]]$lwd <- 2
    div01.theme[["superpose.symbol"]]
    div01.theme[["superpose.symbol"]]$cex <- 1
    #
    return(div01.theme)
}
## }}}
# div02.theme                                                                                     ..{{{
# div02 -- 6 Colors from blue to white to red
div02.theme <- function (...) {
    #
    div01.rgb <- c(c( 55, 117, 179), c(103, 146, 196), c(148, 178, 211),
                   c(196, 210, 226), c(241, 241, 241), c(233, 205, 210),
                   c(224, 169, 176), c(214, 131, 144), c(204,  95, 111))

    # div02.rgb <- div01.rgb[c(1:3, 7:9)]
    div02.rgb <- div01.rgb[c(1:9, 19:27)]

    div02 <- grDevices::rgb(red=div02.rgb[seq(1, 16, 3)],
                            green=div02.rgb[seq(2, 17, 3)],
                            blue= div02.rgb[seq(3, 18, 3)],
                            names=paste0("div02.", 1:6),
                            maxColorValue=255)
    # pie(1:6, col=div02)

    div02.theme <- lattice::simpleTheme(col=div02,
                                        col.line=div02, #[1],
                                        alpha=1,
                                        pch=16,
                                        lwd=1,
                                        fill=div02, #[1],
                                        border="black"
                                        )
    # div02.theme$superpose.polygon$col
    #
    div02.theme[["strip.shingle"]]
    div02.theme[["strip.shingle"]]$col   <- div02
    div02.theme[["strip.shingle"]]$alpha <- 1
    div02.theme[["strip.background"]]
    div02.theme[["strip.background"]]$col   <- div02
    div02.theme[["strip.background"]]$alpha <- 1
    div02.theme[["strip.border"]]
    div02.theme[["superpose.line"]]
    div02.theme[["superpose.line"]]$col <- div02
    div02.theme[["superpose.line"]]$lwd <- 2
    div02.theme[["superpose.symbol"]]
    div02.theme[["superpose.symbol"]]$cex <- 1
    div02.theme[["superpose.polygon"]]
    div02.theme[["superpose.polygon"]]$col   <- div02
    div02.theme[["superpose.polygon"]]$alpha <- 1
    #
    return(div02.theme)
}
## }}}
# div03.theme                                                                                     ..{{{
# div03 -- 9 Colrs from green to white to brown
div03.theme <- function (...) {
    #
    div03.rgb <- c(c( 71, 128, 123), c(115, 156, 153), c(157, 184, 183),
                   c(199, 212, 211), c(241, 241, 241), c(223, 211, 204),
                   c(205, 183, 165), c(185, 154, 126), c(165, 124,  87))

    # div03.rgb <- div01.rgb[c(1:3, 7:9)]
    # div03.rgb <- div01.rgb[c(1:9, 19:27)]

    div03 <- grDevices::rgb(red=div03.rgb[seq(1, 25, 3)],
                            green=div03.rgb[seq(2, 26, 3)],
                            blue= div03.rgb[seq(3, 27, 3)],
                            names=paste0("div03.", 1:9),
                            maxColorValue=255)
    # pie(1:6, col=div03)

    div03.theme <- lattice::simpleTheme(col=div03,
                                        col.line=div03, #[1],
                                        alpha=1,
                                        pch=16,
                                        lwd=1,
                                        fill=div03, #[1],
                                        border="black"
                                        )
    # div03.theme$superpose.polygon$col
    #
    div03.theme[["strip.shingle"]]
    div03.theme[["strip.shingle"]]$col   <- div03
    div03.theme[["strip.shingle"]]$alpha <- 1
    div03.theme[["strip.background"]]
    div03.theme[["strip.background"]]$col   <- div03
    div03.theme[["strip.background"]]$alpha <- 1
    div03.theme[["strip.border"]]
    div03.theme[["superpose.line"]]
    div03.theme[["superpose.line"]]$col <- div03
    div03.theme[["superpose.line"]]$lwd <- 2
    div03.theme[["superpose.symbol"]]
    div03.theme[["superpose.symbol"]]$cex <- 1
    div03.theme[["superpose.polygon"]]
    div03.theme[["superpose.polygon"]]$col   <- div03
    div03.theme[["superpose.polygon"]]$alpha <- 1
    #
    return(div03.theme)
}
## }}}
#
# qualxx The Qualitative Color Scheme ..............................................................
# qual01.theme                                                                                    ..{{{
qual01.theme <- function (...) {
    #
    qual01.rgb <- c(c( 88, 132, 179), c(182, 206, 229),
                    c(204, 102, 134), c(229, 181, 197),
                    c(232, 123, 112), c(242, 206, 193),
                    c(229, 207, 108), c(249, 235, 170),
                    c(145, 190, 100), c(206, 229, 181),
                    c( 91, 190, 148), c(182, 228, 209))

    # qual01.rgb <- div01.rgb[c(1:3, 7:9)]
    # qual01.rgb <- div01.rgb[c(1:9, 19:27)]

    qual01 <- grDevices::rgb(red=qual01.rgb[seq(1, 34, 3)],
                             green=qual01.rgb[seq(2, 35, 3)],
                             blue= qual01.rgb[seq(3, 36, 3)],
                             names=paste0("qual01.", 1:12),
                             maxColorValue=255)
    # pie(1:6, col=qual01)

    qual01.theme <- lattice::simpleTheme(col=qual01,
                                         col.line=qual01, #[1],
                                         alpha=1,
                                         pch=16,
                                         lwd=1,
                                         fill=qual01, #[1],
                                         border="black"
                                         )
    # qual01.theme$superpose.polygon$col
    #
    qual01.theme[["strip.shingle"]]
    qual01.theme[["strip.shingle"]]$col   <- qual01
    qual01.theme[["strip.shingle"]]$alpha <- 1
    qual01.theme[["strip.background"]]
    qual01.theme[["strip.background"]]$col   <- qual01
    qual01.theme[["strip.background"]]$alpha <- 1
    qual01.theme[["strip.border"]]
    qual01.theme[["superpose.line"]]
    qual01.theme[["superpose.line"]]$col <- qual01
    qual01.theme[["superpose.line"]]$lwd <- 2
    qual01.theme[["superpose.symbol"]]
    qual01.theme[["superpose.symbol"]]$cex <- 1
    qual01.theme[["superpose.polygon"]]
    qual01.theme[["superpose.polygon"]]$col   <- qual01
    qual01.theme[["superpose.polygon"]]$alpha <- 1
    #
    return(qual01.theme)
}
## }}}
# qual02.theme                                                                                    ..{{{
qual02.theme <- function (...) {
    #
    qual02a.rgb <- c(c( 88, 132, 179), c(182, 206, 229),
                     c(204, 102, 134), c(229, 181, 197),
                     c(232, 123, 112), c(242, 206, 193),
                     c(229, 207, 108), c(249, 235, 170),
                     c(145, 190, 100), c(206, 229, 181),
                     c( 91, 190, 148), c(182, 228, 209))

    # qual02.rgb <- div01.rgb[c(1:3, 7:9)]
    qual02b.rgb <- qual02a.rgb[c(1:3, 7:9, 13:15, 19:21, 25:27, 31:33)]

    qual02 <- grDevices::rgb(red=qual02b.rgb[seq(1, 16, 3)],
                             green=qual02b.rgb[seq(2, 17, 3)],
                             blue= qual02b.rgb[seq(3, 18, 3)],
                             names=paste0("qual02.", 1:6),
                             maxColorValue=255)
    # pie(1:6, col=qual02)

    qual02.theme <- lattice::simpleTheme(col=qual02,
                                         col.line=qual02, #[1],
                                         alpha=1,
                                         pch=16,
                                         lwd=1,
                                         fill=qual02, #[1],
                                         border="black"
                                         )
    # qual02.theme$superpose.polygon$col
    #
    qual02.theme[["strip.shingle"]]
    qual02.theme[["strip.shingle"]]$col   <- qual02
    qual02.theme[["strip.shingle"]]$alpha <- 1
    qual02.theme[["strip.background"]]
    qual02.theme[["strip.background"]]$col   <- qual02
    qual02.theme[["strip.background"]]$alpha <- 1
    qual02.theme[["strip.border"]]
    qual02.theme[["superpose.line"]]
    qual02.theme[["superpose.line"]]$col <- qual02
    qual02.theme[["superpose.line"]]$lwd <- 2
    qual02.theme[["superpose.symbol"]]
    qual02.theme[["superpose.symbol"]]$cex <- 1
    qual02.theme[["superpose.polygon"]]
    qual02.theme[["superpose.polygon"]]$col   <- qual02
    qual02.theme[["superpose.polygon"]]$alpha <- 1
    #
    return(qual02.theme)
}
## }}}
#}}}
# --------------- percentLabel                create percentage labels for plots                  ..{{{
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
# --------------- univarModDataCont           dependent variable vs. independent variables        ..{{{
# RR 20130807     ------------------------------------------------------------------------------- --
# Manual          ------------------------------------------------------------------------------- --
#' @title Graph dependent variable vs. list of independent variables
#'
#' @description Graph dependent variable vs. list of independent variables
#'
#' @details Creates xyplots for all combinations
#'
#' @param d_data data.frame
#' @param var_dep name of dependent variable
#' @param label_dep label of dependent variable
#' @param var_indep name of independent variable(s)
#' @param label_indep label of independent variable(s)
#' @param \dots arguments passed to further functions
#' @return data-frame with percentage as number and character string
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso see other functions in this R-package
#' @references none
#' @examples
#'  data(mtcars, package = "datasets")
#'  str(mtcars)
#'  univarModDataCont(d_data = mtcars
#'                    , var_dep = "mpg"
#'                    , label_dep = "Miles per Gallon"
#'                    , var_indep = c("cyl", "disp", "hp", "wt")
#'                    , label_indep = c("Zylinder", "disp", "PS", "Gewicht"))
#'  univarModDataCont(d_data = mtcars
#'                    , var_dep = "mpg"
#'                    , label_dep = "Miles per Gallon"
#'                    , var_indep = c("cyl", "hp")
#'                    , label_indep = c("Zylinder", "PS"))
#' @export
univarModDataCont <- function( d_data, var_dep, var_indep, label_dep, label_indep, ...)
{
    # lattice is reqired for 'rrMisc'
    # tt <- require(lattice)
    # if(!tt)
    # {
    #     print("This function requires 'lattice'!")
    #     return()
    # }
    #
    mean_dep <- mean(d_data[, var_dep])
    #
    if (length(var_indep) == 1)  n_cols <- 1
    if (length(var_indep)  > 1)  n_cols <- 2
    #
    if (length(var_indep) %% 2 == 0) n_rows <- length(var_indep)/2
    if (length(var_indep) %% 2 == 1) n_rows <- (length(var_indep) + 1)/2
    #
    this.settings <- trellis.par.get()
    this.settings$plot.line$col <- "black"
    # show.settings(this.settings)
    i     <- 1
    i_col <- 1
    i_row <- 1
    for (i in c(1:length(var_indep))) {
        var_i   <- var_indep[i]
        label_i <- label_indep[i]
        ttf     <- formula(paste(var_dep, "~", var_i))
        # xyp     <- xyplot(x = ttf, data = d_data
        xyp     <- lattice::xyplot(x = ttf, data = d_data[!is.na(d_data[, var_i]), ]
                      # , type=c("g", "smooth", "p")
                        , xlab = label_i
                        , ylab = label_dep
                        , par.settings = this.settings
                          , panel = function(x, y, ...) {
                              panel.xyplot(x, y, ref=TRUE, type = c("p", "r"), ...)
                              panel.lmline(x, y, col=4, lwd=2)
                              # panel.loess(x, y, col=2, lwd=2)
                              panel.abline(h=0, col="grey", alpha=0.5, lwd=1)
                          }
                        )
        if (i_col == 1 & i_row == 1) {
            plot(xyp, split = c(i_col, i_row, n_cols, n_rows))
        }
        else {
            plot(xyp, split = c(i_col, i_row, n_cols, n_rows), newpage = FALSE)
        }
        if (i_row == n_rows) {
            i_col <- i_col + 1
            i_row <- 0
        }
        i_row <- i_row + 1
    }
}
# --------------- univarModDataCont-------------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
# }}}
# --------------- univarModDataFact           influence of factors on numeric variable            ..{{{
# RR 20130807     ------------------------------------------------------------------------------- --
# Manual          ------------------------------------------------------------------------------- --
#' @title Graph influence of factors on numeric variable
#'
#' @description Graph influence of factors on numeric variable
#'
#' @details Creates plot.design for all combinations
#'
#' @param d_data data.frame
#' @param var_dep_n name of dependent variable (numeric)
#' @param label_dep_n label of dependent variable
#' @param var_indep_f name of independent variable(s) (factor)
#' @param label_indep_f label of independent variable(s)
#' @param split allow for split into new graphics
#' @param split_nr upper limit of factors per graphic
#' @param ylimits y limits
#' @param plot_dims plot dimensions
#' @param \dots arguments passed to further functions
#' @return data-frame with percentage as number and character string
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso see other functions in this R-package
#' @references none
#' @examples
#'  data(npk, package = "datasets")
#'  str(npk)
#'  univarModDataFact(d_data = npk,
#'                    var_dep_n = "yield",
#'                    var_indep_f = c("N", "P", "K", "block"),
#'                    label_dep_n = "yield",
#'                    label_indep_f = c("N", "P", "K", "block"))
#'
#'  data(Mroz, package = "carData")
#'  str(Mroz)
#'  univarModDataFact(d_data = Mroz,
#'                    var_dep_n = "age",
#'                    var_indep_f = c("wc", "hc"),
#'                    label_dep_n = "Alter",
#'                    label_indep_f = c("WC", "HC"))
#'  # , split=TRUE
#'  # , split_nr=9, ylim=c(0, 0.55)
#'  # , plot_dims=c(28, 24))
#' @export
univarModDataFact <- function(d_data, var_dep_n, var_indep_f, label_dep_n, label_indep_f
                              , split = TRUE, split_nr = 4, ylimits = NULL
                              , plot_dims = c(15, 20), ...)
{
  if (!class(d_data[, var_dep_n]) %in% c("numeric", "integer"))
    return("Variable 'var_dep_n' is nicht numerisch!")
  for (ii in 1:length(var_indep_f)) {
    if (class(d_data[, var_indep_f[ii]]) != "factor")
      return("Variable 'var_indep_f' is kein Faktor!")
  }
  if (!is.null(dev.list())) dev.off()
  lbl_main <- encodeUTF8(paste("Einfl\u00fcsse auf", label_dep_n))
  lbl_x    <- "Faktoren"
  lbl_y    <- paste("Durchschnitt", label_dep_n)
  if (split == TRUE) {
    dev.new(width = plot_dims[1], height = plot_dims[2], unit = "cm")
    index <- (c(1:length(var_indep_f)) - 1)%/%split_nr + 1
    # index
    # max(index)
    # which(index == 1)
    par(mfrow = c(max(index), 1))
    for (i in c(1:max(index))) {
      plot.design(x = formula(paste(var_dep_n, "~", paste(var_indep_f[which(index == i)], collapse = " + ")))
                  , data = d_data
                  , ylim = ylimits
                  , main = lbl_main
                  , xlab = lbl_x
                  , ylab = lbl_y
                  , xaxt = "n")
      axis(1, at = c(1:length(label_indep_f[which(index == i)]))
           , labels = label_indep_f[which(index == i)]
           , las = 1, cex.axis = 0.7)
    }
  }
  else {
    windows(width = plot_dims[2], height = plot_dims[1])
    form <- formula(paste(var_dep_n, "~", paste(var_indep_f, collapse = " + ")))
    # form
    plot.design(x = form
                , data = d_data
                , ylim = ylimits
                , main = lbl_main
                , xlab = lbl_x
                , ylab = lbl_y
                , xaxt = "n")
    axis(1, at = c(1:length(label_indep_f))
         , labels = label_indep_f
         , las = 1
         , cex.axis = 0.7)
  }
}
# --------------- univarModDataFact-------------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
# }}}
# ==================================================================================================
