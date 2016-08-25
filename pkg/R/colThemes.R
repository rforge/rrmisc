
# --------------- color themes for lattice graphics                                               --
# RR 20160825     ------------------------------------------------------------------------------- --
#
cols3.theme = function ()   #                                                                     .. # {{{
{
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
 # }}}
#
cols4.theme = function ()   #                                                                     .. # {{{
{
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
 # }}}
#
cols7.theme = function ()   #                                                                     .. # {{{
{
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
 # }}}
#
# END OF FUNCTION  ----------------------------------------------------------------------------- --
# --------------- color themes for lattice graphics                                               --
#
