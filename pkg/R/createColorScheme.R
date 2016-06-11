createColorScheme <- function(def.theme, colors, colors100, alpha)
{
    #
    if(!requireNamespace(lattice))
    {
        return("lattice is needed - please install!")
    } else {
        #
        # Farbpalette laden  -----------------------------------------------------------------------
        trellis.par.set(def.theme)
        #
        # Farbschemata anpassen --------------------------------------------------------------------
        superpose_symbol         <- trellis.par.get("superpose.symbol")
        superpose_symbol$alpha   <- alpha
        superpose_symbol$cex     <- 1
        superpose_symbol$col     <- colors
        superpose_symbol$fill    <- colors
        superpose_symbol$font    <- 1
        superpose_symbol$pch     <- 16
        trellis.par.set("superpose.symbol", superpose_symbol)

        superpose_line           <- trellis.par.get("superpose.line")
        superpose_line$alpha     <- alpha
        superpose_line$col       <- colors
        superpose_line$lty       <- 1
        superpose_line$lwd       <- 2.5
        trellis.par.set("superpose.line", superpose_line)

        strip_background         <- trellis.par.get("strip.background")
        strip_background$alpha   <- alpha
        strip_background$col     <- colors
        trellis.par.set("strip.background", strip_background)

        strip_shingle            <- trellis.par.get("strip.shingle")
        strip_shingle$alpha      <- alpha
        strip_shingle$col        <- colors
        trellis.par.set("strip.shingle", strip_shingle)

        dot_symbol               <- trellis.par.get("dot.symbol")
        dot_symbol$alpha         <- alpha
        dot_symbol$cex           <- 1
        dot_symbol$col           <- colors[[length(colors)-1]]
        dot_symbol$font          <- 1
        dot_symbol$pch           <- 16
        trellis.par.set("dot.symbol", dot_symbol)

        dot_line                 <- trellis.par.get("dot.line")
        dot_line$alpha           <- alpha
        dot_line$col             <- colors[[length(colors)-1]]
        dot_line$lty             <- 1
        dot_line$lwd             <- 1.5
        trellis.par.set("dot.line", dot_line)

        box_dot                  <- trellis.par.get("box.dot")
        box_dot$alpha            <- alpha
        box_dot$col              <- colors[[length(colors)-1]]
        box_dot$cex              <- 1
        box_dot$font             <- 1
        box_dot$pch              <- "|"
        box_dot$lwd              <- 1.5
        trellis.par.set("box.dot", box_dot)

        box_rectangle            <- trellis.par.get("box.rectangle")
        box_rectangle$alpha      <- alpha
        box_rectangle$col        <- colors[[length(colors)-1]]
        box_rectangle$fill       <- "gray80"
        box_rectangle$lty        <- 1
        box_rectangle$lwd        <- 1.5
        trellis.par.set("box.rectangle", box_rectangle)

        box_umbrella             <- trellis.par.get("box.umbrella")
        box_umbrella$alpha       <- alpha
        box_umbrella$col         <- colors[[length(colors)-1]]
        box_umbrella$lty         <- 1
        box_umbrella$lwd         <- 1.5
        trellis.par.set("box.umbrella", box_umbrella)

        add_line                 <- trellis.par.get("add.line")
        add_line$alpha           <- alpha
        add_line$col             <- colors[[length(colors)-1]]
        add_line$lty             <- 1
        add_line$lwd             <- 1.5
        trellis.par.set("add.line", add_line)

        add_text                 <- trellis.par.get("add.text")
        add_text$alpha           <- alpha
        add_text$cex             <- 1
        add_text$col             <- "gray30"
        add_text$font            <- 1
        add_text$lineheight      <- 1.2

        reference_line           <- trellis.par.get("reference.line")
        reference_line$alpha     <- alpha
        reference_line$col       <- colors[[length(colors)-1]]
        reference_line$lty       <- 1
        reference_line$lwd       <- 1.5
        trellis.par.set("reference.line", reference_line)

        plot_symbol              <- trellis.par.get("plot.symbol")
        plot_symbol$alpha        <- alpha
        plot_symbol$cex          <- 1
        plot_symbol$col          <- colors[[length(colors)-1]]
        plot_symbol$font         <- 1
        plot_symbol$pch          <- 16
        plot_symbol$fill         <- colors[[length(colors)-1]]
        trellis.par.set("plot.symbol", plot_symbol)

        plot_line                <- trellis.par.get("plot.line")
        plot_line$alpha          <- alpha
        plot_line$col            <- colors[[length(colors)-1]]
        plot_line$lty            <- 1
        plot_line$lwd            <- 1.5
        trellis.par.set("plot.line", plot_line)

        plot_polygon             <- trellis.par.get("plot.polygon")
        plot_polygon$alpha       <- alpha
        plot_polygon$col         <- colors[[length(colors)-1]]
        plot_polygon$border      <- "black"
        plot_polygon$lty         <- 1
        plot_polygon$lwd         <- 1
        trellis.par.set("plot.polygon", plot_polygon)

        superpose_polygon        <- trellis.par.get("superpose.polygon")
        superpose_polygon$alpha  <- alpha
        superpose_polygon$col    <- colors
        superpose_polygon$border <- "black"
        superpose_polygon$lty    <- 1
        superpose_polygon$lwd    <- 1
        trellis.par.set("superpose.polygon", superpose_polygon)

        regions                  <- trellis.par.get("regions")
        regions$alpha            <- alpha
        regions$col              <- colors100
        trellis.par.set("regions", regions)

        return(trellis.par.get())
    }
}
