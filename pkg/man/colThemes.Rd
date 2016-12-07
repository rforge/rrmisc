\name{cols3.theme}
\alias{cols3.theme}
\alias{cols4.theme}
\alias{cols7.theme}
\title{ Custom color schemes for lattict graphics }
\description{ Lattice color schemes with 3, 5 and 7 colors }
\usage{ cols3.theme(...) }
\arguments{
    \item{\dots}{ arguments passed to further functions }
}
\value{ list with definition of lattice color scheme }
\references{ none }
\author{ Roland Rapold }
\note{ under continuous developement }
\examples{
if(!require(lattice)&require(latticeExtra))
{
    print("please install packages 'lattice' and 'latticeExtra' for this example to work!")
} else {
    par(mfrow=c(4, 1),          # following parameters go c(bottom, left, top, right)
        oma=c(2,2,2,2)+0.1,     # two rows of text at the outer left and bottom margin
        mar=c(1,1,2,1)+0.1)     # space for one row of text at ticks and to separate plots
    image(x=c(1:3), y=1, z=matrix(1:3, ncol=1), col=cols3.theme()$superpose.polygon$col,
          xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="cols3.theme()", cex.main=2)
    image(x=c(1:4), y=1, z=matrix(1:4, ncol=1), col=cols4.theme()$superpose.polygon$col,
          xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="cols4.theme()", cex.main=2)
    image(x=c(1:7), y=1, z=matrix(1:7, ncol=1), col=cols7.theme()$superpose.polygon$col,
          xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="cols7.theme()", cex.main=2)
    image(x=c(1:5), y=1, z=matrix(1:5, ncol=1), col=theEconomist.theme()$superpose.polygon$col,
          xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main="theEconomist.theme()", cex.main=2)
}
}
% \keyword{lattice}
\keyword{graphics}


