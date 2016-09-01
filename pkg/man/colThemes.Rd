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
txt <- "please install packages 'lattice' and 'latticeExtra' for this example to work!"
if(!require(lattice)&require(latticeExtra)) print(txt)
if(require(lattice)&require(latticeExtra))
{
    require(latticeExtra)
    data(postdoc, package="latticeExtra")
    bc1 <- barchart(prop.table(postdoc, margin=1), xlab="Proportion",
                    main="default color scheme")
    bc2 <- barchart(prop.table(postdoc, margin=1), xlab="Proportion",
                    par.settings=cols3.theme(), main="color scheme 'cols3.theme'")
    bc3 <- barchart(prop.table(postdoc, margin=1), xlab="Proportion",
                    par.settings=cols4.theme(), main="color scheme 'cols4.theme'")
    bc4 <- barchart(prop.table(postdoc, margin=1), xlab="Proportion",
                    par.settings=cols7.theme(), main="color scheme 'cols7.theme'")
    plot(bc1, split=c(1, 1, 2, 2), more=TRUE)
    plot(bc2, split=c(1, 2, 2, 2), more=TRUE)
    plot(bc3, split=c(2, 1, 2, 2), more=TRUE)
    plot(bc4, split=c(2, 2, 2, 2), more=FALSE)
}
}
\keyword{lattice}
\keyword{graphics}
