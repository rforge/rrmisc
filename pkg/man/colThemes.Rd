\name{cols3.theme}
\alias{cols3.theme}
\alias{cols5.theme}
\alias{cols7.theme}
\title{ Custom color schemes for lattict graphics }
\description{ Lattice color schemes with 3, 5 and 7 colors }
\usage{ cols3.theme() }
\value{ list with definition of lattice color scheme }
\references{ none }
\author{ Roland Rapold }
\note{ under continuous developement }
\examples{
\dontrun{
if(require(lattice))
{
    data(Titanic)
    graph1 <- barchart(Class~Freq|Sex+Age, groups=Survived, stack=TRUE, data=as.data.frame(Titanic),
                       main="default color scheme of lattice")
    graph2 <- update(graph1, par.settings=cols3.theme(), main="'cols3.theme' color scheme")
    plot(graph1, split=c(1, 1, 1, 2), more=TRUE)
    plot(graph2, split=c(1, 2, 1, 2), more=FALSE)
}
}
}
\keyword{lattice}
\keyword{graphics}
