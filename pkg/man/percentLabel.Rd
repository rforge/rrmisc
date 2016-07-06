\name{percentLabel}
\alias{percentLabel}
\title{list of percent values, e.g. for axis displays.}
\description{list of percent values, e.g. for axis displays.}
\usage{ percentLabel(perc.dist = 1, min.perc = -100, max.perc = 100, ...) }
\arguments{
  \item{perc.dist}{ interval steps of percent list }
  \item{min.perc}{ minimum percent value }
  \item{max.perc}{ maximum percent value }
  \item{\dots}{ arguments passed to further functions }
}
\details{ utility function for formatting }
\value{ data-frame with percentage as number and character string }
\references{ none }
\author{ Roland Rapold }
\note{ under continuous developement }
\seealso{ see other functions in this R-package }
\examples{
print("--")
print("list from 0 to 1 in 5 - steps")
percentLabel(perc.dist=5, min.perc=0)
#
print("--")
print("example for usage in plot()")
p.lab <- percentLabel(perc.dist=20, min.perc=-100, max.perc=200)
x.sam <- rnorm(40, 0.4, 0.4)
y.sam <- rnorm(40, 0.4, 0.4)
par(mfrow=c(1, 2))
plot(x=x.sam, y=y.sam, yaxt="n", xlim=c(-0.5, 1.5), ylim=c(-0.5, 1.5))
axis(2, at=p.lab$position ,labels=p.lab$text)
plot(x=x.sam, y=y.sam, yaxt="n", xlim=c(-0.5, 1.5), ylim=c(-0.5, 1.5))
axis(2, at=p.lab$position ,labels=p.lab$text, las=1, cex.axis=0.8)
}
