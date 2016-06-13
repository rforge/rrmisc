\name{countDistinct}
\alias{countDistinct}
\title{ SQL-command 'count(distinct ...)' convreted to R }
\description{ SQL-command 'count(distinct ...)' convreted to R }
\usage{ countDistinct(x, ...) }
\arguments{
  \item{x}{ vector or data.frame in which the number of different entries are counted }
  \item{\dots}{ arguments passed to further functions }
}
\value{ count (integer) }
\references{ none }
\author{ Roland Rapold }
\note{ under continuous developement }
\examples{
print("-------------------------------------------------------------------------------------------")
print("Example for vector                   ------------------------------------------------------")
                      c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6)
               length(c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6))
        countDistinct(c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6))
       length(na.omit(c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6)))
countDistinct(na.omit(c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6)))
#
print("-------------------------------------------------------------------------------------------")
\dontrun{
require("MASS")
print("Example for data.frame               ------------------------------------------------------")
data(crabs, package="MASS")
head(crabs)
str(crabs)
countDistinct(crabs[, c("sp")])
countDistinct(crabs[, c("sex")])
countDistinct(crabs[, c("FL")])
}
}
