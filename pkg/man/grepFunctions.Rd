\name{grepColEntries}
\alias{grepColEntries}
\alias{grepColNames}
\alias{grepColNA}
\alias{grepObjNames}
\alias{grepNotCompleteEntries}
\alias{grepMultipleEntries}
\alias{grepRowNames}
\title{ obtain grep components of object easily }
\description{ obtain grep components of object easily }
\usage{ grepColEntries(d.frame, col.name, part.name, ...) }
\arguments{
  \item{d.frame}{ R-object }
  \item{col.name}{ name of column }
  \item{part.name}{ search string in column names }
  \item{\dots}{ arguments passed to further functions }
}
\details{ functions facilitating handling of objects }
\value{ column names, values containing search string or entries not complete or multiple entries }
\references{ none }
\author{ Roland Rapold }
\note{ under continuous developement }
\seealso{ other utility-functions in this R-package }

\examples{
if(require("MASS")){
    # load example data
    data(crabs, package="MASS")
    head(crabs)
    #
    # example for 'grepColEntries'
    print(head(grepColEntries(crabs, "sex", "M")))
    print(head(grepColEntries(crabs, "index", "2")))
    #
    # example for 'grepColNames'
    print(grepColNames(crabs, "W"))
    #
    # example for 'grepColNA'
    print(grepColNA(crabs))
    print(grepColNA(c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6)))
    #
    # example for 'grepObjNames'
    print(grepObjNames("Pfad"))
    #
    # example for 'grepNotCompleteEntries'
    df <- crabs
    df[33, 4] <- NA
    df[55, 5] <- NA
    df[77, 6] <- NA
    df[99, 7] <- NA
    print(grepNotCompleteEntries(df))
    rm(df)
    #
    # example for 'grepMultipleEntries'
    # - with one column
    print(head(grepMultipleEntries(crabs, "FL"), n=10))
    #
    # - with two columns
    print(head(grepMultipleEntries(crabs, c("sex", "FL"))))
}
}
