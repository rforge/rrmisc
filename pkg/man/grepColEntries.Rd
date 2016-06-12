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
print("-------------------------------------------------------------------------------------------")
if(require("MASS"))
{
    print("load example data                    ------------------------------------------------------")
    data(crabs, package="MASS")
    dim(crabs)
    head(crabs)
    #
    print("-------------------------------------------------------------------------------------------")
    print("example for 'grepColEntries'         ------------------------------------------------------")
    head(grepColEntries(crabs, "sex", "M"))
    head(grepColEntries(crabs, "index", "2"))
    #
    print("-------------------------------------------------------------------------------------------")
    print("example for 'grepColNames'           ------------------------------------------------------")
    grepColNames(crabs, "W")
    #
    print("-------------------------------------------------------------------------------------------")
    print("example for 'grepColNA'              ------------------------------------------------------")
    grepColNA(crabs)
    grepColNA(c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6))
    #
    print("-------------------------------------------------------------------------------------------")
    print("example for 'grepObjNames'           ------------------------------------------------------")
    grepObjNames("Pfad")
    #
    print("-------------------------------------------------------------------------------------------")
    print("example for 'grepNotCompleteEntries' ------------------------------------------------------")
    df <- crabs
    df[33, 4] <- NA
    df[55, 5] <- NA
    df[77, 6] <- NA
    df[99, 7] <- NA
    grepNotCompleteEntries(df)
    rm(df)
    #
    print("-------------------------------------------------------------------------------------------")
    print("example for 'grepMultipleEntries '   ------------------------------------------------------")
    print(" - with one column                   ------------------------------------------------------")
    head(grepMultipleEntries(crabs, "FL"), n=10)
    #
    print(" - with two columns                  ------------------------------------------------------")
    head(grepMultipleEntries(crabs, c("sex", "FL")))
}
}
