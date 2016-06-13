\name{defObjList}
\alias{defObjList}
\alias{setObjList}
\title{Manage list of R-Objects}
\description{Manage list of R-Objects -- lists can be named as will}
\usage{
    defObjList(name="A")
    setObjList(name="A")
}
\arguments{
  \item{name}{ list name }
  \item{\dots}{ arguments passed to further functions }
}
\details{ With this function one can manage list of object so that one can jump to a former number
    of objects. }
\value{ Newer objects than the ones in the list are removed }
\references{ none }
\author{ Roland Rapold }
\note{ function under continuous developement }
\seealso{ other functiond in this package }
\examples{
\dontrun{
    defObjList(name="A")
    setObjList(name="A")
}
}
