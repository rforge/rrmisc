\name{listObjSizes}
\alias{listObjSizes}
\alias{listObjContrasts}
\title{get properties of objects easily.}
\description{get sizes and contrasts of objects.}
\usage{ listObjSizes(...) }
\arguments{
  \item{\dots}{ arguments passed to further functions }
}
\details{function for the description of properties of objects rather than processing of data.}
\value{sizes and contrasts of loaded objects}
\references{ none }
\author{ Roland Rapold }
\note{ under continuous developement }
\seealso{ other utility-functions in this R-package }
\examples{
if(require("MASS")){
    # Example - loading data
    data(crabs, package="MASS")
    print(head(crabs))
    #
    # Example for 'listObjContrasts
    print(listObjContrasts(crabs))
    #
    # Example for 'listObjSizes'
    print(listObjSizes())
}
}
