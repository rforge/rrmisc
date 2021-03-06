% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify.R
\name{naToZero}
\alias{naToZero}
\title{Substitution of NAs in data.frame or vector}
\usage{
naToZero(dataf, substit_numeric = TRUE, substit_character = FALSE, ...)
}
\arguments{
\item{dataf}{data.frame or vector}

\item{substit_numeric}{shall numeric fields (integer, numerical) be substituted?}

\item{substit_character}{shall character fields be substituted?}

\item{\dots}{arguments passed to further functions}
}
\value{
corresponding data.frame or vector with substituted NAs
}
\description{
Substitution of NAs in a data.frame or a vector. NAs in numeric fields (integer,
numeric) are substituted by 0, NAs in caracter fields are substituted by '-'.
}
\details{
utility function for treating NAs
}
\examples{
        (x <- c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6))
        naToZero(x)
        naToZero(x, substit_numeric=TRUE)
        naToZero(x, substit_numeric=FALSE)

        (x <- c("pi", NA, "4.000000", "5.000000", "5", "6", "5", "6"))
        naToZero(x)
        naToZero(x, substit_character=TRUE)
        naToZero(x, substit_character=FALSE)
}
\author{
Roland Rapold
}
\keyword{NA}
