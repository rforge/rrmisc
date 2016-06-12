\name{colThemes}
\alias{colThemes}
\alias{defaultTheme}
\alias{helRot4Theme}
\alias{helRot5Theme}
\alias{helBlau4Theme}
\alias{helBlau5Theme}
\alias{helOlive4Theme}
\alias{helOlive5Theme}
\alias{helOrange4Theme}
\alias{helOrange5Theme}
\alias{helsanaTheme}
\alias{helsanaTheme3}
\alias{helEconTheme}
\docType{data}
\title{ lattice - color themes }
\description{ colot themes to use with 'lattice'. }
\usage{
    library(lattice)
    data(colThemes)
    trellis.par.set(colThemes$defaultTheme);          show.settings()
    trellis.par.set(colThemes$helRot4Theme);          show.settings()
}
\arguments{
  \item{theme}{ one of several predefined lattice color themes }
  \item{\dots}{ arguments passed to further functions }
}
\format{ lattict themes }
\details{ }
\source{ }
\references{ }
\examples{
\dontrun{
    require(lattice)
    data(colThemes)
    names(colThemes)

    trellis.par.set(colThemes$defaultTheme);          show.settings()
    trellis.par.set(colThemes$helRot4Theme);          show.settings()
    trellis.par.set(colThemes$helRot5Theme);          show.settings()
}
}
\keyword{lattice, graphics}
