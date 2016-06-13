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
\alias{createColorScheme }
\docType{data}
\title{ lattice - color themes }
\description{ colot themes to use with 'lattice'. }
\usage{
    library("lattice")
    data(colThemes)
    lattice::trellis.par.set(colThemes$defaultTheme);          lattice::show.settings()
    lattice::trellis.par.set(colThemes$helRot4Theme);          lattice::show.settings()
}
\arguments{
  \item{theme}{ one of several predefined lattice color themes }
  \item{\dots}{ arguments passed to further functions }
}
\format{ lattict themes }
%% \details{ }
%% \source{ }
%% \references{ }
\examples{
\dontrun{
    require("lattice")
    data(colThemes)
    names(colThemes)

    lattice::trellis.par.set(colThemes$defaultTheme);          lattice::show.settings()
    lattice::trellis.par.set(colThemes$helRot4Theme);          lattice::show.settings()
    lattice::trellis.par.set(colThemes$helRot5Theme);          lattice::show.settings()
}
}
\keyword{lattice}
\keyword{graphics}
