\name{colThemes}
\alias{colThemes}
\docType{data}
\title{ lattice - color themes }
\description{ color themes to use with 'lattice'. }
\usage{
    data(colThemes)
}
%%    trellis.par.set(colThemes$defaultTheme);           show.settings()
%%    trellis.par.set(colThemes$Rot4Theme);              show.settings()
%%    trellis.par.set(colThemes$Rot5Theme);              show.settings()
%%    trellis.par.set(colThemes$Blau4Theme);             show.settings()
%%    trellis.par.set(colThemes$Blau5Theme);             show.settings()
%%    trellis.par.set(colThemes$Olive4Theme);            show.settings()
%%    trellis.par.set(colThemes$Olive5Theme);            show.settings()
%%    trellis.par.set(colThemes$Orange4Theme);           show.settings()
%%    trellis.par.set(colThemes$Orange5Theme);           show.settings()
%%    trellis.par.set(colThemes$Theme3);                 show.settings()
%%    trellis.par.set(colThemes$Theme4);                 show.settings()
%%    trellis.par.set(latticeExtra::theEconomist.theme); show.settings()
%% \arguments{
%%   \item{theme}{ one of several predefined lattice color themes }
%%   \item{\dots}{ arguments passed to further functions }
%% }
%% \format{ lattice themes }
\details{ The following color schemes for usage with 'lattice' have been defined:
\tabular{rll}{
     1 \tab defaultTheme  \tab default colors in 'lattice'\cr
     2 \tab Rot4Theme     \tab red with four colors\cr
     3 \tab Rot5Theme     \tab red with five colors\cr
     4 \tab Blau4Theme    \tab blue with four colors\cr
     5 \tab Blau5Theme    \tab blue with five colors\cr
     6 \tab Olive4Theme   \tab olive with four colors\cr
     7 \tab Olive5Theme   \tab olive with five colors\cr
     8 \tab Orange4Theme  \tab orange with four colors\cr
     9 \tab Orange5Theme  \tab orange with five colors\cr
    10 \tab Theme3        \tab basic three color scheme\cr
    11 \tab Theme4        \tab basic four color scheme
    }
}
%% \source{ }
%% \references{ }
\examples{
if(require("lattice"))
{
    data(colThemes)
    names(colThemes)

    trellis.par.set(colThemes$defaultTheme);       show.settings()
    Sys.sleep(5)
    trellis.par.set(colThemes$Rot4Theme);          show.settings()
    Sys.sleep(5)
    trellis.par.set(colThemes$Rot5Theme);          show.settings()
}
}
\keyword{lattice}
\keyword{graphics}
