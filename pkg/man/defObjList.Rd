\name{defObjList}
\alias{defObjList}
\alias{setObjList}
\title{Liste von R-Objekten verwalten.}
\description{Liste von R-Objekten verwalten. Es k?nnen Listen mit Namen definiert werden.}
\usage{ defObjList(name="A"), setObjList(name="A") }
\arguments{
  \item{name}{ Listennamen }
  \item{\dots}{ arguments passed to further functions }
}
\details{ Diese Funktionen verwalten Objektlisten, so dass im Verlauf einer Analyse auf einen
bestimmten, vorherigen Stand der vorhandenen Objekte (nach Objektname) zur?ckgegangen werden kann. }
\value{ Neue Objekte nach Definition des Standes werden gel?scht. }
\references{ Keine }
\author{ Roland Rapold }
\note{ Funktionen sind st?ndig in Entwicklung. }
\seealso{ Weitere Funktionen in diesem R-Paket. }
\examples{
\dontrun{
defObjList(name="A")
setObjList(name="A")
}
}
