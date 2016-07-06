\name{pFromT}
\alias{pFromT}
\alias{pFromZ}
\alias{pFromF}
\title{
Calculate p-values from t-, z- or F-statistics.
}
\description{
Calculate p-values from t-, z- or F-statistics.
}
\usage{
pFromZ(z_value = 1.96, two.sided = FALSE)
pFromT(t_value = 1.96, df = 10, two.sided = FALSE)
pFromF(f_value = 1.96, df1 = 10, df2 = 10, two.sided = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t_value, z_value, f_value}{
      t-, z- or F-statistics to be converted into a p-Value
%%     ~~Describe \code{t_value} here~~

}
  \item{df, df1, df2}{
      df: degree of freedom in t-statistics
      df1, df2: degrees of freedom in F-statistics
%%     ~~Describe \code{df} here~~
}
  \item{two.sided}{
      shall the statistics be calculatede as two-sided (TURE) or one-sided (FALSE)
%%     ~~Describe \code{two.sided} here~~
}
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
\value{
    corresponding p-Value from the t-, z- or F-statistics
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
\author{
    Roland Rapold
%%  ~~who you are~~
}
%% \note{
%% %%  ~~further notes~~
%% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,

        pFromT(t_value=1.96, df=10)
        pFromT(t_value=1.96, df=100)
        pFromT(t_value=1.96, df=1000)
        pFromT(t_value=1.96, df=1000, two.sided = TRUE)

        pFromZ(z_value=1.96)
        pFromZ(z_value=1.96, two.sided=TRUE)

        pFromF(f_value=35.68, df1=2,  df2=5)
        pFromF(f_value=35.68, df1=2,  df2=5, two.sided=TRUE)
        pFromF(f_value= 1.96, df1=10, df2=10)
        pFromF(f_value= 1.96, df1=10, df2=100)
        pFromF(f_value= 1.96, df1=10, df2=1000)
        pFromF(f_value= 1.96, df1=10, df2=1000, two.sided=TRUE)
        pFromF(f_value= 1.96, df1=10, df2=1000, two.sided=TRUE)

        x <- c(seq(0.0001, 0.6, 0.01), NA, seq(0.6001, 1.0, 0.01))
        p_value <- pFromT(t_value=x, df=100)
        p_value
        formatPValue(p_value, digits=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ p-value }% __ONLY ONE__ keyword per line
