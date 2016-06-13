\name{descrTable}
\alias{descrTable}
\alias{createDefMeasures}
\alias{descrMeasures}
\title{ Create descritive table }
\description{ Create descriptive table with up to five subgroups/total and comparison between
    groups. }
\usage{ formatPValue(x, digits=3, ...) }
\arguments{
  \item{descr.table}{ description scheme with measures, e.g. from createDefMeasures() }
  \item{sub.d1 to sub.d5}{ vectors of data in columns 1 to 5 }
  \item{test.gr}{columns to compare}
  \item{verbose}{verbose level between 0 and 2}
  \item{weights.1 to weights.5}{ vectors of weights of data columns 1 to 5}
  \item{lalbel.compact}{compact result columns label and measure or make two columns}
  \item{label.width}{width of first column if label.compact==TRUE}
  \item{lang}{language for certain key words, 'de' or 'en'}
  \item{\dots}{ arguments passed to further functions }
}
\details{ utility function for formating }
\value{ list with 1. descriptive table, 2. tests used, 3. attributes with NA's }
\references{ none }
\author{ Roland Rapold }
\note{ under continuous developement }
\seealso{ other utility-functions in this R-package }
\examples{
\dontrun{
require("car")
data(Mroz)
str(Mroz)

# insert attribute 'i' for population size              ........................................
Mroz <- merge(data.frame(i=as.factor(1)), Mroz)

# reference list for variable labels                    ........................................
var.list <- data.frame(var_name=c("i", "lfp", "k5", "k618", "age", "wc", "lwg", "inc"),
                       var_label=c("population",
                                   "employed",
                                   "# children to 5", "# children 6-18",
                                   "age",
                                   "wife college",
                                   "log expected wage",
                                   "family income excl. wife"))

# create definition-table without variable for groups   ........................................
Mroz_hc      <- Mroz[ , -which(colnames(Mroz)=="hc")]
#   - variation 1: default, no adjustments
def.measures <- createDefMeasures(d.data=Mroz_hc)
#   - variation 2: apply variable labels
def.measures <- createDefMeasures(d.data=Mroz_hc, var.list=var.list)
def.measures[2, 6] <- -2
def.measures[9, 6] <- -1

# modify data                                           ........................................
#   - variation 1: default, no adjustments

#   - variation 2: insert some missing values for 'lfp'
#                   3 at Mroz$lfp where hc='no'
n1 <- sample(1:length(Mroz[Mroz$hc=="no", "lfp"]), 3)
Mroz[Mroz$hc=="no", "lfp"][n1] <- NA
#                   2 at Mroz$lfp where hc='yes'
n2 <- sample(1:length(Mroz[Mroz$hc=="yes", "lfp"]), 2)
Mroz[Mroz$hc=="yes", "lfp"][n2] <- NA

#   - variation 3: insert some missing values for 'age'
#                   1 at Mroz$lfp where hc='no'
n3 <- sample(1:length(Mroz[Mroz$hc=="no", "age"]), 1)
Mroz[Mroz$hc=="no", "age"][n3] <- NA
#                   5 at Mroz$lfp where hc='no'
n4 <- sample(1:length(Mroz[Mroz$hc=="yes", "age"]), 5)
Mroz[Mroz$hc=="yes", "age"][n4] <- NA

# small sample size to provoke warnings in tests
# Mroz <- Mroz[1:10, ]

# create description-table                              ........................................
descr.table <- descrTable(def.measures = def.measures
                        , sub.d1 = subset(Mroz, hc=="no")
                        , sub.d2 = subset(Mroz, hc=="yes")
                        , sub.d3 = Mroz
                        , test.gr = c(1, 2)
                        , lang="en")
colnames(descr.table[[1]]) <- c("attribute", "measure"
                        , "husband college 'no'", "husband college 'yes'"
                        , "overall", "p-value")
descr.table
head(descr.table[[1]])

# verify statistics                                     ........................................
if(require("doBy"))
{
    doBy::summaryBy(k5+k618+age+lwg+inc~hc, data=Mroz, FUN=c(mean, median))
    table(Mroz$lfp, Mroz$hc, dnn=c("lfp", "hc"))
    table(Mroz$wc, Mroz$hc, dnn=c("lfp", "hc"))
}
} }

