# ==================================================================================================
# --------------- countDistinct                     Number of different Entries                   -- # {{{
# RR 20130920     ------------------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#'
#' @title  SQL-command 'count(distinct ...)' convreted to R
#' @description SQL-command 'count(distinct ...)' convreted to R
#' @details Returns the number of distinct values and indicates if there are NAs present
#' @param x Vector or data.frame in which the number of different entries are counted.
#' @param \dots arguments passed to further functions
#' @return named vector of numbers
#' @return - count:  number of different entries (without NAs)
#' @return - NAs:    index of NAs present (0=no, 1=yes)
#' @note under continuous developement
#' @author Roland Rapold
#' @references none
#' @examples
#' print("Example for vector")
#'                       c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6)
#'                length(c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6))
#'         countDistinct(c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6))
#'        length(na.omit(c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6)))
#' countDistinct(na.omit(c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6)))
#' #
#' if(require("MASS"))
#' {
#'     print("Example for data.frame")
#'     data(crabs, package="MASS")
#'     str(crabs)
#'     print(countDistinct(crabs[, c("sp")]))
#'     print(countDistinct(crabs[, c("sex")]))
#'     print(countDistinct(crabs[, c("FL")]))
#' }
#' @export
countDistinct <- function(x, ...) {
    #
    # method                                        ............................................. ..
    # - uses ( unique( ) ) as grouping function
    #
    # input                                         ............................................. ..
    # - vector
    # - dataframe
    #
    # output                                        ............................................. ..
    # - integer number of count and index of NA
    #
{
    naString <- ifelse(sum(is.na(x)) > 0, "yes", "no")
    if (is.null(dim(x))) {
        result <- c(length(unique(stats::na.omit(x))), naString)
    }
    else {
        result <- c(nrow(unique(stats::na.omit(x))), naString)
    }
    names(result) <- c("count", "NAs")
    return(result)
}}
# END OF FUNCTION  ------------------------------------------------------------------------------ --
# --------------- countDistinct  ---------------------------------------------------------------- --
## }}}
# --------------- compTwoVects                      Compare two vectors of the same length        .. # {{{
# RR 20160822     ------------------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#
#' @title Compare two numeric vectors
#'
#' @description Compare two numeric vectors of same length for NAs and equals.
#'
#' @param x vector one
#' @param y vector of same length as x
#' @param \dots arguments passed to further functions
#' @return comparisons of vector contents with respect to equality and NAs
#' @note under continuous developement
#' @author Roland Rapold
#' @references none
#' @examples
#' if(base::require("car")) {
#'    data(Mroz, package="car")
#'    str(Mroz)
#'    dim(Mroz)
#'    table(Mroz$wc, useNA="always")
#'    set.seed(1234567)
#'    Mroz[sample(nrow(Mroz), 5), "wc"] <- NA
#'    Mroz$wc_new <- Mroz$wc
#'    Mroz[sample(nrow(Mroz), 2), "wc"]      <- NA
#'    Mroz[sample(nrow(Mroz), 8), "wc_new"]  <- NA
#'    Mroz[sample(nrow(Mroz), 17), "wc_new"] <- "yes"
#'    table(Mroz$wc, useNA="always")
#'    table(Mroz$wc_new, useNA="always")
#'    #
#'    compTwoVects(Mroz$wc, Mroz$wc_new)
#' }
#' @export
compTwoVects <- function(x, y, ...)
{
    #
    # ----------------------------------------------------------------------------------------------
    # method
    # - compare two numeric vectors
    #
    # input
    # - x       = first vector
    # - y       = second vector
    #
    # output
    # - description of comparison of the two vectors
    # ----------------------------------------------------------------------------------------------
    #
    if(length(x)!=length(y))
    {
        return("the two vectors have not the same length")
    }
    #
    # compare NAs
    rows_na_1   <- is.na(x)
    rows_na_2   <- is.na(y)
    rows_na_12  <- sum(rows_na_1==TRUE & rows_na_2==TRUE)
    rows_na_1o2 <- sum(rows_na_1==TRUE | rows_na_2==TRUE)
    cat("compare NAs            ------------------------------------\n")
    cat(sprintf("%-45s", "length of vectors"), "\t", formatC(length(x), big.mark="'", width=10), "\n")
    cat(sprintf("%-45s", paste0("number of NAs in vector 1 ")), "\t", formatC(sum(rows_na_1), big.mark="'", width=10), "\n")
    cat(sprintf("%-45s", paste0("number of NAs in vector 2 ")), "\t", formatC(sum(rows_na_2), big.mark="'", width=10), "\n")
    cat(sprintf("%-45s", paste0("number of NAs in both vectors")), "\t", formatC(rows_na_12,     big.mark="'", width=10), "\n")
    cat(sprintf("%-45s", paste0("number of NAs in at least one vector")), "\t", formatC(rows_na_1o2, big.mark="'", width=10), "\n\n")
    #
    # compare NAs in one vector but not in the other
    cat("compare NAs in one but not the other vector ---------------")
    rows_na_1_notna_2 <- sum(rows_na_1==TRUE  & rows_na_2==FALSE)
    rows_notna_1_na_2 <- sum(rows_na_1==FALSE & rows_na_2==TRUE)
    cat("\n")
    cat(sprintf("%-45s", paste0("number of NAs in vector1 not in vector2")), "\t", formatC(rows_na_1_notna_2, big.mark="'", width=10), "\n")
    cat(sprintf("%-45s", paste0("number of NAs in vector2 not in vector1")), "\t", formatC(rows_notna_1_na_2, big.mark="'", width=10), "\n\n")
    #
    # compare equals (not NA in both vectors)
    cat("compare equals (not NA in both vectors) -------------------")
    x1 <- x[((1-rows_na_1) + (1-rows_na_2))==2]
    y1 <- y[((1-rows_na_1) + (1-rows_na_2))==2]
    length(x1)
    length(y1)
    length(x)
    length(y)
    n_equal     <- sum(x1==y1)
    n_not_equal <- sum(x1!=y1)
    cat("\n")
    cat(sprintf("%-45s", "rows excluding NAs in both vectors"),      "\t", formatC(length(x1), big.mark="'", width=10), "\n")
    cat(sprintf("%-45s", "number of equal entries in both vectors"), "\t", formatC(n_equal, big.mark="'", width=10), "\n")
    cat(sprintf("%-45s", "number of entries not equal in vectors"),  "\t", formatC(n_not_equal, big.mark="'", width=10), "\n\n")
    #
}
#
# --------------- compTwoVects ------------------------------------------------------------------ --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
# # }}}
# --------------- testGranularity                   Test granularity of attirbutes in data.frame  .. # {{{
# RR 20200630     ------------------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#
#' @title test granularity of attributes in data.frame
#'
#' @description Compare granularity of whole data.frame and of provided attributes
#'
#' @param d.data, data.frame
#' @param var, vector of attributes in d.data
#' @param \dots arguments passed to further functions
#' @return output describing the granularity
#' @note under continuous developement
#' @author Roland Rapold
#' @references none
#' @examples
#' d.data <- data.frame(a = 1:20,
#'                      b = rep(1:10, 2),
#'                      c = letters[1:20],
#'                      d = rep(letters[1:10], 2))
#' var <- c("a")
#' testGranularity(d.data = d.data, var = var)
#'
#' var <- c("b")
#' testGranularity(d.data = d.data, var = var)
#'
#' var <- c("a", "b")
#' testGranularity(d.data = d.data, var = var)
#'
#' var <- c("a", "b", "c", "d")
#' testGranularity(d.data = d.data, var = var)
#' @export
testGranularity <- function(d.data, var, ...)
{
  #
  # ----------------------------------------------------------------------------------------------
  # method
  # - test granularity of attributes in data.frame
  #
  # input
  # - d.data    = data.frame
  # - var       = vector of attributes to test
  #
  # output
  # - description of granularity of attributes in data.frame
  # ----------------------------------------------------------------------------------------------
  #
  # data.frame Syntax wird angewandt, so muss ein data.frame Obejkt erstellt werden
  if ("data.table" %in% class(d.data)) {
    class(d.data) <- "data.frame"
  }
  #
  # Eindeutigkeit ueberpruefen
  # t1 <- length(unique(d.data[, var[1]])) # erstes Attribut
  t1 <- nrow(d.data) # Dimension Datensatz
  t2 <- unique(d.data[, var]) # Kombinationen alle Attribute
  # unique(c(2, 3, 4, NA, 2, 3, 4, NA, NA)) # NA als ein Wert
  # [1] 2 3 4 NA
  if (is.null(dim(t2))) {
    t2 <- length(t2)
  } else {
    t2 <- nrow(t2)
  }
  if (t1 == t2) {
    print(paste("eindeutig in den Attributen --", paste(var, collapse = ", ")))
  } else {
    print(paste("nicht eindeutig in den Attributen --", paste(var, collapse = ", ")))
    print(paste("Dimension Datensatz --",
                formatC(t1, big.mark = "'", width = 10, format = "d")))
    #
    # NAs weggelassen
    print(paste("Kombinationen Attribute ohne NAs --",
                formatC(as.numeric(countDistinct(d.data[, var])[1]),
                        big.mark = "'", width = 10, format = "d")))
    #
    # NAs als ein Wert mitgezaehlt
    print(paste("Kombinationen Attribute mit NAs --",
                formatC(t2, big.mark = "'", width = 10, format = "d")))
    #
    # NAs weggelassen wenn ein anderer Wert vorhanden, NAs als Wert mitgezaehlt wenn kein anderer
    # Wert vorhanden
    if (length(var) == 2) {
      v1All <- unique(d.data[, var])
      v1 <- unique(d.data[, var[1]])
      # zweite Spalte hat nicht nur NAs
      # - NAs in zweiter Spalte weglassen und Kombinationen zaehlen
      v1NotOnlyNA <- unique(v1All[!is.na(v1All[, 2]), ][, 1])
      c1NotOnlyNA <- as.numeric(countDistinct(d.data[d.data[, var[1]] %in% v1NotOnlyNA, var])[1])
      # zweite Spalte hat nur NAs
      # - das sind schon alle Kombinationen
      v1OnlyNA <- v1[!v1 %in% v1NotOnlyNA]
      c1OnlyNA <- length(v1OnlyNA)
      print(paste("Kombinationen Attribute NAs selektiv --",
                  formatC(c1NotOnlyNA + c1OnlyNA, big.mark = "'", width = 10, format = "d")))
    }

    for (v in var)
    {
      # countDistinct(c(2, 3, 4, NA, 2, 3, 4, NA, NA)) # NA _nicht_ als ein Wert
      # count NAs
      # "3" "yes"
      cD <- countDistinct(d.data[, v])
      print(paste(sprintf("%-40s", paste("Kombinationen Attribut", v)), "--",
                  formatC(as.numeric(cD[1]), big.mark = "'", width = 10, format = "d"),
                  "-- NAs present --", cD[2]))
    }
  }
  # print(res)
  # return(res)
}


#
# --------------- testGranularity --------------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
# # }}}
# ==================================================================================================
