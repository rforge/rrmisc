# ==================================================================================================
# --------------- countDistinct                     Number of different Entries                   --{{{
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
# --------------- compTwoNumVects                   Compare two vectors of the same length        ..{{{
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
#'    data(Mroz, package="carData")
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
#'    compTwoNumVects(Mroz$wc, Mroz$wc_new)
#' }
#' @export
compTwoNumVects <- function(x, y, ...)
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
# --------------- compTwoNumVects --------------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
# # }}}
# --------------- compTwoCharVects                  Vergleich von zwei Zeichenvektoren            ..{{{
# xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
# RR 20160822     ------------------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#
#' @title Compare two string vectors
#'
#' @description Compare two string vectors
#'
#' @param vec1 vector one
#' @param vec2 vector two
#' @param showElements TRUE/FALSE for showing entries
#' @param \dots arguments passed to further functions
#' @return comparisons of vector contents with respect to equality and optionally show entries
#' @note under continuous developement
#' @author Roland Rapold
#' @references none
#' @examples
#' if(base::require("car")) {
#'    data(Mroz, package = "carData")
#'    str(Mroz)
#'    Mroz$wc <- as.character(Mroz$wc)
#'    Mroz$hc <- as.character(Mroz$hc)
#'    compTwoCharVects(Mroz$wc, Mroz$hc)
#' }
#' @export
compTwoCharVects <- function(vec1, vec2, showElements = FALSE, ...)
{
  if (!(is.character(vec1) & is.character(vec2))) {
    print("Bitte zwei Zeichenvektoren \u00fcbergeben")
    return()
  }
  print(paste("L\u00e4nge Vektor 1                            --", formatC(length(vec1), big.mark = "'", width = 8)))
  print(paste("L\u00e4nge Vektor 2                            --", formatC(length(vec2), big.mark = "'", width = 8)))

  lengU1 <- length(unique(vec1))
  lengU2 <- length(unique(vec2))
  print(paste("Anzahl verschiedene Elemente in Vektor 1  --", formatC(lengU1, big.mark = "'", width = 8)))
  print(paste("Anzahl verschiedene Elemente in Vektor 2  --", formatC(lengU2, big.mark = "'", width = 8)))

  if (max(lengU1, lengU2) <= 20) {
    unique1 <- unique(vec1)
    unique2 <- unique(vec2)
    unique1 <- paste(unique1[order(unique1)], collapse = ", ")
    unique2 <- paste(unique2[order(unique2)], collapse = ", ")
    print(paste("verschiedene Elemente in Vektor 1         --", format(unique1, justify = "right", width = 8)))
    print(paste("verschiedene Elemente in Vektor 2         --", format(unique2, justify = "right", width = 8)))
  }

  v1_in_v2  <- vec1[ vec1 %in% vec2]
  v1_Nin_v2 <- vec1[!vec1 %in% vec2]
  v2_Nin_v1 <- vec2[!vec2 %in% vec1]

  print(paste("Anzahl Elemente Vektor 1 und in Vektor2   --", formatC(length(v1_in_v2),  big.mark = "'", width = 8)))
  print(paste("Anzahl Elemente Vektor 1 nicht in Vektor2 --", formatC(length(v1_Nin_v2), big.mark = "'", width = 8)))
  print(paste("Anzahl Elemente Vektor 2 nicht in Vektor1 --", formatC(length(v2_Nin_v1), big.mark = "'", width = 8)))

  if (showElements) {
    print("Elemente Vektor 1 und in Vektor2          --")
    print(paste(v1_in_v2, collapse = ", "))

    print("Elemente Vektor 1 nicht in Vektor2        --")
    print(paste(v1_Nin_v2, collapse = ", "))

    print("Elemente Vektor 2 nicht in Vektor1        --")
    print(paste(v2_Nin_v1, collapse = ", "))
  }
}
#
# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#}}}
# ==================================================================================================
