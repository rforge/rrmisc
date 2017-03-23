#
# ==================================================================================================
#
# --------------- countDistinct                     Number of different Entries                   -- # {{{
# RR 20130920     ------------------------------------------------------------------------------- --
#


#' SQL-command 'count(distinct ...)' convreted to R
#' 
#' SQL-command 'count(distinct ...)' convreted to R
#' 
#' 
#' @param x Vector or data.frame in which the number of different entries are counted.
#' @param \dots arguments passed to further functions
#' @return named vector of numbers
#' @return - count:  number of different entries (without NAs)
#' @return - NAs:    index of NAs present (0=no, 1=yes)
#' @note under continuous developement
#' @author Roland Rapold
#' @references none
#' @export
#' @examples
#' 
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
#' 
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
    naString <- ifelse(sum(is.na(x)) > 0, 1, 0)
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
# ==================================================================================================
#
# --------------- compTwoCols                    Compare two columns of a data.frame              .. # {{{
# RR 20160822     ------------------------------------------------------------------------------- --
#


#' Compare two columns of a data.frame
#' 
#' Compare two columns of a data.frame
#' 
#' 
#' @param x vector one
#' @param y vector of same length as x
#' @param \dots arguments passed to further functions
#' @return comparisons of vector contents with respect to equality and NAs
#' @note under continuous developement
#' @author Roland Rapold
#' @references none
#' @export
#' @examples
#' 
#' if(base::require("Mroz")) {
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
#' 
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
    # - description of comparison of the two columns
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
    cat(sprintf("%-45s", paste0("number of NAs in both columns")), "\t", formatC(rows_na_12,     big.mark="'", width=10), "\n")
    cat(sprintf("%-45s", paste0("number of NAs in at least one column")), "\t", formatC(rows_na_1o2, big.mark="'", width=10), "\n\n")
    #
    # compare NAs in one column but not in the other
    cat("compare NAs in one but not the other vector ---------------")
    rows_na_1_notna_2 <- sum(rows_na_1==TRUE  & rows_na_2==FALSE)
    rows_notna_1_na_2 <- sum(rows_na_1==FALSE & rows_na_2==TRUE)
    cat("\n")
    cat(sprintf("%-45s", paste0("number of NAs in vector1 not in vector2")), "\t", formatC(rows_na_1_notna_2, big.mark="'", width=10), "\n")
    cat(sprintf("%-45s", paste0("number of NAs in vector2 not in vector1")), "\t", formatC(rows_notna_1_na_2, big.mark="'", width=10), "\n\n")
    #
    # compare equals (not NA in both columns)
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
# --------------- cite.by.name ------------------------------------------------------------------ --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
# # }}}
# ==================================================================================================
