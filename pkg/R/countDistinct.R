# --------------- countDistinct                     Number of different Entries                   --
# RR 20130920     ------------------------------------------------------------------------------- --
#


#' SQL-command 'count(distinct ...)' convreted to R
#' 
#' SQL-command 'count(distinct ...)' convreted to R
#' 
#' 
#' @param x Vector or data.frame in which the number of different entries are counted.
#' @param \dots arguments passed to further functions
#' @return count (integer)
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
    # - integer number of count
    #
{

    naString <- ifelse(sum(is.na(x))>0, "plus NA", "")

    if(is.null(dim(x)))                             # x is vector

    {

        return(c(length(unique(stats::na.omit(x))), naString))

    }

    else {                                          # x is data.frame ...

        return(c(nrow(unique(stats::na.omit(x))), naString))

    }

}}
# END OF FUNCTION  ------------------------------------------------------------------------------ --
# --------------- countDistinct  ---------------------------------------------------------------- --
#
