# --------------- countDistinct                     Number of different Entries                   --
# RR 20130920     ------------------------------------------------------------------------------- --
#
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
    if(is.null(dim(x))) {
        return(length(unique(x)))
    } else {
        return(nrow(unique(x)))
    }
    #
}
# END OF FUNCTION  ----------------------------------------------------------------------------- --
# --------------- countDistinct  ---------------------------------------------------------------- --
#
