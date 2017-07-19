#
# ==================================================================================================
#
# --------------- listObjSizes                      list all objects with their size -- in order  --# {{{
# RR 20130816  ---------------------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#' @title get properties of objects easily.
#' 
#' @description get sizes and contrasts of objects.
#' 
#' @details function for the description of properties of objects rather than processing of data.
#' 
#' @aliases listObjSizes listObjContrasts
#' @export  listObjSizes listObjContrasts
#' @param \dots arguments passed to further functions
#' @return sizes and contrasts of loaded objects
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso other utility-functions in this R-package
#' @references none
#' @examples
#' if(require("MASS")){
#'     # Example - loading data
#'     data(crabs, package="MASS")
#'     print(head(crabs))
#'     #
#'     # Example for 'listObjContrasts
#'     print(listObjContrasts(crabs))
#'     #
#'     # Example for 'listObjSizes'
#'     print(listObjSizes())
#' }
#' @export
listObjSizes <- function(...) {
    #
    # method                                        ............................................. ..
    # - use 'object.size'
    #
    # input                                         ............................................. ..
    #
    # output                                        ............................................. ..
    # - names of all objects
    #
    obj   <- ls(envir=.GlobalEnv)
    objsz <- data.frame(name=1, size=1)

    for(i in 1:length(obj))
    {
        objsz[i, ] <- c(obj[i], utils::object.size(get(obj[i]))[1])
    }
    #
    objsz$size <- as.integer(objsz$size)
    objsz <- objsz[order(objsz$size, decreasing=FALSE), ]
    return(objsz)
    #
}
# END OF FUNCTION  ----------------------------------------------------------------------------- --
# --------------- listObjSizes ----------------------------------------------------------------- --
## }}}
# --------------- listObjContrasts                  list contrasts of all factor attributes       --# {{{
# RR 20141027  ---------------------------------------------------------------------------------- --
#
listObjContrasts <- function(d.frame, ...) {
    #
    # method                                        ............................................. ..
    # - use 'contrasts'
    #
    # input                                         ............................................. ..
    #
    # output                                        ............................................. ..
    # - contrasts of all factors in data.frame
    #
    if(!is.data.frame(d.frame)) return("please provide a data frame!")
    #
    factor_var <- which(lapply(d.frame, class)=="factor")
    if(length(names(factor_var))==0) return("no factors in data frame!")
    #
    contrasts <- lapply(d.frame[, names(factor_var)], contrasts)
    #
    return(contrasts)
    #
}
# END OF FUNCTION  ----------------------------------------------------------------------------- --
# --------------- listObjContrasts ------------------------------------------------------------- --
## }}}
# ==================================================================================================
