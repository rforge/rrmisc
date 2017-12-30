# ==================================================================================================
# --------------- listObjSizes                      list all objects with their size -- in order  --# {{{
# RR 20130816  ---------------------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#' @title Show sizes of R-objects in Memory
#' 
#' @description Show sizes of R-objects in Memory
#' 
#' @details listObjSizes() returns a list of all objects in memory and lists them in order of
#' increasing size. Object size is reported by the function utils::object.size()
#' 
#' @param \dots arguments passed to further functions
#' @return List of sizes of R-objects in memory.
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso other utility-functions in this R-package
#' @references none
#' @examples
#' if(require("MASS")){
#'     data(crabs, package="MASS")
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
# Manual          ------------------------------------------------------------------------------- --
#' @title Get contrasts of data.frame
#' 
#' @description Get the contrasts of all factor variables in a data.frame.
#' 
#' @details Get the contrasts of all factor variables in a data.frame.
#' 
#' @param d.frame data.frame
#' @param \dots arguments passed to further functions
#' @return contrasts of factor variables of data.frame
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso other utility-functions in this R-package
#' @references none
#' @examples
#' if(require("MASS")){
#'     data(crabs, package="MASS")
#'     print(listObjContrasts(crabs))
#' }
#' @export
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
