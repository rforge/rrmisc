#
# ==================================================================================================
#
# --------------- defObjList                        take snapshot of current object names         --
# RR 20130920     ------------------------------------------------------------------------------- --
#
defObjList <- function (name = "A", ...) {
    #
    # method                                        ............................................. ..
    # -
    #
    # input                                         ............................................. ..
    # -
    #
    # output                                        ............................................. ..
    # - snapshot list name 'objList_' ...
    #
    objListName <- paste("objList", name, sep = "")
    assign(objListName, ls(all.names = TRUE, envir = .GlobalEnv), envir = .GlobalEnv)
    invisible(get(objListName))
    #
}
# END OF FUNCTION  ----------------------------------------------------------------------------- --
# --------------- defObjList -------------------------------------------------------------------- --
#
# ==================================================================================================
#
# --------------- setObjList                        delete all but the objects in the snapshot list-
# RR 20130920     ------------------------------------------------------------------------------- --
#
setObjList <- function (name = "A", ...) {
    #
    # method                                        ............................................. ..
    # -
    #
    # input                                         ............................................. ..
    # - snapshot list name 'objList_' ...
    #
    # output                                        ............................................. ..
    # -
    #
    objList_n <- paste("objList", name, sep = "")
    if(!exists(objList_n)) return(paste("object list", objList_n, "does not exist"))
    #
    objList_x <- get(objList_n)
    objList_z <<- ls(all.names = TRUE, envir = .GlobalEnv)
    objDel <<- objList_z[!objList_z %in% objList_x]
    if (length(which(objDel == objList_n)) > 0) {
        objDel <<- objDel[-which(objDel == objList_n)]
    }
    objDel <<- c(objDel, "objDel")
    objDel <<- c(objDel, "objList_z")
    rm(list = objDel, envir = .GlobalEnv)
    #
}
# END OF FUNCTION  ----------------------------------------------------------------------------- --
# --------------- setObjList -------------------------------------------------------------------- --
#
# ==================================================================================================
#
