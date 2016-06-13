#
# ==================================================================================================
#
# --------------- grepColEntries                    show records with specified text              --
# RR 20140807               --------------------------------------------------------------------- --
#
grepColEntries <- function(d.frame, col.name, part.name, ...) {
    #
    # method                                        ............................................. ..
    # - use 'grep'
    #
    # input                                         ............................................. ..
    # - d.frame         data frame
    # - col.name        column in which to look
    # - part.name       character string to look for
    #
    # output                                        ............................................. ..
    # - records found
    #
    nr           <- grep(part.name, d.frame[, col.name])
    foundEntries <- cbind(nr, d.frame[grep(part.name, d.frame[, col.name]), ])
    #
    #
    return(foundEntries)
    #
}
# END OF FUNCTION  ----------------------------------------------------------------------------- --
# --------------- grepColEntries ---------------------------------------------------------------- --
#
# ==================================================================================================
#
# --------------- grepColNames                      show attributes containing specified text     --
# RR 20131122               --------------------------------------------------------------------- --
#
grepColNames <- function(d.frame, part.name, ...) {
    #
    # method                                        ............................................. ..
    # - use 'grep'
    #
    # input                                         ............................................. ..
    # - d.frame         data frame
    # - part.name       character string to look for in names()
    #
    # output                                        ............................................. ..
    # - attributes found; provide characteristics of attributes
    #
    ns <- names(d.frame)
    ni <- grep(part.name, ns)
    cl <- unlist(sapply(d.frame, class))
    ty <- unlist(sapply(d.frame, typeof))
    mo <- unlist(sapply(d.frame, mode))
    sm <- unlist(sapply(d.frame, storage.mode))
    #
    result <- data.frame(nr=ni
                         , names=ns[ni]
                         , class=cl[ni]
                         , typeof=ty[ni]
                         , mode=mo[ni]
                         , storage.mode=sm[ni])
    rownames(result) <- NULL
    #
    return(result)
    #
}
# END OF FUNCTION  ------------------------------------------------------------------------------ --
# --------------- grepColNames ------------------------------------------------------------------ --
#
# ==================================================================================================
#
# --------------- grepColNA                         show attributes containing 'NA's              --
# RR 20131122               --------------------------------------------------------------------- --
#
grepColNA <- function(x, ...) {
    #
    # method                                        ............................................. ..
    # -
    #
    # input                                         ............................................. ..
    # - x               data frame or numeric vector
    #
    # output                                        ............................................. ..
    # - attributes found
    #
    varlist <- ""
    #
    # data.frame
    if (is.data.frame(x)) {
        for (i in 1:ncol(x)) {
            if (sum(is.na(x[, i]))>0) {
                varlist <- c(varlist, colnames(x)[i])
            }
        }
        if(length(varlist)==1) {
            return("There are no NAs in this data frame!")
        }
        else {
            varlist <- varlist[-1]
            return(varlist)
        }
    # numeric
    } else {
        if (is.numeric(x)) {
            anz_na <- sum(is.na(x))
            if (anz_na>0) {
                return(paste("In this vector are", anz_na, "NAs pressent!"))
            }
            else {
                return("There are no NAs in this vector!")
            }
        }
        else {
            print("You need to provide a data frame or a numeric vetor to this function !")
            return()
        }
    }
    #
}
# END OF FUNCTION  ----------------------------------------------------------------------------- --
# --------------- grepColNA -------------------------------------------------------------------- --
#
# ==================================================================================================
#
# --------------- grepObjNames                      show object names containing specified text   --
# rr 20131129               --------------------------------------------------------------------- --
#
grepObjNames <- function(part.name, ...) {
    #
    # method                                        ............................................. ..
    # -
    #
    # input                                         ............................................. ..
    # - part.name        character string to look for in list of objects
    #
    # output                                        ............................................. ..
    # - objects found
    #
    if(!is.character(part.name)) return("Please provide a character string!")
    #
    ls <- ls(all.names=TRUE, envir=.GlobalEnv)
    ni <- grep(part.name, ls)
    #
    return(ls[ni])
    #
}
# END OF FUNCTION  ------------------------------------------------------------------------------ --
# --------------- grepobjnames ------------------------------------------------------------------ --
#
# ==================================================================================================
#
# --------------- grepNotCompleteEntries            show records with missing entries             --
# RR 20141021               --------------------------------------------------------------------- --
#
grepNotCompleteEntries <- function(d.frame, ...) {
    #
    # method                                        ............................................. ..
    # - use 'complete.cases'
    #
    # input                                         ............................................. ..
    # - d.frame       data frame
    #
    # output                                        ............................................. ..
    # - records found
    #
    if(!is.data.frame(d.frame)) return("Please provide a data frame!")
    #
    id <- which(!complete.cases(d.frame))
    #
    return(d.frame[id, ])
    #
}
# END OF FUNCTION  ------------------------------------------------------------------------------ --
# --------------- grepNotCompleteEntries -------------------------------------------------------- --
#
# ==================================================================================================
#
# --------------- grepMultipleEntries               find multiple records and show them           --
# RR 20140320               --------------------------------------------------------------------- --
#
grepMultipleEntries <- function(d.frame, var, ...) {
    #
    # method                                        ............................................. ..
    # - use 'duplicated'
    #
    # input                                         ............................................. ..
    # - d.frame         data frame
    # - var             variables for which multiple entries should be found
    #
    # output                                        ............................................. ..
    # - records found
    #
    # find first records of multiple ones           ................................................
    mult <- duplicated(d.frame[, var])
    mult <- unique(d.frame[mult, var])
    #
    a <- NULL
    b <- NULL
    c <- NULL
    d <- NULL
    e <- NULL
    f <- NULL
    #
    # select all records                            ................................................
    if(class(mult)=="data.frame"){                  # multiple attribures
        result <- merge(d.frame, mult)
        for(i in 1:length(var))
        {
            assign(letters[i], get("result")[, var[i]])
        }
        if(length(var)==2) result <- result[order(a, b), ]
        if(length(var)==3) result <- result[order(a, b, c), ]
        if(length(var)==4) result <- result[order(a, b, c, d), ]
        if(length(var)==5) result <- result[order(a, b, c, d, e), ]
        if(length(var)>=6) result <- result[order(a, b, c, d, e, f), ]

    } else {                                        # only one attribute
        result <- d.frame[d.frame[, var]%in%mult, ]
        result <- result[order(result[, var]), ]
    }
    result <- result[, colnames(d.frame)]
    #
    return(result)
    #
}
# END OF FUNCTION  ------------------------------------------------------------------------------ --
# --------------- grepMultipleEntries  ---------------------------------------------------------- --
#
# ==================================================================================================
#
# --------------- grepRowNames                      show rownames containing specified text       --
# RR 20131122               --------------------------------------------------------------------- --
#
grepRowNames <- function(d.frame, part.name, ...) {
    #
    # method                                        ............................................. ..
    # - use 'grep'
    #
    # input                                         ............................................. ..
    # - d.frame         data frame
    # - part.name       character string to look for in names()
    #
    # output                                        ............................................. ..
    # - row names found
    #
    ns <- row.names(d.frame)
    ni <- grep(part.name, ns)
    #
    result <- data.frame(nr=ni
                         , names=ns[ni]
                         )
    rownames(result) <- NULL
    #
    return(result)
    #
}
# END OF FUNCTION  ------------------------------------------------------------------------------ --
# --------------- grepRowNames ------------------------------------------------------------------ --
#
# ==================================================================================================
#
