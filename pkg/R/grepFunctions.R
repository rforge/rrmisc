#
# ==================================================================================================
#
# --------------- grepColEntries                    show entries in data.frame with search string -- # {{{
# RR 20140807               --------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#' @title obtain grep components of object easily
#' 
#' @description obtain grep components of object easily
#' 
#' @details functions facilitating handling of objects
#' 
#' @aliases grepColEntries grepColNames grepColNA grepObjNames grepNotCompleteEntries grepMultipleEntries grepRowNames grepColNegNum grepColFactors
#' @export  grepColEntries grepColNames grepColNA grepObjNames grepNotCompleteEntries grepMultipleEntries grepRowNames grepColNegNum grepColFactors
#' @param d.frame R-object
#' @param col.name Name of column
#' @param part.name Search string in column names
#' @param \dots Arguments passed to further functions
#' @return Column names, values containing search string or entries not complete or multiple entries
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso other utility-functions in this R-package
#' @references none
#' @examples
#' if(require("MASS"))
#' {
#'     # load example data
#'     data(crabs, package="MASS")
#'     head(crabs)
#'     #
#'     # example for grepColEntries()
#'     print(head(grepColEntries(crabs, "sex", "M")))
#'     print(head(grepColEntries(crabs, "index", "2")))
#'     #
#'     # example for grepColNames()
#'     print(grepColNames(crabs, "W"))
#'     #
#'     # example for grepColNA()
#'     print(grepColNA(crabs))
#'     print(grepColNA(c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6)))
#'     #
#'     # example for 'grepObjNames'
#'     print(grepObjNames("Pfad"))
#'     #
#'     # example for grepNotCompleteEntries()
#'     df <- crabs
#'     df[33, 4] <- NA
#'     df[55, 5] <- NA
#'     df[77, 6] <- NA
#'     df[99, 7] <- NA
#'     print(grepNotCompleteEntries(df))
#'     rm(df)
#'     #
#'     # example for grepMultipleEntries()
#'     # - with one column
#'     print(head(grepMultipleEntries(crabs, "FL"), n=10))
#'     #
#'     # - with two columns
#'     print(head(grepMultipleEntries(crabs, c("sex", "FL"))))
#'     #
#'     # example for grepColNegNum()
#'     if(require("car"))
#'     {
#'       data(Mroz, package="car")
#'       print(grepColNegNum(Mroz))
#'     }
#'     #
#'     # example for grepColFactors()
#'     if(require("car"))
#'     {
#'       data(Mroz, package="car")
#'       Mroz$wc_ord <- as.ordered(Mroz$wc)
#'       print(grepColFactors(Mroz))
#'     }
#'     #
#' }
#' @export
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
## }}}
# --------------- grepColNames                      show attributes containing specified text     -- # {{{
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
    cl <- unlist(lapply(sapply(d.frame, class), paste, collapse=" - "))
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
## }}}
# --------------- grepColNA                         show attributes containing 'NA's              -- # {{{
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
## }}}
# --------------- grepObjNames                      show object names containing specified text   -- # {{{
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
# ==================================================================================================
## }}}
# --------------- grepNotCompleteEntries            show records with missing entries             -- # {{{
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
    id <- which(!stats::complete.cases(d.frame))
    #
    return(d.frame[id, ])
    #
}
# END OF FUNCTION  ------------------------------------------------------------------------------ --
# --------------- grepNotCompleteEntries -------------------------------------------------------- --
# ==================================================================================================
## }}}
# --------------- grepMultipleEntries               find multiple records and show them           -- # {{{
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
# ==================================================================================================
## }}}
# --------------- grepRowNames                      show rownames containing specified text       -- # {{{
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
# ==================================================================================================
## }}}
# --------------- grepColNegNum                     show columns with negative values             .. # {{{
# RR 20130920     ------------------------------------------------------------------------------- --
#
grepColNegNum <- function(x) {
    #
    # ----------------------------------------------------------------------------------------------
    # method
    # - return columns containing negative numbers
    #
    # input
    # - x = data.frame or numeric vector
    #
    # output
    # - column names wich negative entries
    # ----------------------------------------------------------------------------------------------
    #
    list.neg.num <- NULL
    #
    if (is.data.frame(x))
    {
        # apply method to data.frame() .............................................................
        var.list <- data.frame(var_name=names(x), var_type="")
        var.list$var_type <- unlist(sapply(x[, var.list$var_name], class))
        var.num  <- var.list$var_type%in%c("numeric", "integer")
        var.num  <- var.list$var_name[var.num]
        for(i in var.num)
        {
            if(min(x[, i], na.rm=TRUE)<0)
            {
                list.neg.num <- c(list.neg.num, i)
            }
        }
        if (length(list.neg.num) == 0) {
            return("no columns with negative numbers present!")
        } else {
            list.neg.num <- data.frame(nr=which(names(x)%in%list.neg.num),
                                       name=list.neg.num)
            return(list.neg.num)
        }
    } else {
        # apply method to numeric vector ...........................................................
        if(is.numeric(x))
        {
            if(min(x, na.rm=TRUE)<0)
            {
                return("in this vector are negative numbers present!")
            } else {
                return("in this vector are no negative numbers present!")
            }
        }
        else {
            print("please provide a data.frame or numeric vector!")
            return()
        }
    }
    #
}
#
# --------------- grepColNegNum  ---------------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
# # }}}
# --------------- grepColFactors                    show factor columns in data.frame             .. # {{{
# RR 20150601     ------------------------------------------------------------------------------- --
grepColFactors <- function(x) {
    #
    # ----------------------------------------------------------------------------------------------
    # method
    # - return factor attributes with position
    #
    # input
    # - x = data.frame
    #
    # output
    # - column numbers with facotr variables and indicator for ordered factors
    # ----------------------------------------------------------------------------------------------
    #
    # Check data object ............................................................................
    if(!is.data.frame(x))
    {
        return("please provide a data.frame as object x")
    }
    # Listen mit Indikatoren von Faktoren und geordneten Faktoren in angezeigtem data.frame ........
    dfClasses <- lapply(x, class)
    dfFactors <- lapply(dfClasses, function(x) max(x=="factor"))
    dfOrdered <- lapply(dfClasses, function(x) max(x=="ordered"))
    #
    aa <- unlist(dfFactors) == 1
    ab <- unlist(dfOrdered) == 1
    #
    # Resultat - data.frame erstellen ..............................................................
    na <- data.frame(nr=which(aa), factor=names(aa[aa]))
    na <- merge(na, data.frame(id=names(ab[ab]), ordered=names(ab[ab])),
                by.x="factor", by.y="id", all.x=TRUE)
    na[is.na(na$ordered), "ordered"] <- "-"
    na <- na[order(na$nr), ]
    na <- na[, c("nr", "factor", "ordered")]
    rownames(na) <- 1:nrow(na)
    #
    return(na)
    #
}
#
# --------------- grepColFactors ------------------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
# # }}}
# ==================================================================================================
