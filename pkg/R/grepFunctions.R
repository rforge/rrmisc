# ==================================================================================================
# --------------- grepColEntries                    show entries in data.frame with search string -- # {{{
# RR 20140807               --------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#' @title Subset of data.frame containing string
#'
#' @description Get a subset of a data.frame with specified string in a given column.
#'
#' @details Unlike subset which works with '==' or '\%in\%', here search is performed using grep() in
#' a fashion of 'like', meaning substrings are recognized.
#'
#' @param d.frame data.frame
#' @param col.name Name of column
#' @param part.name Search string in column names
#' @param \dots Arguments passed to further functions
#' @return Subset of data.frame for which selection conditions are met.
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso \code{\link{grepColNames}} \code{\link{grepColNA}} \code{\link{grepObjNames}}
#' \code{\link{grepNotCompleteEntries}} \code{\link{grepMultipleEntries}} \code{\link{grepRowNames}}
#' \code{\link{grepColNegNum}} \code{\link{grepColFactors}}
#' @references none
#' @examples
#' if(require("MASS"))
#' {
#'     data(crabs, package="MASS")
#'     print(head(grepColEntries(crabs, "sex", "M")))
#'     print(head(grepColEntries(crabs, "index", "2")))
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
# Manual          ------------------------------------------------------------------------------- --
#' @title Get column names of data.frame matching a search string
#'
#' @description Specify a character string and find the column names in a data.frame containing this
#' character string.
#'
#' @details Unlike using '==' or '\%in\%', here search is performed using grep() in a fashion of
#' 'like', meaning substrings are recognized.
#'
#' @param d.frame data.frame
#' @param part.name Search string in column names
#' @param sort.col specify output column to sort by
#' @param \dots Arguments passed to further functions
#' @return grepColNames() returns the found column names and details
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso \code{\link{grepColEntries}} \code{\link{grepColNA}} \code{\link{grepObjNames}}
#' \code{\link{grepNotCompleteEntries}} \code{\link{grepMultipleEntries}} \code{\link{grepRowNames}}
#' \code{\link{grepColNegNum}} \code{\link{grepColFactors}}
#' @references none
#' @examples
#' if(require("MASS"))
#' {
#'     data(crabs, package="MASS")
#'     head(crabs)
#'     print(grepColNames(crabs, "W"))
#'     print(grepColNames(crabs, "e"))
#' }
#' @export
grepColNames <- function(d.frame, part.name, sort.col="nr", ...) {
  #
  # method                                        ............................................. ..
  # - use 'grep'
  #
  # input                                         ............................................. ..
  # - d.frame         data frame
  # - part.name       character string to look for in names()
  # - sort.col        column name to sort output upon
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
  if(sort.col%in%colnames(result)){
    result <- result[order(result[, sort.col]), ]
  }
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
# Manual          ------------------------------------------------------------------------------- --
#' @title Get attributes of data.frame which contain NAs
#'
#' @description Get attributes of data.frame which contain NAs.
#'
#' @details If the input is a vector, grepColNA() indicates if NAs are present. If the input is a
#' data.frame, grepColNA() returns the colums which contain NAs.
#'
#' @param x data.frame
#' @param \dots Arguments passed to further functions
#' @return Column names which values contain NAs or indication about NAs.
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso \code{\link{grepColEntries}} \code{\link{grepColNames}} \code{\link{grepObjNames}}
#' \code{\link{grepNotCompleteEntries}} \code{\link{grepMultipleEntries}} \code{\link{grepRowNames}}
#' \code{\link{grepColNegNum}} \code{\link{grepColFactors}}
#' @references none
#' @examples
#' if(require("MASS"))
#' {
#'     # load example data
#'     data(crabs, package="MASS")
#'     head(crabs)
#'     print(grepColNA(crabs))
#'     tt <- crabs
#'     tt[122,2] <- NA
#'     tt[22, 4] <- NA
#'     print(grepColNA(tt))
#'     print(grepColNA(c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6)))
#' }
#' @export
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
# Manual          ------------------------------------------------------------------------------- --
#' @title Find objects whose names contain search string
#'
#' @description The function grepObjNames() gets the objects whose names contain the specified
#' search string.
#'
#' @details Unlike subset which works with '==' or '\%in\%', here search is performed in a fashion of
#' 'like', meaning substrings are recognized.
#'
#' @param part.name search string in object names
#' @param \dots Arguments passed to further functions
#' @return column names which values contain NAs or indication about NAs
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso \code{\link{grepColEntries}} \code{\link{grepColNames}} \code{\link{grepColNA}}
#' \code{\link{grepNotCompleteEntries}} \code{\link{grepMultipleEntries}} \code{\link{grepRowNames}}
#' \code{\link{grepColNegNum}} \code{\link{grepColFactors}}
#' @references none
#' @examples
#'     a  <- 1
#'     aa <- 1
#'     b  <- 1
#'     ab <- 1
#'     A  <- 1
#'     Aa <- 1
#'     b  <- 1
#'     print(grepObjNames("a"))
#'     print(grepObjNames("A"))
#'     print(grepObjNames("b"))
#' @export
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
# Manual          ------------------------------------------------------------------------------- --
#' @title Get incomplete records in a data.frame
#'
#' @description grepNotCompleteEntries returns the records of a data.frame containing NAs.
#'
#' @details The input is a data.frame of which the records are returned that contain NAs in any
#' values.
#'
#' @param d.frame data.frame
#' @param \dots Arguments passed to further functions
#' @return Subset of data.frame where records contain NAs.
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso \code{\link{grepColEntries}} \code{\link{grepColNames}} \code{\link{grepColNA}}
#' \code{\link{grepObjNames}} \code{\link{grepMultipleEntries}} \code{\link{grepRowNames}}
#' \code{\link{grepColNegNum}} \code{\link{grepColFactors}}
#' @references none
#' @examples
#' if(require("MASS"))
#' {
#'     data(crabs, package="MASS")
#'     df <- crabs
#'     df[33, 4] <- NA
#'     df[55, 5] <- NA
#'     df[77, 6] <- NA
#'     df[99, 7] <- NA
#'     print(grepNotCompleteEntries(df))
#' }
#' @export
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
# Manual          ------------------------------------------------------------------------------- --
#' @title Records with duplicates in selected columns
#'
#' @description Subset a data.frame wich multiple entries in one or more columns.
#'
#' @details The multiple entries are found with duplicated() and the whole records of these entries
#' are returned.
#'
#' @param d.frame data.frame
#' @param var variable(s) to search for duplicates
#' @param \dots Arguments passed to further functions
#' @return Subset of data.frame for which selection conditions are met
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso \code{\link{grepColEntries}} \code{\link{grepColNames}} \code{\link{grepColNA}}
#' \code{\link{grepObjNames}} \code{\link{grepNotCompleteEntries}} \code{\link{grepRowNames}}
#' \code{\link{grepColNegNum}} \code{\link{grepColFactors}}
#' @references none
#' @examples
#' if(require("MASS"))
#' {
#'     data(crabs, package="MASS")
#'     #
#'     # - with one column
#'     print(head(grepMultipleEntries(crabs, "FL"), n=10))
#'     #
#'     # - with two columns
#'     print(head(grepMultipleEntries(crabs, c("sex", "FL"))))
#' }
#' @export
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
# Manual          ------------------------------------------------------------------------------- --
#' @title Get row names of data.frame matching a search string
#'
#' @description Specify a character string and find the row names in a data.frame containing this
#' character string.
#'
#' @details Unlike using '==' or '\%in\%', here search is performed using grep() in a fashion of
#' 'like', meaning substrings are recognized.
#'
#' @param d.frame data.frame
#' @param part.name Search string in row names
#' @param \dots Arguments passed to further functions
#' @return Number and names of found row names.
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso \code{\link{grepColEntries}} \code{\link{grepColNames}} \code{\link{grepColNA}}
#' \code{\link{grepObjNames}} \code{\link{grepNotCompleteEntries}} \code{\link{grepMultipleEntries}}
#' \code{\link{grepColNegNum}} \code{\link{grepColFactors}}
#' @references none
#' @examples
#' if(require("MASS"))
#' {
#'     data(crabs, package="MASS")
#'     head(crabs)
#'     rownames(crabs) <- paste0("row_", rownames(crabs))
#'     print(grepRowNames(crabs, "16"))
#' }
#' @export
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
# Manual          ------------------------------------------------------------------------------- --
#' @title Get column names of data.frame who contain negative values
#'
#' @description grepColNegNum() returns the column numbers and names of the numerical columns
#' (numeric and integer) which contain negative numbers
#'
#' @details Only numeric attributes (numeric and integer) are searched and reported.
#'
#' @param x data.frame
#' @return grepColNegNum() returns the column number and names of columns containing negabive
#' numeric values.
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso \code{\link{grepColEntries}} \code{\link{grepColNames}} \code{\link{grepColNA}}
#' \code{\link{grepObjNames}} \code{\link{grepNotCompleteEntries}} \code{\link{grepMultipleEntries}}
#' \code{\link{grepRowNames}} \code{\link{grepColFactors}}
#' @references none
#' @examples
#' if(require("MASS"))
#' {
#'     require("car")
#'     data(Mroz)
#'     head(Mroz)
#'     x <- Mroz
#'     print(grepColNegNum(Mroz))
#' }
#' @export
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
    class(x) <- "data.frame"

    # apply method to data.frame() .............................................................
    var.list <- data.frame(var_name = names(x),
                           var_type = unlist(sapply(x[, names(x)], class)))
    var.num  <- var.list$var_type%in%c("numeric", "integer")
    var.num  <- var.list$var_name[var.num]
    var.num  <- which(names(x) %in% var.num)
    for(v in var.num)
    {
      if(min(x[, v], na.rm=TRUE)<0)
      {
        list.neg.num <- c(list.neg.num, v)
      }
    }
    if (length(list.neg.num) == 0) {
      return("no columns with negative numbers present!")
    } else {
      list.neg.num <- data.frame(nr   = list.neg.num,
                                 name = names(x)[list.neg.num])
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
# Manual          ------------------------------------------------------------------------------- --
#' @title Get column names of data.frame with factors
#'
#' @description Of a data.frame, get the attributes with class 'factor' and 'ordered factor'.
#'
#' @details Factor variables are returned and indicated if also defined as 'ordered'.
#'
#' @param x data.frame
#' @return grepColFactors() returns variables are returned and indicated if also defined as
#' 'ordered'.
#' @note under continuous developement
#' @author Roland Rapold
#' @seealso \code{\link{grepColEntries}} \code{\link{grepColNames}} \code{\link{grepColNA}}
#' \code{\link{grepObjNames}} \code{\link{grepNotCompleteEntries}} \code{\link{grepMultipleEntries}}
#' \code{\link{grepRowNames}} \code{\link{grepColNegNum}}
#' @references none
#' @examples
#' if(require("car"))
#' {
#'   require("car")
#'   data(Mroz)
#'   Mroz$wc_ord <- as.ordered(Mroz$wc)
#'   print(grepColFactors(Mroz))
#' }
#' @export
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
