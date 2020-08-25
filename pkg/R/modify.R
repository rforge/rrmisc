# ==================================================================================================
# --------------- naToZero                       Ersetzen von NAs                                 ..{{{
# RR 20150109     ------------------------------------------------------------------------------- --
# Manual          ------------------------------------------------------------------------------- --
#' @title       Substitution of NAs in data.frame or vector
#' @description Substitution of NAs in a data.frame or a vector. NAs in numeric fields (integer,
#' numeric) are substituted by 0, NAs in caracter fields are substituted by '-'.
#' @details utility function for treating NAs
#' @param dataf data.frame or vector
#' @param substit_numeric shall numeric fields (integer, numerical) be substituted?
#' @param substit_character shall character fields be substituted?
#' @param \dots arguments passed to further functions
#' @return corresponding data.frame or vector with substituted NAs
#' @author Roland Rapold
#' @keywords NA
#' @examples
#'         (x <- c(pi, NA, 4.000000, 5.000000, 5, 6, 5, 6))
#'         naToZero(x)
#'         naToZero(x, substit_numeric=TRUE)
#'         naToZero(x, substit_numeric=FALSE)
#'
#'         (x <- c("pi", NA, "4.000000", "5.000000", "5", "6", "5", "6"))
#'         naToZero(x)
#'         naToZero(x, substit_character=TRUE)
#'         naToZero(x, substit_character=FALSE)
#' @export
naToZero <- function(dataf, substit_numeric=TRUE, substit_character=FALSE, ...)
{
    if(is.data.frame(dataf))
    {
        for(i in 1:ncol(dataf))
        {
            dataf_i    <- dataf[, i]
            dataf_i_cl <- class(dataf_i)
            switch(dataf_i_cl
                 , "integer"= {
                        if(substit_numeric==TRUE)
                        {
                            dataf_i[is.na(dataf_i)] <- 0
                            dataf[, i] <- dataf_i
                        }
                    }
                 , "numeric"= {
                        if(substit_numeric==TRUE)
                        {
                            dataf_i[is.na(dataf_i)] <- 0
                            dataf[, i] <- dataf_i
                        }
                    }
                 , "character"= {
                        if(substit_character==TRUE)
                        {
                            dataf_i[is.na(dataf_i)] <- "-"
                            dataf[, i] <- dataf_i
                        }
                    })
            rm(dataf_i)
        }
        return(dataf)
    } else {
        dataf_cl <- class(dataf)
        switch(dataf_cl
             , "integer"= {
                    if(substit_numeric==TRUE)
                    {
                        dataf[is.na(dataf)] <- 0
                    }
                    return(dataf)
                }
             , "numeric"= {
                    if(substit_numeric==TRUE)
                    {
                        dataf[is.na(dataf)] <- 0
                    }
                    return(dataf)
                }
             , "character"= {
                    if(substit_character==TRUE)
                    {
                        dataf[is.na(dataf)] <- "-"
                    }
                    return(dataf)
                })
        warning("naToZero: please input a data.frame or a numeric vector")
        return(dataf)
    }
}
# --------------- naToZero ------------------------------------------
# ENDE DER FUNKTION -------------------------------------------------
# # }}}
# ==================================================================================================
