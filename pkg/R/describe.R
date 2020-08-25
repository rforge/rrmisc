# ==================================================================================================
# --------------- listObjSizes                      list all objects with their size -- in order  --{{{
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
# --------------- listObjContrasts                  list contrasts of all factor attributes       --{{{
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
# --------------- getContStat                       get statistics of contingency table           --{{{
# RR 20200817     ------------------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#' @title Get statistics of contingency table
#'
#' @description Get statistics of contingency table
#'
#' @details utility function for analyzing contingency tables
#'
#' @param d.crosstab contincency table in class 'table
#' @param switchPosNeg switching positive and negative classes
#' @param \dots arguments passed to further functions
#' @return statistics of contingency table
#' @note to be refined
#' @author Roland Rapold
#' @seealso other utility-functions in this R-package
#' @references none
#' @examples
#' d.crosstab <- matrix(c(65.64, 1.95, 27.81, 4.6), ncol = 2, byrow = TRUE)
#' colnames(d.crosstab) <- c("true 0", "true 1")
#' rownames(d.crosstab) <- c("fit 0", "fit 1")
#' d.crosstab <- as.table(d.crosstab)
#' getContStat(d.crosstab)
#' @export
#
getContStat <- function(d.crosstab, switchPosNeg = FALSE, ...) {
  # Fawcett_2003, Sing 2005 (and ROCR documentation)
  # siehe auch Wikipedia
  #
  # true classes                 |             p          |            n           |
  # ^^^^^^^^^^^^                -|------------------------|------------------------|-
  #                            Y |  True Positives (TP)   |  False Positives (FP)  |
  # hypotzesized classes        -|------------------------|------------------------|-
  # ^^^^^^^^^^^^^^^^^^^^       N |  False Negatives (FN)  |  True Negatives (TN)   |
  #                             -|------------------------|------------------------|-
  # column totals                              P                       N
  # ^^^^^^^^^^^^^                              ^                       ^
  #
  # Umkehren von Positiven und Negativen Kennungen in Spalten und Reihen ...........................
  if (switchPosNeg) {
    temp.d.crosstab <- d.crosstab
    d.crosstab[2, 2] <- temp.d.crosstab[1, 1]
    d.crosstab[1, 1] <- temp.d.crosstab[2, 2]
    d.crosstab[1, 2] <- temp.d.crosstab[2, 1]
    d.crosstab[2, 1] <- temp.d.crosstab[1, 2]
    colnames(d.crosstab) <- rev(colnames(temp.d.crosstab))
    rownames(d.crosstab) <- rev(rownames(temp.d.crosstab))
    rm(temp.d.crosstab)
  }
  #
  # Berechnung der Kennzahlen ......................................................................
  TP <- d.crosstab[1, 1] # True Positives
  TN <- d.crosstab[2, 2] # True Negatives
  FP <- d.crosstab[1, 2] # False Positives
  FN <- d.crosstab[2, 1] # False Negatives

  P <- TP + FN # sum of true positives
  N <- TN + FP # sum of true negatives

  TP_rate <- TP / P # True positive rate -- Hit rate
  TN_rate <- TN / N # True negative rate
  FP_rate <- FP / N # False positive rate -- Type I error
  FN_rate <- FN / P # False negative rate -- Type II error

  sensitivity <- TP_rate
  specificity <- TN_rate

  accuracy   <- ( TP + TN ) / ( P + N )   # Korrektklassifikationsrate
  error_rate <- ( FP + FN ) / ( P + N )   # Falschklassifikationsrate
  precision  <- TP / ( TP + FP )          # Genauigkeit
  recall     <- TP_rate                   # Trefferquote
  ppv        <- precision                 # positive predictive value
  npv        <- TN / ( TN + FN )          # negative predictive value

  Phi <- ( TP * TN - FP * FN ) / sqrt( ( TP + FN ) * ( TN + FP ) * ( TP + FP ) * ( TN + FN ) )
  # Yields a number between -1 and 1, with 1 indicating a perfect prediction, 0 indicating a random
  # prediction. Values below 0 indicate a worse than random prediction.

  f_score <- 2 * (precision * recall) / (precision + recall)
  #
  # Test von Unabhängigkeit ........................................................................
  d.test <- chisq.test(d.crosstab)
  # d.test$p.value
  #
  # Ausgabe der Kennzahlen .........................................................................
  print(d.crosstab)
  print(">>> True positives in position [1, 1] <<<")
  print(sprintf("%-39s %.3f%%", "true positive rate (sensitivity)", TP_rate*100))
  print(sprintf("%-39s %.3f%%", "true negative rate (specificity)", TN_rate*100))
  print(sprintf("%-39s %.3f%%", "false positive rate (type I error)", FP_rate*100))
  print(sprintf("%-39s %.3f%%", "false negative rate (type II error)", FN_rate*100))
  print(sprintf("%-39s %.3f%%", "positive predictive value (ppv)", ppv*100))
  print(sprintf("%-39s %.3f%%", "negative predictive value (npv)", npv*100))
  print(sprintf("%-39s %.3f%%", "error rate", error_rate*100))
  print(sprintf("%-39s %.5f", "correlation (phi)", Phi))
  print(sprintf("%-39s %.3f%%", "F-value", f_score*100))
  print(sprintf("%-39s %.5f", "p-value (Chi-squared test)", d.test$p.value))

  return(NULL)
}
# END OF FUNCTION  ------------------------------------------------------------------------------ --
# --------------- getContStat ------------------------------------------------------------------- --
#}}}
# --------------- testGranularity                   Test granularity of attirbutes in data.frame  ..{{{
# RR 20200630     ------------------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#
#' @title test granularity of attributes in data.frame
#'
#' @description Compare granularity of whole data.frame and of provided attributes
#'
#' @param d.data, data.frame
#' @param var, vector of attributes in d.data
#' @param \dots arguments passed to further functions
#' @return output describing the granularity
#' @note under continuous developement
#' @author Roland Rapold
#' @references none
#' @examples
#' d.data <- data.frame(a = 1:20,
#'                      b = rep(1:10, 2),
#'                      c = letters[1:20],
#'                      d = rep(letters[1:10], 2))
#' var <- c("a")
#' testGranularity(d.data = d.data, var = var)
#'
#' var <- c("b")
#' testGranularity(d.data = d.data, var = var)
#'
#' var <- c("a", "b")
#' testGranularity(d.data = d.data, var = var)
#'
#' var <- c("a", "b", "c", "d")
#' testGranularity(d.data = d.data, var = var)
#' @export
testGranularity <- function(d.data, var, ...)
{
  #
  # ----------------------------------------------------------------------------------------------
  # method
  # - test granularity of attributes in data.frame
  #
  # input
  # - d.data    = data.frame
  # - var       = vector of attributes to test
  #
  # output
  # - description of granularity of attributes in data.frame
  # ----------------------------------------------------------------------------------------------
  #
  # data.frame Syntax wird angewandt, so muss ein data.frame Obejkt erstellt werden
  if ("data.table" %in% class(d.data)) {
    class(d.data) <- "data.frame"
  }
  #
  # Eindeutigkeit ueberpruefen
  # t1 <- length(unique(d.data[, var[1]])) # erstes Attribut
  t1 <- nrow(d.data) # Dimension Datensatz
  t2 <- unique(d.data[, var]) # Kombinationen alle Attribute
  # unique(c(2, 3, 4, NA, 2, 3, 4, NA, NA)) # NA als ein Wert
  # [1] 2 3 4 NA
  if (is.null(dim(t2))) {
    t2 <- length(t2)
  } else {
    t2 <- nrow(t2)
  }
  if (t1 == t2) {
    print(paste("eindeutig in den Attributen --", paste(var, collapse = ", ")))
  } else {
    print(paste("nicht eindeutig in den Attributen --", paste(var, collapse = ", ")))
    print(paste("Dimension Datensatz --",
                formatC(t1, big.mark = "'", width = 10, format = "d")))
    #
    # NAs weggelassen
    print(paste("Kombinationen Attribute ohne NAs --",
                formatC(as.numeric(countDistinct(d.data[, var])[1]),
                        big.mark = "'", width = 10, format = "d")))
    #
    # NAs als ein Wert mitgezaehlt
    print(paste("Kombinationen Attribute mit NAs --",
                formatC(t2, big.mark = "'", width = 10, format = "d")))
    #
    # NAs weggelassen wenn ein anderer Wert vorhanden, NAs als Wert mitgezaehlt wenn kein anderer
    # Wert vorhanden
    if (length(var) == 2) {
      v1All <- unique(d.data[, var])
      v1 <- unique(d.data[, var[1]])
      # zweite Spalte hat nicht nur NAs
      # - NAs in zweiter Spalte weglassen und Kombinationen zaehlen
      v1NotOnlyNA <- unique(v1All[!is.na(v1All[, 2]), ][, 1])
      c1NotOnlyNA <- as.numeric(countDistinct(d.data[d.data[, var[1]] %in% v1NotOnlyNA, var])[1])
      # zweite Spalte hat nur NAs
      # - das sind schon alle Kombinationen
      v1OnlyNA <- v1[!v1 %in% v1NotOnlyNA]
      c1OnlyNA <- length(v1OnlyNA)
      print(paste("Kombinationen Attribute NAs selektiv --",
                  formatC(c1NotOnlyNA + c1OnlyNA, big.mark = "'", width = 10, format = "d")))
    }

    for (v in var)
    {
      # countDistinct(c(2, 3, 4, NA, 2, 3, 4, NA, NA)) # NA _nicht_ als ein Wert
      # count NAs
      # "3" "yes"
      cD <- countDistinct(d.data[, v])
      print(paste(sprintf("%-40s", paste("Kombinationen Attribut", v)), "--",
                  formatC(as.numeric(cD[1]), big.mark = "'", width = 10, format = "d"),
                  "-- NAs present --", cD[2]))
    }
  }
  # print(res)
  # return(res)
}


#
# --------------- testGranularity --------------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
# # }}}
# ==================================================================================================
