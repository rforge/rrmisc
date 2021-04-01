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
#' @title test granularity of attributes in data.frame
#'
#' @description Compare granularity of whole data.frame and of provided attributes
#'
#' @param d.data, data.frame
#' @param var, vector of attributes in d.data
#' @param verbose, add additional output
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
#' testGranularity(d.data = d.data, var = var, verbose = FALSE)
#'
#' var <- c("b")
#' testGranularity(d.data = d.data, var = var)
#' testGranularity(d.data = d.data, var = var, verbose = FALSE)
#'
#' var <- c("a", "b")
#' testGranularity(d.data = d.data, var = var)
#' testGranularity(d.data = d.data, var = var, verbose = FALSE)
#'
#' var <- c("a", "b", "c", "d")
#' testGranularity(d.data = d.data, var = var)
#' testGranularity(d.data = d.data, var = var, verbose = FALSE)
#' @export
testGranularity <- function(d.data, var, verbose = NULL, ...)
{
  #
  # ----------------------------------------------------------------------------------------------
  # method
  # - test granularity of attributes in data.frame
  #
  # input
  # - d.data    = data.frame
  # - var       = vector of attributes to test
  # - verbose   = verbose status TRUE/FALSE/NULL(default)
  #
  # output
  # - description of granularity of attributes in data.frame
  # ----------------------------------------------------------------------------------------------
  #
  # detailThreshold -- Schwelle in Anzahl Zeilen wenn bei verbose = NULL von verbose = TRUE auf
  # verbose = FALSE umgeschaltet wird
  detailThreshold <- 100000
  #
  # data.frame Syntax wird angewandt, so muss ein data.frame Obejkt erstellt werden
  if ("data.table" %in% class(d.data)) {
    class(d.data) <- "data.frame"
  }
  #
  # Faktoren in Zeichenattribute umwandeln
  for (attr in names(d.data))
  {
    if (is.factor(d.data[, attr])) {
      d.data[, attr] <- as.character(d.data[, attr])
    }
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
    print(paste(sprintf("%-40s", "eindeutig in den Attributen"), "--", paste(var, collapse = ", ")))
  } else {
    print(paste(sprintf("%-40s", "nicht eindeutig in den Attributen"), "--", paste(var, collapse = ", ")))
  }

  if ((is.null(verbose) && t1 < detailThreshold) | (!is.null(verbose) && verbose == TRUE)) {
    print(paste(sprintf("%-40s", "Dimension Datensatz"), "--", formatC(t1, big.mark = "'", width = 10, format = "d")))
    #
    # NAs weggelassen
    print(paste(sprintf("%-40s", "Kombinationen Attribute ohne NAs"), "--",
                formatC(as.numeric(countDistinct(d.data[, var])[1]),
                        big.mark = "'", width = 10, format = "d")))
    #
    # NAs als ein Wert mitgezaehlt
    print(paste(sprintf("%-40s", "Kombinationen Attribute mit NAs"), "--",
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
      print(paste(sprintf("%-40s", "Kombinationen Attribute NAs selektiv"), "--",
                  formatC(c1NotOnlyNA + c1OnlyNA, big.mark = "'", width = 10, format = "d")))
    }

    for (v in var)
    {
      # countDistinct(c(2, 3, 4, NA, 2, 3, 4, NA, NA)) # NA _nicht_ als ein Wert
      # count NAs
      # "3" "yes"
      cD <- countDistinct(d.data[, v])
      if (cD[2] == "yes") {
        cD[2] <- "ja"
      } else {
        cD[2] <- "nein"
      }
      print(paste(sprintf("%-40s", paste("Kombinationen Attribut", v)), "--",
                  formatC(as.numeric(cD[1]), big.mark = "'", width = 10, format = "d"),
                  "-- NAs vorhanden:", cD[2]))
    }
  }
  # print(res)
  # return(res)
}


#
# --------------- testGranularity --------------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
# # }}}
# --------------- tableCountPart                    Table with counts and parts                   ..{{{
# Manual          ------------------------------------------------------------------------------- --
#' @title Generate table with counts, parts and cumulative parts
#'
#' @description Generate table with counts, parts and cumulative parts. Allow to cut table after
#' predefined length
#'
#' @param x data.frame
#' @param varName name of attribute to list
#' @param anzName column name for counts
#' @param antName column name for parts
#' @param csumName column name for cumulative parts
#' @param ordCol number of column to sort
#' @param ordDir sort order ('asc' for ascending or 'desc' for descending)
#' @param rowLimit number of rows with detailed counts and parts
#' @param \dots arguments passed to further functions
#' @return data.frame with computed statistics
#' @note under continuous developement
#' @author Roland Rapold
#' @references none
#' @examples
#'    data(mtcars, package = "datasets")
#'    str(mtcars)
#'    table(mtcars$gear)
#'    tableCountPart(x = mtcars$gear
#'            , varName = "Gänge"
#'            , anzName = "Anzahl"
#'            , antName = "Anteil"
#'            , csumName = "Anteil kumuliert"
#'            , ordCol = 1
#'            , ordDir = "desc"
#'            , rowLimit = 20
#'            )
#'    tableCountPart(x = mtcars$carb
#'            , varName = "Vergaser"
#'            , anzName = "Anzahl"
#'            , antName = "Anteil"
#'            , csumName = "Anteil kumuliert"
#'            , ordCol = 1
#'            , ordDir = "asc"
#'            , rowLimit = 20
#'            )
#'    tableCountPart(x = mtcars$carb
#'            , varName = "Vergaser"
#'            , anzName = "Anzahl"
#'            , antName = "Anteil"
#'            , csumName = "Anteil kumuliert"
#'            , ordCol = 1
#'            , ordDir = "asc"
#'            , rowLimit = 4
#'            )
#'    tableCountPart(x = mtcars$carb
#'            , varName = "Vergaser"
#'            , anzName = "Anzahl"
#'            , antName = "Anteil"
#'            )
#' @export
tableCountPart <- function(x, varName = "var", anzName = "anz", antName = "ant", csumName = FALSE,
                           ordCol = 1, ordDir = "asc", rowLimit = FALSE, ...)
{
  # run <- 0
    run <- 1
  # if(run != 1)
  # {
  #     x        <- sample_data_med$kanton
  #     varName  <- "Kanton"
  #     anzName  <- "Anzahl Personen"
  #     antName  <- "Anteil Personen"
  #     csumName <- FALSE
  #     csumName <- "Anteil Personen kumuliert"
  #     ordCol   <- FALSE
  #     ordCol   <- 1
  #     ordCol   <- 2
  #     ordDir   <- "asc"
  #     rowLimit <- 20
  # }

    n_NA <- sum(is.na(x))
    x    <- x[!is.na(x)]

    tt <- table(x)
    pt <- prop.table(x = tt)
    if(!run == 1) tt
    if(!run == 1) pt

    tt <- data.frame(tt)
#   tt$ord <- 1:nrow(tt)
    pt <- data.frame(pt)
    if(!run == 1) tt
    if(!run == 1) pt

#   colnames(tt) <- c(varName, anzName, "ord")
    colnames(tt) <- c(varName, anzName)
    colnames(pt) <- c(varName, antName)
    tt           <- merge(tt, pt)
    if(!run == 1) tt

    # sortieren                                  ---------------------------------------------------
#   if(ordCol==FALSE) {
#       tt <- tt[order(tt$ord), ]
#   } else {
    if(ordDir == "asc") {
        tt <- tt[order(tt[, ordCol], decreasing = FALSE), ]
    } else {
        tt <- tt[order(tt[, ordCol], decreasing = TRUE), ]
    }
#   }
#   tt$ord <- NULL
    if(!run == 1) tt

    rownames(tt) <- NULL
    tt[, 1]      <- as.character(tt[, 1])
    tt[, 2]      <- as.integer(tt[, 2])

    # Rest berechnen für eingeschränkte Sicht    ---------------------------------------------------
    if(rowLimit!=FALSE & rowLimit<nrow(tt)) {
        tta <- tt[c(1:rowLimit), ]
        restAnz <- length(x)-sum(tta[c(1:rowLimit), 2])
        restAnt <- sprintf("%.1f%%", (restAnz * 100) / length(x))
        if(!run==1) sum(tta[ , 2])
        if(!run==1) length(x)
        if(!run==1) tta
        if(!run==1) restAnz
        if(!run==1) restAnt
    }

    # Zahlen formatieren                         ---------------------------------------------------
    if(csumName != FALSE) tt[, csumName] <- cumsum(tt[, antName])
                          tt[, antName]  <- sprintf("%.1f%%", tt[, antName] * 100)
    if(csumName != FALSE) tt[, csumName] <- sprintf("%.1f%%", tt[, csumName] * 100)
    if(run != 1) tt

    # Totalangaben                               ---------------------------------------------------
    if(rowLimit == FALSE) {
        if(csumName == FALSE)          tt[nrow(tt) + 1, ] <- c("Total:", length(x), "")
        if(csumName != FALSE)          tt[nrow(tt) + 1, ] <- c("Total:", length(x), "", "")
        if(csumName == FALSE & n_NA>0) tt[nrow(tt) + 1, ] <- c("Anzahl NA:", n_NA, "")
        if(csumName != FALSE & n_NA>0) tt[nrow(tt) + 1, ] <- c("Anzahl NA:", n_NA, "", "")
        return(tt)
    }
    if(rowLimit >= nrow(tt)) {
        if(csumName == FALSE)          tt[nrow(tt) + 1, ] <- c("Total:", length(x), "")
        if(csumName != FALSE)          tt[nrow(tt) + 1, ] <- c("Total:", length(x), "", "")
        if(csumName == FALSE & n_NA>0) tt[nrow(tt) + 1, ] <- c("Anzahl NA:", n_NA, "")
        if(csumName != FALSE & n_NA>0) tt[nrow(tt) + 1, ] <- c("Anzahl NA:", n_NA, "", "")
        return(tt)
    }
    if(!run == 1) tt

    # Zeile mit Restangaben bei eingeschr. Sicht ---------------------------------------------------
    tt <- tt[c(1:rowLimit), ]
    if(csumName == FALSE)            tt[nrow(tt) + 1, ] <- c("Rest:", restAnz, restAnt)
    if(csumName != FALSE)            tt[nrow(tt) + 1, ] <- c("Rest:", restAnz, restAnt, "")
    if(csumName == FALSE)            tt[nrow(tt) + 1, ] <- c("Total:", length(x), "")
    if(csumName != FALSE)            tt[nrow(tt) + 1, ] <- c("Total:", length(x), "", "")
    if(csumName == FALSE & n_NA > 0) tt[nrow(tt) + 1, ] <- c("Anzahl NA:", n_NA, "")
    if(csumName != FALSE & n_NA > 0) tt[nrow(tt) + 1, ] <- c("Anzahl NA:", n_NA, "", "")
    if(run != 1) tt
    return(tt)
}
#
# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
##}}}
# --------------- tableCountPart_2d()               Table with parts in various representations   ..{{{
# Manual          ------------------------------------------------------------------------------- --
#' @title Generate table with counts an parts in various different representations.
#'
#' @description Generate table with counts and parts in various different representations. The
#' differen representations are also du to marginal totals in the different directions.
#'
#' @param x data vector together with y as alternative to z
#' @param y data vector together with x as alternative to z
#' @param z data.frame as alternative to x and y
#' @param precDigit number of digits for result
#' @param x_prefix prefix for label in x-axis
#' @param y_prefix prefix for label in y-axis
#' @param debug not relevant
#' @param big.mark = thousand separator
#' @param \dots arguments passed to further functions
#' @return data.frame with computed statistics
#' @note under continuous developement
#' @author Roland Rapold
#' @references none
#' @examples
#'    str(mtcars)
#'    table(mtcars$gear)
#'    table(mtcars$carb)
#'    precDigit  <- 1
#'
#'    x <- mtcars$gear               # Auspraegungen in Resultat in Reihen
#'    y <- mtcars$carb               # Auspraegungen in Resultat in Spalten
#'    z <- as.data.frame.matrix(table(x, y))
#'
#'    tableCountPart_2d(x = x, y = y, precDigit = 3) [[4]]
#'    tableCountPart_2d(z = z, precDigit = 3)[[4]]
#'
#'    tableCountPart_2d(z = z, precDigit = 1)[[5]]
#'    tableCountPart_2d(z = z, precDigit = 1)[[6]]
#'    tableCountPart_2d(z = z, precDigit = 1)[[7]]
#'    tableCountPart_2d(z = z, precDigit = 0)$abs
#'    tableCountPart_2d(z = z, precDigit = 0)$abs_ant_col
#'    tableCountPart_2d(z = z, precDigit = 1)
#' @export
tableCountPart_2d <- function(x = 1, y = 1, z = 1, precDigit = 1, x_prefix = "", y_prefix = "",
                              debug = 0, big.mark = FALSE, ...)
{
    # precDigit -->> precDigit
    if(precDigit != 0) precDigit <- precDigit
    #
    #
    # Funktion Beginn                       ===================================================== ==
    # absolute Zahlen                       ----------------------------------------------------- --
    # absolute Zahlen und Chi^2-Test        -- abs -- p.value_raw -- p.value -------------------- -- {{{
    abs <- z
    if(!is.null(dim(abs)))
    {
        abs <- round(abs, 0)
        if(!is.data.frame(abs)) abs <- as.data.frame.matrix(abs)
    }
    if(is.null(dim(abs)))  abs <- as.data.frame.matrix(table(x, y))
    if(x_prefix != "") rownames(abs) <- paste(x_prefix, rownames(abs))
    if(y_prefix != "") colnames(abs) <- paste(y_prefix, colnames(abs))

    chitest     <- chisq.test(abs)
    p.value_raw <- chitest$p.value
    p.value     <- chitest$p.value
    if(!is.na(p.value)) {
        ifelse(chitest$p.value<0.001,
               p.value <- "< 0.001",
               p.value <- sprintf("%.3f", chitest$p.value))
    }
    # abs=pim_sd_1
    # abs <- cbind(data.frame(rownames(abs)), abs)
    # rownames(abs) <- c(1:nrow(abs))
    # colnames(abs)[1] <- " "
    #
    abs_form <- cbind(rownames(abs), abs)
    rownames(abs_form)    <- as.character(c(1:nrow(abs_form)))
    colnames(abs_form)[1] <- " "
    #
    if(debug==1) print("abs")
    if(debug==1) print(abs)
    if(debug==1) print("abs_form")
    if(debug==1) print(abs_form)
    if(debug==1) print("p.value_raw")
    if(debug==1) print(p.value_raw)
    if(debug==1) print("p.value")
    if(debug==1) if(!is.na(p.value)) print(p.value)
    # # }}}
    #
    # absolute Zahlen mit Randsummen        -- abs_margin_1..3 ---------------------------------- --{{{
    x_tot <- apply(abs, MARGIN = 1, FUN = sum)    # Zeilensummen
    y_tot <- apply(abs, MARGIN = 2, FUN = sum)    # Spaltensummen
    #
    abs_margin_1 <- cbind(abs, x_tot)
    colnames(abs_margin_1)[ncol(abs_margin_1)] <- "total"

    abs_margin_2 <- rbind(abs, y_tot)
    rownames(abs_margin_2)[nrow(abs_margin_2)] <- "total"

    abs_margin_3 <- rbind(abs_margin_1, c(y_tot, sum(abs)))
    rownames(abs_margin_3)[nrow(abs_margin_3)] <- "total"
    #
    abs_margin_form_1 <- cbind(rownames(abs_margin_1), abs_margin_1)
    rownames(abs_margin_form_1)    <- as.character(c(1:nrow(abs_margin_form_1)))
    colnames(abs_margin_form_1)[1] <- " "
    #
    abs_margin_form_2 <- cbind(rownames(abs_margin_2), abs_margin_2)
    rownames(abs_margin_form_2)    <- as.character(c(1:nrow(abs_margin_form_2)))
    colnames(abs_margin_form_2)[1] <- " "
    #
    abs_margin_form_3 <- cbind(rownames(abs_margin_3), abs_margin_3)
    rownames(abs_margin_form_3)    <- as.character(c(1:nrow(abs_margin_form_3)))
    colnames(abs_margin_form_3)[1] <- " "
    #
    if(debug==1) print("abs_margin_1")
    if(debug==1) print(abs_margin_1)
    if(debug==1) print("abs_margin_2")
    if(debug==1) print(abs_margin_2)
    if(debug==1) print("abs_margin_3")
    if(debug==1) print(abs_margin_3)
    if(debug==1) print("abs_margin_form_1")
    if(debug==1) print(abs_margin_form_1)
    if(debug==1) print("abs_margin_form_2")
    if(debug==1) print(abs_margin_form_2)
    if(debug==1) print("abs_margin_form_3")
    if(debug==1) print(abs_margin_form_3)
    # # }}}
    #
    # relative Zahlen                       ----------------------------------------------------- --
    # Anteile nach Total                    -- ant_tot -- ant_tot_margin_1..3 ------------------- --{{{
    ant_tot <- abs / sum(abs)
    #
    margin_row <- apply(ant_tot, MARGIN=1, FUN=sum)
    margin_col <- apply(ant_tot, MARGIN=2, FUN=sum)

    ant_tot_margin_1 <- cbind(ant_tot, margin_row)
    colnames(ant_tot_margin_1)[ncol(ant_tot_margin_1)] <- "total"

    ant_tot_margin_2 <- rbind(ant_tot, c(margin_col, sum(margin_row)))
    rownames(ant_tot_margin_2)[nrow(ant_tot_margin_2)] <- "total"

    ant_tot_margin_3 <- rbind(ant_tot_margin_1, c(margin_col, sum(margin_row)))
    rownames(ant_tot_margin_3)[nrow(ant_tot_margin_3)] <- "total"
    #
    for(i in 1:ncol(ant_tot))
    {
        ant_tot[, i] <- sprintf(paste("%.", precDigit, "f%%", sep=""), ant_tot[, i]*100)
    }
    for(i in 1:ncol(ant_tot_margin_1))
    {
        ant_tot_margin_1[, i] <- sprintf(paste("%.", precDigit, "f%%", sep=""), ant_tot_margin_1[, i]*100)
        ant_tot_margin_3[, i] <- sprintf(paste("%.", precDigit, "f%%", sep=""), ant_tot_margin_3[, i]*100)
    }
    for(i in 1:ncol(ant_tot_margin_2))
    {
        ant_tot_margin_2[, i] <- sprintf(paste("%.", precDigit, "f%%", sep=""), ant_tot_margin_2[, i]*100)
    }
    #
    ant_tot_form <- cbind(rownames(ant_tot), ant_tot)
    rownames(ant_tot_form)    <- as.character(c(1:nrow(ant_tot_form)))
    colnames(ant_tot_form)[1] <- " "
    if(debug==1) print("ant_tot")
    if(debug==1) print(ant_tot)
    if(debug==1) print("ant_tot_form")
    if(debug==1) print(ant_tot_form)
    #
    ant_tot_margin_form_1 <- cbind(rownames(ant_tot_margin_1), ant_tot_margin_1)
    rownames(ant_tot_margin_form_1)    <- as.character(c(1:nrow(ant_tot_margin_form_1)))
    colnames(ant_tot_margin_form_1)[1] <- " "
    ant_tot_margin_form_2 <- cbind(rownames(ant_tot_margin_2), ant_tot_margin_2)
    rownames(ant_tot_margin_form_2)    <- as.character(c(1:nrow(ant_tot_margin_form_2)))
    colnames(ant_tot_margin_form_2)[1] <- " "
    ant_tot_margin_form_3 <- cbind(rownames(ant_tot_margin_3), ant_tot_margin_3)
    rownames(ant_tot_margin_form_3)    <- as.character(c(1:nrow(ant_tot_margin_form_3)))
    colnames(ant_tot_margin_form_3)[1] <- " "
    if(debug==1) print("ant_tot_margin_1")
    if(debug==1) print(ant_tot_margin_1)
    if(debug==1) print("ant_tot_margin_2")
    if(debug==1) print(ant_tot_margin_2)
    if(debug==1) print("ant_tot_margin_3")
    if(debug==1) print(ant_tot_margin_3)
    if(debug==1) print("ant_tot_margin_form_1")
    if(debug==1) print(ant_tot_margin_form_1)
    if(debug==1) print("ant_tot_margin_form_2")
    if(debug==1) print(ant_tot_margin_form_2)
    if(debug==1) print("ant_tot_margin_form_3")
    if(debug==1) print(ant_tot_margin_form_3)
    # # }}}
    # Anteile nach Zeilen                   -- ant_row -- ant_row_margin ------------------------ --{{{
    ant_row <- abs
    for(i in 1:ncol(abs))
    {
        ant_row[, i] <- abs[, i] / x_tot
    }
    #
    margin_row <- apply(ant_row, MARGIN=1, FUN=sum)
    margin_col <- apply(ant_row, MARGIN=2, FUN=sum)
    ant_row_margin <- cbind(ant_row, margin_row)
    colnames(ant_row_margin)[ncol(ant_row_margin)] <- "total"
    #
    for(i in 1:ncol(ant_row))
    {
        ant_row[, i] <- sprintf(paste("%.", precDigit, "f%%", sep=""), ant_row[, i]*100)
    }
    for(i in 1:ncol(ant_row_margin))
    {
        ant_row_margin[, i] <- sprintf(paste("%.", precDigit, "f%%", sep=""), ant_row_margin[, i]*100)
    }
    #
    ant_row_form <- cbind(rownames(ant_row), ant_row)
    rownames(ant_row_form)    <- as.character(c(1:nrow(ant_row_form)))
    colnames(ant_row_form)[1] <- " "
    #
    if(debug==1) print("ant_row")
    if(debug==1) print(ant_row)
    if(debug==1) print("ant_row_form")
    if(debug==1) print(ant_row_form)
    #
    ant_row_margin_form <- cbind(rownames(ant_row_margin), ant_row_margin)
    rownames(ant_row_margin_form)    <- as.character(c(1:nrow(ant_row_margin_form)))
    colnames(ant_row_margin_form)[1] <- " "
    #
    if(debug==1) print("ant_row_margin")
    if(debug==1) print(ant_row_margin)
    if(debug==1) print("ant_row_margin_form")
    if(debug==1) print(ant_row_margin_form)
    #
    # # }}}
    # Anteile nach Spalten                  -- ant_col -- ant_col_margin ------------------------ --{{{
    ant_col <- abs
    for(i in 1:nrow(abs))
    {
        ant_col[i, ] <- abs[i, ] / y_tot
    }
    #
    margin_row <- apply(ant_col, MARGIN=1, FUN=sum)
    margin_col <- apply(ant_col, MARGIN=2, FUN=sum)
    ant_col_margin <- rbind(ant_col, margin_col)
    rownames(ant_col_margin)[nrow(ant_col_margin)] <- "total"
    #
    for(i in 1:ncol(ant_col))
    {
        ant_col[, i] <- sprintf(paste("%.", precDigit, "f%%", sep=""), ant_col[, i]*100)
    }
    for(i in 1:ncol(ant_col_margin))
    {
        ant_col_margin[, i] <- sprintf(paste("%.", precDigit, "f%%", sep=""), ant_col_margin[, i]*100)
    }
    #
    ant_col_form <- cbind(rownames(ant_col), ant_col)
    rownames(ant_col_form)    <- as.character(c(1:nrow(ant_col_form)))
    colnames(ant_col_form)[1] <- " "
    if(debug==1) print("ant_col")
    if(debug==1) print(ant_col)
    if(debug==1) print("ant_col_form")
    if(debug==1) print(ant_col_form)
    #
    ant_col_margin_form <- cbind(rownames(ant_col_margin), ant_col_margin)
    rownames(ant_col_margin_form)    <- as.character(c(1:nrow(ant_col_margin_form)))
    colnames(ant_col_margin_form)[1] <- " "
    if(debug==1) print("ant_col_margin")
    if(debug==1) print(ant_col_margin)
    if(debug==1) print("ant_col_margin_form")
    if(debug==1) print(ant_col_margin_form)
    # # }}}
    #
    # Absole und relative Zahlen mischen    ----------------------------------------------------- --
    # Anteile nach Total                    -- abs_ant_tot -------------------------------------- --{{{

    # Obsolete Fassung                      ..................................................... ..
    if (0==1) {
        abs_ant_tot      <- abs[1, ]
        abs_ant_tot[2, ] <- ant_tot[1, ]
        if(nrow(abs)>1)
        {
            for(i in 2:nrow(abs))
            {
                abs_ant_tot[2*i-1, ] <- abs[i, ]
                abs_ant_tot[2*i  , ] <- ant_tot[i, ]
            }
        }
    }

    # Leerzeilen hinzufÃ¼gen                 ..................................................... ..
    abs_temp <- abs
    abs_ant_tot <- rbind(abs_temp[1, ], c(rep(" ", ncol(abs_temp))))
    abs_ant_tot <- as.data.frame(abs_ant_tot)
    for(i in 2:nrow(abs_temp)) {
        abs_ant_tot <- rbind(abs_ant_tot, abs_temp[i, ])
        abs_ant_tot <- rbind(abs_ant_tot, c(rep(" ", ncol(abs_temp))))
    }
    # Leerspalten hinzufÃ¼gen                 .................................................... ..
    abs_temp <- abs_ant_tot
    abs_ant_tot <- as.data.frame(cbind(abs_temp[, 1], c(rep(" ", nrow(abs_temp)))))
    for(i in 2:ncol(abs_temp)) {
        abs_ant_tot <- cbind(abs_ant_tot, abs_temp[, i])
        abs_ant_tot <- cbind(abs_ant_tot, c(rep(" ", nrow(abs_temp))))
    }
    rm(abs_temp)
    # Anteilwerte hinzufÃ¼gen                 .................................................... ..
    for(i in 1:nrow(ant_tot)) {
        for(j in 1:ncol(ant_tot)) {
            abs_ant_tot[2*i, 2*j] <- ant_tot[i, j]
        }
    }
    #
    # Spaltennamen hinzufÃ¼gen                            ........................................ ..
    colnames(abs_ant_tot) <- c(1:ncol(abs_ant_tot))
    colnames(abs_ant_tot)[c(seq(1, (ncol(abs_ant_tot)-1), 2))] <- colnames(abs)
    colnames(abs_ant_tot)[seq(2, (ncol(abs_ant_tot)),   2)] <- " "
    #
    # Formatierte Variante erstellen                     ........................................ ..
    abs_ant_tot_form <- cbind(rownames(abs_ant_tot), abs_ant_tot)
    colnames(abs_ant_tot_form)[1] <- " "
    #
    abs_ant_tot_form[seq(1, (nrow(abs_ant_tot_form)-1), 2), 1] <- rownames(abs)
    abs_ant_tot_form[seq(2, (nrow(abs_ant_tot_form)),   2), 1] <- " "
    #
    if(debug==1) print("abs_ant_tot")
    if(debug==1) print(abs_ant_tot)
    if(debug==1) print("abs_ant_tot_form")
    if(debug==1) print(abs_ant_tot_form)
    #
    # # }}}

    # Anteile nach Total pl. Zeilen/Spalten -- abs_ant_tot_col/row/row_col----------------------- -- {{{
    abs_ant_tot
    ant_col
    ant_row

    abs_ant_tot_col <- abs_ant_tot
    for(i in 1:nrow(ant_col)) {
        for(j in 1:ncol(ant_col)) {
            abs_ant_tot_col[2*i, 2*j-1] <- ant_col[i, j]
        }
    }
    # abs_ant_tot_col

    abs_ant_tot_row <- abs_ant_tot
    for(i in 1:nrow(ant_row)) {
        for(j in 1:ncol(ant_row)) {
            abs_ant_tot_row[2*i-1, 2*j] <- ant_row[i, j]
        }
    }
    # abs_ant_tot_row

    abs_ant_tot_row_col <- abs_ant_tot_row
    for(i in 1:nrow(ant_col)) {
        for(j in 1:ncol(ant_col)) {
            abs_ant_tot_row_col[2*i, 2*j-1] <- ant_col[i, j]
        }
    }
    # abs_ant_tot_row_col

    abs_ant_tot_col_form <- cbind(rownames(abs_ant_tot_col), abs_ant_tot_col)
    rownames(abs_ant_tot_col_form)    <- as.character(c(1:nrow(abs_ant_tot_col_form)))
    colnames(abs_ant_tot_col_form)[1] <- " "
    if(debug==1) print("abs_ant_tot_col")
    if(debug==1) print(abs_ant_tot_col)
    if(debug==1) print("abs_ant_tot_col_form")
    if(debug==1) print(abs_ant_tot_col_form)

    abs_ant_tot_row_form <- cbind(rownames(abs_ant_tot_row), abs_ant_tot_row)
    rownames(abs_ant_tot_row_form)    <- as.character(c(1:nrow(abs_ant_tot_row_form)))
    colnames(abs_ant_tot_row_form)[1] <- " "
    if(debug==1) print("abs_ant_tot_row")
    if(debug==1) print(abs_ant_tot_row)
    if(debug==1) print("abs_ant_tot_row_form")
    if(debug==1) print(abs_ant_tot_row_form)

    abs_ant_tot_row_col_form <- cbind(rownames(abs_ant_tot_row_col), abs_ant_tot_row_col)
    rownames(abs_ant_tot_row_col_form)    <- as.character(c(1:nrow(abs_ant_tot_row_col_form)))
    colnames(abs_ant_tot_row_col_form)[1] <- " "
    if(debug==1) print("abs_ant_tot_row_col")
    if(debug==1) print(abs_ant_tot_row_col)
    if(debug==1) print("abs_ant_tot_row_col_form")
    if(debug==1) print(abs_ant_tot_row_col_form)
# }}}
#
    # Anteile nach Zeilen                   -- abs_ant_row -- abs_ant_row_margin ---------------- --{{{
    margin_row_abs <- apply(abs, MARGIN=1, FUN=sum)
    margin_row_ant <- rep("100%", length(margin_row_abs))

    abs_ant_row      <- as.data.frame(cbind(abs[, 1], ant_row[, 1]))
    rownames(abs_ant_row)    <- rownames(abs)
    colnames(abs_ant_row)[1] <- colnames(abs)[1]
    colnames(abs_ant_row)[2] <- " "
    if(ncol(abs)>1)
    {
        for(i in 2:ncol(abs)){
            abs_ant_row <- cbind(abs_ant_row, abs[, i])
            colnames(abs_ant_row)[ncol(abs_ant_row)] <- colnames(abs)[i]
            abs_ant_row <- cbind(abs_ant_row, ant_row[, i])
            colnames(abs_ant_row)[ncol(abs_ant_row)] <- " "
        }
    }
    #
    abs_ant_row_form <- cbind(rownames(abs_ant_row), abs_ant_row)
    rownames(abs_ant_row_form)    <- as.character(c(1:nrow(abs_ant_row_form)))
    colnames(abs_ant_row_form)[1] <- " "
    #
    abs_ant_row_margin <- cbind(abs_ant_row, margin_row_abs)
    colnames(abs_ant_row_margin)[ncol(abs_ant_row_margin)] <- "total"
    abs_ant_row_margin <- cbind(abs_ant_row_margin, margin_row_ant)
    colnames(abs_ant_row_margin)[ncol(abs_ant_row_margin)] <- "total"
    #
    abs_ant_row_margin_form <- cbind(rownames(abs_ant_row_margin), abs_ant_row_margin)
    rownames(abs_ant_row_margin_form)    <- as.character(c(1:nrow(abs_ant_row_margin_form)))
    colnames(abs_ant_row_margin_form)[1] <- " "
    #
    if(debug==1) print("abs_ant_row")
    if(debug==1) print(abs_ant_row)
    if(debug==1) print("abs_ant_row_form")
    if(debug==1) print(abs_ant_row_form)
    if(debug==1) print("abs_ant_row_margin")
    if(debug==1) print(abs_ant_row_margin)
    if(debug==1) print("abs_ant_row_margin_form")
    if(debug==1) print(abs_ant_row_margin_form)
    # # }}}
    # Anteile nach Spalten                  -- abs_ant_col -------------------------------------- --{{{
    abs_ant_col      <- abs[1, ]
    abs_ant_col[2, ] <- ant_col[1, ]
    if(nrow(abs)>1)
    {
        for(i in 2:nrow(abs)){
            abs_ant_col[2*i-1, ] <- abs[i, ]
            abs_ant_col[2*i  , ] <- ant_col[i, ]
        }
    }

    abs_ant_col_form <- cbind(rownames(abs_ant_col), abs_ant_col)
    rownames(abs_ant_col_form)    <- as.character(c(1:nrow(abs_ant_col_form)))
    abs_ant_col_form[seq(2, nrow(abs_ant_col_form), 2), 1] <- " "
    colnames(abs_ant_col_form)[1] <- " "
    if(debug==1) print("abs_ant_col")
    if(debug==1) print(abs_ant_col)
    if(debug==1) print("abs_ant_col_form")
    if(debug==1) print(abs_ant_col_form)
    # # }}}
    #
    # Nomenklatur Resultat                  ----------------------------------------------------- --
    # abs       Absolutwerte ( Summen, Anzahlen )
    # ant_tot   Anteile ueber ganze Matrix
    # ant_row   Anteile ueber Zeilen
    # ant_col   Anteile ueber Spalten
    # margin_1  Randsummen von Zeilen
    # margin_2  Randsummen von Spalten
    # margin_3  Randsummen von Zeilen und Spalten und Total
    #
    obj_list <-c("abs_margin_form_2", "abs_margin_form_3")
    obj_list <-c("abs_ant_row_margin_form")
    # obj_list <-c("abs_form", "p.value", "p.value_raw", ...)
    obj_list <-c("abs_form",
                 "abs_margin_form_1", "abs_margin_form_2", "abs_margin_form_3",
                 "ant_tot_form",
                 "ant_tot_margin_form_1", "ant_tot_margin_form_2", "ant_tot_margin_form_3",
                 "ant_row_form", "ant_col_form",
                 "abs_ant_tot_form", "abs_ant_tot_col_form", "abs_ant_tot_row_form",
                 "abs_ant_tot_row_col_form", "abs_ant_row_form", "abs_ant_row_margin_form", "abs_ant_col_form")
    #
    # big.mark <- "'"
    # browser()
    if(big.mark!=FALSE & is.character(big.mark)) {
        for(o in obj_list)
        {
            # t <- abs_ant_row_margin_form
            # i <- 16
            t <- get(o)
            for(i in 1:ncol(t))
            {
                if(class(t[, i])=="integer") t[, i] <- format(t[, i], big.mark=big.mark)
            }
            assign(o, t)
        }
    }
    #
    return(list(abs                         = abs_form
              , p.value                     = ifelse(is.na(p.value), NA, p.value)
              , p.value_raw                 = p.value_raw
              , abs_margin_1                = abs_margin_form_1
              , abs_margin_2                = abs_margin_form_2
              , abs_margin_3                = abs_margin_form_3
              , ant_tot                     = ant_tot_form
              , ant_tot_margin_1            = ant_tot_margin_form_1
              , ant_tot_margin_2            = ant_tot_margin_form_2
              , ant_tot_margin_3            = ant_tot_margin_form_3
              , ant_row                     = ant_row_form
              , ant_col                     = ant_col_form
              , abs_ant_tot                 = abs_ant_tot_form
              , abs_ant_tot_col             = abs_ant_tot_col_form
              , abs_ant_tot_row             = abs_ant_tot_row_form
              , abs_ant_tot_row_col         = abs_ant_tot_row_col_form
              , abs_ant_row                 = abs_ant_row_form
              , abs_ant_row_margin          = abs_ant_row_margin_form
              , abs_ant_col                 = abs_ant_col_form
                ))

    # Funktion Ende                         ===================================================== ==
}
#
# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
# }}}
# ==================================================================================================
