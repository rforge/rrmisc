#
# ==================================================================================================
#
# --------------- createDefMeasures              Vorlage fuer def.measures erstellen              .. # {{{
# RR 20151110               --------------------------------------------------------------------- --
createDefMeasures <- function (d.data, version=2, var.list)
{
    #
    # d.data       = Input-Daten fuer 'descrMeasures' resp. 'descrTable', z.B. dort verwendete sub.d1
    # var.list     = Liste mit Variablennamen und Variablenbezeichnungen
    #                var.list$var_name      ->  def.measures$measure_name
    #                var.list$var_label     ->  def.measures$measure_label
    # def.measures = Resultat: Input-Parameter Tabelle fuer 'descrMeasures' resp. 'descrTable'
    #
    # pasteClass local function ....................................................................
    pasteClass <- function(x)
    {
        class.x <- class(x)
        if(length(class.x)>1)
        {
            class.x <- paste(class.x, collapse=" ")
        }
        return(class.x)
    }
    #
    # check input data                                  ............................................
    if (missing(d.data) || !is.data.frame(d.data))
    {
        return("please provide data.frame as 'd.data'")
    }
    #
    # create generic definition-table if none is provided ..........................................
  # if (missing(def.measures) || is.null(def.measures))
  # {
        def.measures <- data.frame(measure_label="a", measure_name="a"
                                 , measure_1="a"
                                 , measure_2="a"
                                 , measure_ref_level=NA
                                 , measure_prec_1=1
                                 , measure_prec_2=1)
        i.row        <- 1

        for(i in 1:ncol(d.data))
        {
            if(pasteClass(d.data[, i])=="numeric")
            {
                def.measures[i.row, "measure_label"] <- colnames(d.data)[i]
                def.measures[i.row, "measure_name"]  <- colnames(d.data)[i]
                def.measures[i.row, "measure_1"] <- "mean"
                def.measures[i.row, "measure_2"] <- "median"
                def.measures[i.row, "measure_ref_level"] <- NA
                def.measures[i.row, "measure_prec_1"] <- 1
                def.measures[i.row, "measure_prec_2"] <- 1
                i.row <- i.row + 1
                if(version>1)
                {
                    def.measures[i.row, "measure_label"] <- colnames(d.data)[i]
                    def.measures[i.row, "measure_name"]  <- colnames(d.data)[i]
                    def.measures[i.row, "measure_1"] <- "sd"
                    def.measures[i.row, "measure_2"] <- "IQR"
                    def.measures[i.row, "measure_ref_level"] <- NA
                    def.measures[i.row, "measure_prec_1"] <- 1
                    def.measures[i.row, "measure_prec_2"] <- 1
                    i.row <- i.row + 1
                }
            }
            if(pasteClass(d.data[, i])=="integer")
            {
                def.measures[i.row, "measure_label"] <- colnames(d.data)[i]
                def.measures[i.row, "measure_name"]  <- colnames(d.data)[i]
                def.measures[i.row, "measure_1"] <- "mean"
                def.measures[i.row, "measure_2"] <- "median"
                def.measures[i.row, "measure_ref_level"] <- NA
                def.measures[i.row, "measure_prec_1"] <- 1
                def.measures[i.row, "measure_prec_2"] <- 1
                i.row <- i.row + 1
                if(version>1)
                {
                    def.measures[i.row, "measure_label"] <- colnames(d.data)[i]
                    def.measures[i.row, "measure_name"]  <- colnames(d.data)[i]
                    def.measures[i.row, "measure_1"] <- "sd"
                    def.measures[i.row, "measure_2"] <- "IQR"
                    def.measures[i.row, "measure_ref_level"] <- NA
                    def.measures[i.row, "measure_prec_1"] <- 1
                    def.measures[i.row, "measure_prec_2"] <- 1
                    i.row <- i.row + 1
                }
            }
            if(pasteClass(d.data[, i])=="factor")
            {
                def.measures[i.row, "measure_label"] <- colnames(d.data)[i]
                def.measures[i.row, "measure_name"]  <- colnames(d.data)[i]
                def.measures[i.row, "measure_1"] <- "count"
                def.measures[i.row, "measure_2"] <- "portion"
                def.measures[i.row, "measure_ref_level"] <- NA
                def.measures[i.row, "measure_prec_1"] <- 0
                def.measures[i.row, "measure_prec_2"] <- 1
                i.row <- i.row + 1
            }
            if(pasteClass(d.data[, i])=="ordered factor")
            {
                def.measures[i.row, "measure_label"] <- colnames(d.data)[i]
                def.measures[i.row, "measure_name"]  <- colnames(d.data)[i]
                def.measures[i.row, "measure_1"] <- "count"
                def.measures[i.row, "measure_2"] <- "portion"
                def.measures[i.row, "measure_prec_1"] <- 0
                def.measures[i.row, "measure_prec_2"] <- 1
                i.row <- i.row + 1
            }
            if(pasteClass(d.data[, i])=="ordered")
            {
                def.measures[i.row, "measure_label"] <- colnames(d.data)[i]
                def.measures[i.row, "measure_name"]  <- colnames(d.data)[i]
                def.measures[i.row, "measure_1"] <- "count"
                def.measures[i.row, "measure_2"] <- "portion"
                def.measures[i.row, "measure_ref_level"] <- NA
                def.measures[i.row, "measure_prec_1"] <- 0
                def.measures[i.row, "measure_prec_2"] <- 1
                i.row <- i.row + 1
            }
        }
  # }
  # else {
  #     return(def.measures)
  # }
    #
    # substitute variable labels from reference list    ............................................
    if (!missing(var.list) && !is.null(var.list))
    {
        # order of rows
        def.measures$nr <- 1:nrow(def.measures)

        # merge reference list for measure_label's
        def.measures <- merge(def.measures[, -which(colnames(def.measures) == "measure_label")]
                              , var.list[, c("var_label", "var_name")]
                              , by.x = "measure_name"
                              , by.y = "var_name"
                              , all.x = TRUE)

        # get default labels when missing from reference list
        def.measures[is.na(def.measures$var_label), "var_label"] <-
            def.measures[is.na(def.measures$var_label), "measure_name"]

        # apply original order of rows
        def.measures <- def.measures[order(def.measures$nr), ]
        def.measures$nr <- NULL

        # apply and name original column order
        def.measures <- def.measures[, c("var_label", "measure_name", "measure_1", "measure_2"
                                         , "measure_ref_level", "measure_prec_1", "measure_prec_2")]
        colnames(def.measures)[1] <- "measure_label"
    }
    return(def.measures)
}
# --------------- createDefMeasures -------------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
## }}}
# ==================================================================================================
#
# --------------- descrMeasures                  Kennzahlen deskriptiv -- allgemeine Funktion     .. # {{{
# RR 20150325     ------------------------------------------------------------------------------- --
#
# Kennzahlen fuer Subgruppen
#
# Moegliche Kennzahlenaufrufe:
#   - count             Faktor oder Wert mit 1 Auspraegung (<- noch zu kontrollieren!!)
#   - count_distinct    Faktor
#   - portion           Faktor -- vertikal 100%, sonst horizontal 100%
#   - mean              numerisch
#   - wtd_mean          numerisch
#   - sd                numerisch
#   - median            numerisch
#   - wtd_median        numerisch
#   - IQR               numerisch
# Als zweite Kennzahl zusaetzlich zu einer bisherigen Kennzahl
#       - portion       Faktor -- vertikal 100%, sonst horizontal 100%
#       - mean          numerisch
#       - wtd_mean      numerisch
#       - sd            numerisch
#       - median        numerisch
#       - wtd_median    numerisch
#       - IQR           numerisch
#
# Spezialfall: Variable 'i' --> wird als Indikator 'Recordeinheit' aufgefasst und als
#                               'Kollektivgroesse' ausgegeben!
#
descrMeasures  <- function(descr.table=NULL             # Resultattabelle vorformatiert
                           , var.measure.label          # Variablenbezeichnung
                           , var.measure.name           # Variablenname / Attribut
                           , measures                   # Funktion fuer Kennzahlen
                           , measure.ref.level          # Referenzlevel fuer Faktoren
                           , prec.digit                 # Praezision, d.h. Anzahl Nachkommastellen
                           , sub.d1                     # Daten Spalte 1
                           , sub.d2                     # Daten Spalte 2
                           , sub.d3                     # Daten Spalte 3
                           , sub.d4                     # Daten Spalte 4
                           , sub.d5                     # Daten Spalte 5
                           , weights.1                  # Gewichte (Daten Spalte 1)
                           , weights.2                  # Gewichte (Daten Spalte 2)
                           , weights.3                  # Gewichte (Daten Spalte 3)
                           , weights.4                  # Gewichte (Daten Spalte 4)
                           , weights.5                  # Gewichte (Daten Spalte 5)
                           , label.compact              # Variablenliste und Messgroessen zusammen
                           , label.width                # Breite der ersten Spalte (nur bei label.compact==TRUE)
                           , test.gr                    # Welche Datenspalten sollten verglichen (getestet) werden?
                           , lang                       # Sprache, 'de' oder 'en'
                           , verbose)                   # 0 bis 2: Ausgabe Zwischenresultate
{
    # ------------------------------------------------------------------------------------------- --
    if(verbose>1)
    {
        print(paste("var.measure.label=", var.measure.label))
        print(paste("var.measure.name=", var.measure.name))
        print(paste("measures=", measures))
        print(paste("measure.ref.level=", measure.ref.level))
        print(paste("prec.digit=", prec.digit))
        print(paste("test.gr=", test.gr))
    }
    # ------------------------------------------------------------------------------------------- --
    # withWarnings() local function ................................................................ # {{{
    withWarnings <- function(expr) {
        myWarnings <- NULL
        wHandler <- function(w) {
            myWarnings <<- c(myWarnings, list(w))
            invokeRestart("muffleWarning")
        }
        val <- withCallingHandlers(expr, warning = wHandler)
        list(value = val, warnings = myWarnings)
    }
    ## }}}
    # ------------------------------------------------------------------------------------------- --
    # 0. ueberpruefung Eingabe Kennzahlen                                                         .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    #    Erste Kennzahl
    if(!measures[1]%in%c("count", "count_distinct", "portion", "mean", "wtd_mean", "sd", "median",
                         "wtd_median", "IQR", "min", "max"))
    {
        return(print(paste("selected first measure is not valid:", measures[1])))
    }
    #
    #    Zweite Kennzahl
    if(length(measures)>1 & !is.na(measures[2]))
    {
        if(!measures[2]%in%c("portion", "mean", "wtd_mean", "sd", "median", "wtd_median", "IQR", "min", "max"))
        {
            return(print(paste("selected second measure ist not valid:", measures[2])))
        }
        if(measures[2]=="portion" & !measures[1]=="count")
            return(print(paste("if the second measure is \'portion\' the first has to be \'count\'!")))
    }
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # ------------------------------------------------------------------------------------------- --
    # 0. Vorbereitungen                                                                           .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    # Ausgabewert wenn kein Wert fuer P-Wert .......................................................
    p.na <- "- "
    #
    # Indikatorvariablen fuer Tests ................................................................
    i.chisq         <- 0
    i.wilcoxon      <- 0
    i.kruskal       <- 0
    i.test.warning  <- 0
    #
    # Kennzahlenvektor zusammenfassen ..............................................................
    ifelse(is.na(measures[2]), measures <- c(measures[1]),
                               measures <- c(measures[1], measures[2]))
    #
    # Gewichtete vs. ungewichtete Analyse ..........................................................
    i.weights   <- 0                # 0: ungewichtete Analyse, 1: gewichtete Analyse
    #                               # wird bei der Wahl von wtd_mean oder wtd_median auf 1 gesetzt
    #
    # Anzahl Datenspalten ermitteln ................................................................
    dim.data <- 1
    if(!is.null(sub.d2)) dim.data <- 2
    if(!is.null(sub.d3)) dim.data <- 3
    if(!is.null(sub.d4)) dim.data <- 4
    if(!is.null(sub.d5)) dim.data <- 5
    if(verbose==2) print(paste("dim.data: -- :", dim.data))
    #
    # Datenvektoren erstellen ......................................................................
                   sub.data.1 <- sub.d1[, var.measure.name]
    if(dim.data>1) sub.data.2 <- sub.d2[, var.measure.name]
    if(dim.data>2) sub.data.3 <- sub.d3[, var.measure.name]
    if(dim.data>3) sub.data.4 <- sub.d4[, var.measure.name]
    if(dim.data>4) sub.data.5 <- sub.d5[, var.measure.name]
    rm(sub.d1, sub.d2, sub.d3, sub.d4, sub.d5)
    #
    # Ueberpruefung auf NAs ........................................................................
                   res.na <- c(        sum(is.na(sub.data.1)))
    if(dim.data>1) res.na <- c(res.na, sum(is.na(sub.data.2)))
    if(dim.data>2) res.na <- c(res.na, sum(is.na(sub.data.3)))
    if(dim.data>3) res.na <- c(res.na, sum(is.na(sub.data.4)))
    if(dim.data>4) res.na <- c(res.na, sum(is.na(sub.data.5)))
    #
    # Variablentyp - Faktor vs. numerisch - bestimmen ..............................................
    var.is.factor <- is.factor(sub.data.1)      # && !"count"%in%measures
    if(verbose==2) print(paste("var.is.factor: -- :", var.is.factor))
    #
    # Vorbereitungen fuer Faktoren .................................................................
    if(var.is.factor & !is.na(measure.ref.level))
    {
        if(!(measure.ref.level%in%levels(sub.data.1))) {
            return(print(paste("selected reference group 'measure.ref.level' is not valid !",
                                var.measure.label)))
        }
        if(measure.ref.level!=levels(sub.data.1)[1])
        {
                           sub.data.1 <- relevel(sub.data.1, measure.ref.level)
            if(dim.data>1) sub.data.2 <- relevel(sub.data.2, measure.ref.level)
            if(dim.data>2) sub.data.3 <- relevel(sub.data.3, measure.ref.level)
            if(dim.data>3) sub.data.4 <- relevel(sub.data.4, measure.ref.level)
            if(dim.data>4) sub.data.5 <- relevel(sub.data.5, measure.ref.level)
        }
    }
    #
    # Kontrolle -- bei Faktoren darf kein 'level' einen leeren String aufweisen ................. ..
    if(var.is.factor)
    {
        if(requireNamespace("DescTools", quietly=TRUE))
        {
                           levels(sub.data.1) <- DescTools::StrTrim(levels(sub.data.1))
            if(dim.data>1) levels(sub.data.2) <- DescTools::StrTrim(levels(sub.data.2))
            if(dim.data>2) levels(sub.data.3) <- DescTools::StrTrim(levels(sub.data.3))
            if(dim.data>3) levels(sub.data.4) <- DescTools::StrTrim(levels(sub.data.4))
            if(dim.data>4) levels(sub.data.5) <- DescTools::StrTrim(levels(sub.data.5))
        }
        m <- which(levels(sub.data.1)=="")
        if(length(m)>0)
        {
            if(lang=="de")
            {
                               levels(sub.data.1)[m] <- "_leer_"
                if(dim.data>1) levels(sub.data.2)[m] <- "_leer_"
                if(dim.data>2) levels(sub.data.3)[m] <- "_leer_"
                if(dim.data>3) levels(sub.data.4)[m] <- "_leer_"
                if(dim.data>4) levels(sub.data.5)[m] <- "_leer_"
            } else {
                               levels(sub.data.1)[m] <- "_empty_"
                if(dim.data>1) levels(sub.data.2)[m] <- "_empty_"
                if(dim.data>2) levels(sub.data.3)[m] <- "_empty_"
                if(dim.data>3) levels(sub.data.4)[m] <- "_empty_"
                if(dim.data>4) levels(sub.data.5)[m] <- "_empty_"
            }
        }
        rm(m)
    }
    #
    # Kontrolle -- bei Faktoren duerfen Levels keine Zahlen sein ................................ ..
    if(var.is.factor &&
       sum(levels(sub.data.1)%in%c(0:10000))==length(levels(sub.data.1)) &&
       var.measure.name!="i")
    {
                       levels(sub.data.1) <- paste(levels(sub.data.1), "_", sep="")
        if(dim.data>1) levels(sub.data.2) <- paste(levels(sub.data.2), "_", sep="")
        if(dim.data>2) levels(sub.data.3) <- paste(levels(sub.data.3), "_", sep="")
        if(dim.data>3) levels(sub.data.4) <- paste(levels(sub.data.4), "_", sep="")
        if(dim.data>4) levels(sub.data.5) <- paste(levels(sub.data.5), "_", sep="")
    }
    #
    # Kontrolle -- Fuer Faktorvariablen sind nur Anteile (portion) und Anzahlen (count) vorgesehen..
    if(var.is.factor)
    {
        not_m1 <- !measures[1]%in%c("portion", "count")
        not_m2 <- ifelse(length(measures)>1, !measures[2]%in%c("portion", "count"), FALSE)
        if(not_m1 || not_m2)
        {
            return(paste("for factors only 'portion' or 'count' as measures are permitted!\n",
                         "class of the provided variable:", class(sub.data.1), "\n",
                         "selected measures:", measures))
        }
    }
    if(verbose==2) print("Schritt 01")
    if(verbose==2) print(paste("variable : ---- :", var.measure.name))
    if(verbose==2) print(paste("measures : ---- :", measures))
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # ------------------------------------------------------------------------------------------- --
    # 1. Kennzahl berechnen
    # ------------------------------------------------------------------------------------------- --
    # Anzahl                                            'count'                                   .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    # NAs nicht beruecksichtigt !
    #
    if(measures[1]=="count")
    {
      # str(sub.data.1)
      # sub.data.1 <- rep(1, 100)
      # sub.data.2 <- rep(2, 100)
      # sub.data.3 <- rep(3, 100)

        if(dim.data==1)
        {
            measure.1 <- data.frame(table(sub.data.1))
            measure.1 <- measure.1[, c(1, 2)]
        }

        if(dim.data==2)
        {
            measure.1 <- data.frame(table(sub.data.1),
                                    table(sub.data.2))
            measure.1 <- measure.1[, c(1, 2, 4)]
        }
        if(dim.data==3)
        {
            measure.1 <- data.frame(table(sub.data.1),
                                    table(sub.data.2),
                                    table(sub.data.3))
            measure.1 <- measure.1[, c(1, 2, 4, 6)]
        }
        if(dim.data==4)
        {
            measure.1 <- data.frame(table(sub.data.1),
                                    table(sub.data.2),
                                    table(sub.data.3),
                                    table(sub.data.4))
            measure.1 <- measure.1[, c(1, 2, 4, 6, 8)]
        }
        if(dim.data==5)
        {
            measure.1 <- data.frame(table(sub.data.1),
                                    table(sub.data.2),
                                    table(sub.data.3),
                                    table(sub.data.4),
                                    table(sub.data.5))
            measure.1 <- measure.1[, c(1, 2, 4, 6, 8, 10)]
        }

        measure.1[, 1] <- paste("'", levels(sub.data.1), "'", sep="")
        label.1   <- ifelse(lang=="de", "N", "n")

        if(verbose==2) print(paste("count - measure.1: -- :", measure.1))
        if(verbose==2) print(paste("count - label.1: -- :", label.1))
    }
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # Anzahl verschiedene                               'count_distinct'                          .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    # NAs beruecksichtigt !
    #
    if(measures[1]=="count_distinct")
    {
        if(dim.data==1)
        {
            measure.1 <- c(NA, countDistinct(sub.data.1))
        }

        if(dim.data==2)
        {
            measure.1 <- c(NA
                        , countDistinct(sub.data.1)
                        , countDistinct(sub.data.2))
        }
        if(dim.data==3)
        {
            measure.1 <- c(NA
                        , countDistinct(sub.data.1)
                        , countDistinct(sub.data.2)
                        , countDistinct(sub.data.3))
        }
        if(dim.data==4)
        {
            measure.1 <- c(NA
                        , countDistinct(sub.data.1)
                        , countDistinct(sub.data.2)
                        , countDistinct(sub.data.3)
                        , countDistinct(sub.data.4))
        }
        if(dim.data==5)
        {
            measure.1 <- c(NA
                        , countDistinct(sub.data.1)
                        , countDistinct(sub.data.2)
                        , countDistinct(sub.data.3)
                        , countDistinct(sub.data.4)
                        , countDistinct(sub.data.5))
        }

        label.1   <- ifelse(lang=="de", "N", "n")

        if(verbose==2) print(paste("count_distinct - measure.1: -- :", measure.1))
        if(verbose==2) print(paste("count_distinct - label.1: -- :", label.1))
    }
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # Anteile                                           'portion'                                 .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    # NAs nicht beruecksichtigt !
    #
    if(measures[1]=="portion")
    {
      # sub.data.1 <- rep(1, 100)
      # sub.data.2 <- rep(2, 100)
      # sub.data.3 <- rep(3, 100)
        if(dim.data==1)
        {
            measure.1 <- data.frame(table(sub.data.1) / length(na.omit(sub.data.1)) * 100)
            measure.1 <- measure.1[, c(1, 2)]
        }

        if(dim.data==2)
        {
            measure.1 <- data.frame(table(sub.data.1) / length(na.omit(sub.data.1)) * 100,
                                    table(sub.data.2) / length(na.omit(sub.data.2)) * 100)
            measure.1 <- measure.1[, c(1, 2, 4)]
        }
        if(dim.data==3)
        {
            measure.1 <- data.frame(table(sub.data.1) / length(na.omit(sub.data.1)) * 100,
                                    table(sub.data.2) / length(na.omit(sub.data.2)) * 100,
                                    table(sub.data.3) / length(na.omit(sub.data.3)) * 100)
            measure.1 <- measure.1[, c(1, 2, 4, 6)]
        }
        if(dim.data==4)
        {
            measure.1 <- data.frame(table(sub.data.1) / length(na.omit(sub.data.1)) * 100,
                                    table(sub.data.2) / length(na.omit(sub.data.2)) * 100,
                                    table(sub.data.3) / length(na.omit(sub.data.3)) * 100,
                                    table(sub.data.4) / length(na.omit(sub.data.4)) * 100)
            measure.1 <- measure.1[, c(1, 2, 4, 6, 8)]
        }
        if(dim.data==5)
        {
            measure.1 <- data.frame(table(sub.data.1) / length(na.omit(sub.data.1)) * 100,
                                    table(sub.data.2) / length(na.omit(sub.data.2)) * 100,
                                    table(sub.data.3) / length(na.omit(sub.data.3)) * 100,
                                    table(sub.data.4) / length(na.omit(sub.data.4)) * 100,
                                    table(sub.data.5) / length(na.omit(sub.data.5)) * 100)
            measure.1 <- measure.1[, c(1, 2, 4, 6, 8, 10)]
        }

        measure.1[, 1] <- paste("'", levels(sub.data.1), "'", sep="")
        label.1 <- ifelse(lang=="de", "%", "%")

        if(verbose==2) print(paste("portion - measure.1: -- :", measure.1))
        if(verbose==2) print(paste("portion - label.1: -- :", label.1))
    }
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # Minimum                                           'min'                                     .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    if(measures[1]=="min")
    {
        if(dim.data==1)
        {
            measure.1 <- c(NA
                        , min(sub.data.1, na.rm=TRUE))
        }
        if(dim.data==2)
        {
            measure.1 <- c(NA
                        , min(sub.data.1, na.rm=TRUE)
                        , min(sub.data.2, na.rm=TRUE))
        }
        if(dim.data==3)
        {
            measure.1 <- c(NA
                        , min(sub.data.1, na.rm=TRUE)
                        , min(sub.data.2, na.rm=TRUE)
                        , min(sub.data.3, na.rm=TRUE))
        }
        if(dim.data==4)
        {
            measure.1 <- c(NA
                        , min(sub.data.1, na.rm=TRUE)
                        , min(sub.data.2, na.rm=TRUE)
                        , min(sub.data.3, na.rm=TRUE)
                        , min(sub.data.4, na.rm=TRUE))
        }
        if(dim.data==5)
        {
            measure.1 <- c(NA
                        , min(sub.data.1, na.rm=TRUE)
                        , min(sub.data.2, na.rm=TRUE)
                        , min(sub.data.3, na.rm=TRUE)
                        , min(sub.data.4, na.rm=TRUE)
                        , min(sub.data.5, na.rm=TRUE))
        }

        label.1   <- ifelse(lang=="de", "Minimum", "minimum")

        if(verbose==2) print(paste("min - measure.1: -- :", measure.1))
        if(verbose==2) print(paste("min - label.1: -- :", label.1))
    }
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # Maximum                                           'max'                                     .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    if(measures[1]=="max")
    {
        if(dim.data==1)
        {
            measure.1 <- c(NA
                        , max(sub.data.1, na.rm=TRUE))
        }
        if(dim.data==2)
        {
            measure.1 <- c(NA
                        , max(sub.data.1, na.rm=TRUE)
                        , max(sub.data.2, na.rm=TRUE))
        }
        if(dim.data==3)
        {
            measure.1 <- c(NA
                        , max(sub.data.1, na.rm=TRUE)
                        , max(sub.data.2, na.rm=TRUE)
                        , max(sub.data.3, na.rm=TRUE))
        }
        if(dim.data==4)
        {
            measure.1 <- c(NA
                        , max(sub.data.1, na.rm=TRUE)
                        , max(sub.data.2, na.rm=TRUE)
                        , max(sub.data.3, na.rm=TRUE)
                        , max(sub.data.4, na.rm=TRUE))
        }
        if(dim.data==5)
        {
            measure.1 <- c(NA
                        , max(sub.data.1, na.rm=TRUE)
                        , max(sub.data.2, na.rm=TRUE)
                        , max(sub.data.3, na.rm=TRUE)
                        , max(sub.data.4, na.rm=TRUE)
                        , max(sub.data.5, na.rm=TRUE))
        }

        label.1   <- ifelse(lang=="de", "Maximum", "maximum")

        if(verbose==2) print(paste("max - measure.1: -- :", measure.1))
        if(verbose==2) print(paste("max - label.1: -- :", label.1))
    }
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # Mittelwert                                        'mean'                                    .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    if(measures[1]=="mean")
    {
        if(dim.data==1)
        {
            measure.1 <- c(NA
                        , mean(sub.data.1, na.rm=TRUE))
        }
        if(dim.data==2)
        {
            measure.1 <- c(NA
                        , mean(sub.data.1, na.rm=TRUE)
                        , mean(sub.data.2, na.rm=TRUE))
        }
        if(dim.data==3)
        {
            measure.1 <- c(NA
                        , mean(sub.data.1, na.rm=TRUE)
                        , mean(sub.data.2, na.rm=TRUE)
                        , mean(sub.data.3, na.rm=TRUE))
        }
        if(dim.data==4)
        {
            measure.1 <- c(NA
                        , mean(sub.data.1, na.rm=TRUE)
                        , mean(sub.data.2, na.rm=TRUE)
                        , mean(sub.data.3, na.rm=TRUE)
                        , mean(sub.data.4, na.rm=TRUE))
        }
        if(dim.data==5)
        {
            measure.1 <- c(NA
                        , mean(sub.data.1, na.rm=TRUE)
                        , mean(sub.data.2, na.rm=TRUE)
                        , mean(sub.data.3, na.rm=TRUE)
                        , mean(sub.data.4, na.rm=TRUE)
                        , mean(sub.data.5, na.rm=TRUE))
        }

        label.1   <- ifelse(lang=="de", "Mittelwert", "mean")

        if(verbose==2) print(paste("mean - measure.1: -- :", measure.1))
        if(verbose==2) print(paste("mean - label.1: -- :", label.1))
    }
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # Mittelwert gewichtet                              'wtd_mean'                                .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    if(measures[1]=="wtd_mean")
    {
      # require(Hmisc)
        requireNamespace("Hmisc")

        if(dim.data==1)
        {
            measure.1 <- c(NA,
                           Hmisc::wtd.mean(x=sub.data.1, weights=weights.1, na.rm=TRUE))
        }
        if(dim.data==2)
        {
            measure.1 <- c(NA,
                           Hmisc::wtd.mean(x=sub.data.1, weights=weights.1, na.rm=TRUE),
                           Hmisc::wtd.mean(x=sub.data.2, weights=weights.2, na.rm=TRUE))
        }
        if(dim.data==3)
        {
            measure.1 <- c(NA,
                           Hmisc::wtd.mean(x=sub.data.1, weights=weights.1, na.rm=TRUE),
                           Hmisc::wtd.mean(x=sub.data.2, weights=weights.2, na.rm=TRUE),
                           Hmisc::wtd.mean(x=sub.data.3, weights=weights.3, na.rm=TRUE))
        }
        if(dim.data==4)
        {
            measure.1 <- c(NA,
                          Hmisc::wtd.mean(x=sub.data.1, weights=weights.1, na.rm=TRUE),
                          Hmisc::wtd.mean(x=sub.data.2, weights=weights.2, na.rm=TRUE),
                          Hmisc::wtd.mean(x=sub.data.3, weights=weights.3, na.rm=TRUE),
                          Hmisc::wtd.mean(x=sub.data.4, weights=weights.4, na.rm=TRUE))
        }
        if(dim.data==5)
        {
            measure.1 <- c(NA,
                          Hmisc::wtd.mean(x=sub.data.1, weights=weights.1, na.rm=TRUE),
                          Hmisc::wtd.mean(x=sub.data.2, weights=weights.2, na.rm=TRUE),
                          Hmisc::wtd.mean(x=sub.data.3, weights=weights.3, na.rm=TRUE),
                          Hmisc::wtd.mean(x=sub.data.4, weights=weights.4, na.rm=TRUE),
                          Hmisc::wtd.mean(x=sub.data.5, weights=weights.5, na.rm=TRUE))
        }

        label.1   <- ifelse(lang=="de", "gew. Mittelw.", "weighted mean")
        i.weights <- 1

        if(verbose==2) print(paste("wtd.mean - measure.1: -- :", measure.1))
        if(verbose==2) print(paste("wtd.mean - label.1: -- :", label.1))
    }
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # Median                                            'median'                                  .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    if(measures[1]=="median")
    {
        if(dim.data==1)
        {
            measure.1 <- c(NA
                        , median(sub.data.1, na.rm=TRUE))
        }
        if(dim.data==2)
        {
            measure.1 <- c(NA
                        , median(sub.data.1, na.rm=TRUE)
                        , median(sub.data.2, na.rm=TRUE))
        }
        if(dim.data==3)
        {
            measure.1 <- c(NA
                        , median(sub.data.1, na.rm=TRUE)
                        , median(sub.data.2, na.rm=TRUE)
                        , median(sub.data.3, na.rm=TRUE))
        }
        if(dim.data==4)
        {
            measure.1 <- c(NA
                        , median(sub.data.1, na.rm=TRUE)
                        , median(sub.data.2, na.rm=TRUE)
                        , median(sub.data.3, na.rm=TRUE)
                        , median(sub.data.4, na.rm=TRUE))
        }
        if(dim.data==5)
        {
            measure.1 <- c(NA
                        , median(sub.data.1, na.rm=TRUE)
                        , median(sub.data.2, na.rm=TRUE)
                        , median(sub.data.3, na.rm=TRUE)
                        , median(sub.data.4, na.rm=TRUE)
                        , median(sub.data.5, na.rm=TRUE))
        }

        label.1   <- ifelse(lang=="de", "Median", "median")

        if(verbose==2) print(paste("median - measure.1: -- :", measure.1))
        if(verbose==2) print(paste("median - label.1: -- :", label.1))
    }
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # Median gewichtet                                  'wtd_median'                              .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    if(measures[1]=="wtd_median")
    {
        requireNamespace("Hmisc")

        if(dim.data==1)
        {
            measure.1 <- c(NA,
                           Hmisc::wtd.quantile(x=sub.data.1, weights=weights.1, probs=0.5, na.rm=TRUE))
        }
        if(dim.data==2)
        {
            measure.1 <- c(NA,
                           Hmisc::wtd.quantile(x=sub.data.1, weights=weights.1, probs=0.5, na.rm=TRUE),
                           Hmisc::wtd.quantile(x=sub.data.2, weights=weights.2, probs=0.5, na.rm=TRUE))
        }
        if(dim.data==3)
        {
            measure.1 <- c(NA,
                           Hmisc::wtd.quantile(x=sub.data.1, weights=weights.1, probs=0.5, na.rm=TRUE),
                           Hmisc::wtd.quantile(x=sub.data.2, weights=weights.2, probs=0.5, na.rm=TRUE),
                           Hmisc::wtd.quantile(x=sub.data.3, weights=weights.3, probs=0.5, na.rm=TRUE))
        }
        if(dim.data==4)
        {
            measure.1 <- c(NA,
                          Hmisc::wtd.quantile(x=sub.data.1, weights=weights.1, probs=0.5, na.rm=TRUE),
                          Hmisc::wtd.quantile(x=sub.data.2, weights=weights.2, probs=0.5, na.rm=TRUE),
                          Hmisc::wtd.quantile(x=sub.data.3, weights=weights.3, probs=0.5, na.rm=TRUE),
                          Hmisc::wtd.quantile(x=sub.data.4, weights=weights.4, probs=0.5, na.rm=TRUE))
        }
        if(dim.data==5)
        {
            measure.1 <- c(NA,
                          Hmisc::wtd.quantile(x=sub.data.1, weights=weights.1, probs=0.5, na.rm=TRUE),
                          Hmisc::wtd.quantile(x=sub.data.2, weights=weights.2, probs=0.5, na.rm=TRUE),
                          Hmisc::wtd.quantile(x=sub.data.3, weights=weights.3, probs=0.5, na.rm=TRUE),
                          Hmisc::wtd.quantile(x=sub.data.4, weights=weights.4, probs=0.5, na.rm=TRUE),
                          Hmisc::wtd.quantile(x=sub.data.5, weights=weights.5, probs=0.5, na.rm=TRUE))
        }

        label.1   <- ifelse(lang=="de", "gew. Median", "weighted median")
        i.weights <- 1

        if(verbose==2) print(paste("wtd.median - measure.1: -- :", measure.1))
        if(verbose==2) print(paste("wtd.median - label.1: -- :", label.1))
    }
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # Standardabweichung                                'sd'                                      .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    if(measures[1]=="sd")
    {
        if(dim.data==1)
        {
            measure.1 <- c(NA
                        , sd(sub.data.1, na.rm=TRUE))
        }
        if(dim.data==2)
        {
            measure.1 <- c(NA
                        , sd(sub.data.1, na.rm=TRUE)
                        , sd(sub.data.2, na.rm=TRUE))
        }
        if(dim.data==3)
        {
            measure.1 <- c(NA
                        , sd(sub.data.1, na.rm=TRUE)
                        , sd(sub.data.2, na.rm=TRUE)
                        , sd(sub.data.3, na.rm=TRUE))
        }
        if(dim.data==4)
        {
            measure.1 <- c(NA
                        , sd(sub.data.1, na.rm=TRUE)
                        , sd(sub.data.2, na.rm=TRUE)
                        , sd(sub.data.3, na.rm=TRUE)
                        , sd(sub.data.4, na.rm=TRUE))
        }
        if(dim.data==5)
        {
            measure.1 <- c(NA
                        , sd(sub.data.1, na.rm=TRUE)
                        , sd(sub.data.2, na.rm=TRUE)
                        , sd(sub.data.3, na.rm=TRUE)
                        , sd(sub.data.4, na.rm=TRUE)
                        , sd(sub.data.5, na.rm=TRUE))
        }

        label.1   <- ifelse(lang=="de", "Standardabw.", "sd.")

        if(verbose==2) print(paste("sd - measure.1: -- :", measure.1))
        if(verbose==2) print(paste("sd - label.1: -- :", label.1))
    }
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # IQR                                               'IQR'                                     .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    if(measures[1]=="IQR")
    {
        if(dim.data==1)
        {
            measure.1 <- c(NA
                        , IQR(sub.data.1, na.rm=TRUE))
        }
        if(dim.data==2)
        {
            measure.1 <- c(NA
                        , IQR(sub.data.1, na.rm=TRUE)
                        , IQR(sub.data.2, na.rm=TRUE))
        }
        if(dim.data==3)
        {
            measure.1 <- c(NA
                        , IQR(sub.data.1, na.rm=TRUE)
                        , IQR(sub.data.2, na.rm=TRUE)
                        , IQR(sub.data.3, na.rm=TRUE))
        }
        if(dim.data==4)
        {
            measure.1 <- c(NA
                        , IQR(sub.data.1, na.rm=TRUE)
                        , IQR(sub.data.2, na.rm=TRUE)
                        , IQR(sub.data.3, na.rm=TRUE)
                        , IQR(sub.data.4, na.rm=TRUE))
        }
        if(dim.data==5)
        {
            measure.1 <- c(NA
                        , IQR(sub.data.1, na.rm=TRUE)
                        , IQR(sub.data.2, na.rm=TRUE)
                        , IQR(sub.data.3, na.rm=TRUE)
                        , IQR(sub.data.4, na.rm=TRUE)
                        , IQR(sub.data.5, na.rm=TRUE))
        }

        label.1   <- ifelse(lang=="de", "IQR", "IQR")

        if(verbose==2) print(paste("IQR - measure.1: -- :", measure.1))
        if(verbose==2) print(paste("IQR - label.1: -- :", label.1))
    }
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # Angabe Kennzahl                                                                             .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    var.measure <- label.1
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # ------------------------------------------------------------------------------------------- --
    # 2. Kennzahl berechnen
    # ------------------------------------------------------------------------------------------- --
    if(length(measures)>1)
    {
        # Anteil horizontal -- nur nach "count"         'portion' 'i' nach 'count'                .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx 
        #
        if(measures[1]=="count" & measures[2]=="portion" & var.measure.name=="i")
        {
            if(dim.data==1)
            {
                measure.2 <- c(NA
                            , length(na.omit(sub.data.1)))
                tot        <- sum(measure.2[2:2]) / 2
                measure.2[2:2] <- measure.2[2:2] * 100 / tot
            }
            if(dim.data==2)
            {
                measure.2 <- c(NA
                            , length(na.omit(sub.data.1))
                            , length(na.omit(sub.data.2)))
                tot        <- sum(measure.2[2:3]) / 2
                measure.2[2:3] <- measure.2[2:3] * 100 / tot
            }
            if(dim.data==3)
            {
                measure.2 <- c(NA
                            , length(na.omit(sub.data.1))
                            , length(na.omit(sub.data.2))
                            , length(na.omit(sub.data.3)))
                tot        <- sum(measure.2[2:4]) / 2
                measure.2[2:4] <- measure.2[2:4] * 100 / tot
            }
            if(dim.data==4)
            {
                measure.2 <- c(NA
                            , length(na.omit(sub.data.1))
                            , length(na.omit(sub.data.2))
                            , length(na.omit(sub.data.3))
                            , length(na.omit(sub.data.4)))
                tot        <- sum(measure.2[2:5]) / 2
                measure.2[2:5] <- measure.2[2:5] * 100 / tot
            }
            if(dim.data==5)
            {
                measure.2 <- c(NA
                            , length(na.omit(sub.data.1))
                            , length(na.omit(sub.data.2))
                            , length(na.omit(sub.data.3))
                            , length(na.omit(sub.data.4))
                            , length(na.omit(sub.data.5)))
                tot        <- sum(measure.2[2:6]) / 2
                measure.2[2:6] <- measure.2[2:6] * 100 / tot
            }

            measure.2 <- t(data.frame(measure.2))
            label.2   <- ifelse(lang=="de", "%", "%")

            if(verbose==2) print(paste("portion - measure.2: -- :", measure.2))
            if(verbose==2) print(paste("portion - label.2: -- :", label.2))
        }
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
        # Anteil vertikal   -- bei Faktoren             'portion'     nach 'count'                .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx 
        #
        if(measures[1]=="count" & measures[2]=="portion" & var.measure.name!="i")
        {
            if(dim.data==1)
            {
                measure.2 <- data.frame(table(sub.data.1) / length(na.omit(sub.data.1)) * 100)
                if(verbose==2) print(paste("portion - measure.2: -- :", measure.2))
                measure.2 <- measure.2[, c(1, 2)]
            }
            if(dim.data==2)
            {
                measure.2 <- data.frame(table(sub.data.1) / length(na.omit(sub.data.1)) * 100,
                                        table(sub.data.2) / length(na.omit(sub.data.2)) * 100)
                if(verbose==2) print(paste("portion - measure.2: -- :", measure.2))
                measure.2 <- measure.2[, c(1, 2, 4)]
            }
            if(dim.data==3)
            {
                measure.2 <- data.frame(table(sub.data.1) / length(na.omit(sub.data.1)) * 100,
                                        table(sub.data.2) / length(na.omit(sub.data.2)) * 100,
                                        table(sub.data.3) / length(na.omit(sub.data.3)) * 100)
                if(verbose==2) print(paste("portion - measure.2: -- :", measure.2))
                measure.2 <- measure.2[, c(1, 2, 4, 6)]
            }
            if(dim.data==4)
            {
                measure.2 <- data.frame(table(sub.data.1) / length(na.omit(sub.data.1)) * 100,
                                        table(sub.data.2) / length(na.omit(sub.data.2)) * 100,
                                        table(sub.data.3) / length(na.omit(sub.data.3)) * 100,
                                        table(sub.data.4) / length(na.omit(sub.data.4)) * 100)
                if(verbose==2) print(paste("portion - measure.2: -- :", measure.2))
                measure.2 <- measure.2[, c(1, 2, 4, 6, 8)]
            }
            if(dim.data==5)
            {
                measure.2 <- data.frame(table(sub.data.1) / length(na.omit(sub.data.1)) * 100,
                                        table(sub.data.2) / length(na.omit(sub.data.2)) * 100,
                                        table(sub.data.3) / length(na.omit(sub.data.3)) * 100,
                                        table(sub.data.4) / length(na.omit(sub.data.4)) * 100,
                                        table(sub.data.5) / length(na.omit(sub.data.5)) * 100)
                if(verbose==2) print(paste("portion - measure.2: -- :", measure.2))
                measure.2 <- measure.2[, c(1, 2, 4, 6, 8, 10)]
            }

            measure.2[, 1] <- paste("'", levels(sub.data.1), "'", sep="")
            label.2        <- ifelse(lang=="de", "%", "%")

            if(verbose==2) print(paste("portion - measure.2: -- :", measure.2))
            if(verbose==2) print(paste("portion - label.2: -- :", label.2))
        }
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
        # Mittelwert                                    'mean'                                    .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx 
        #
        if(measures[2]=="mean")
        {
            if(dim.data==1)
            {
                measure.2 <- c(NA
                               , mean(sub.data.1, na.rm=TRUE))
            }
            if(dim.data==2)
            {
                measure.2 <- c(NA
                               , mean(sub.data.1, na.rm=TRUE)
                               , mean(sub.data.2, na.rm=TRUE))
            }
            if(dim.data==3)
            {
                measure.2 <- c(NA
                               , mean(sub.data.1, na.rm=TRUE)
                               , mean(sub.data.2, na.rm=TRUE)
                               , mean(sub.data.3, na.rm=TRUE))
            }
            if(dim.data==4)
            {
                measure.2 <- c(NA
                               , mean(sub.data.1, na.rm=TRUE)
                               , mean(sub.data.2, na.rm=TRUE)
                               , mean(sub.data.3, na.rm=TRUE)
                               , mean(sub.data.4, na.rm=TRUE))
            }
            if(dim.data==5)
            {
                measure.2 <- c(NA
                               , mean(sub.data.1, na.rm=TRUE)
                               , mean(sub.data.2, na.rm=TRUE)
                               , mean(sub.data.3, na.rm=TRUE)
                               , mean(sub.data.4, na.rm=TRUE)
                               , mean(sub.data.5, na.rm=TRUE))
            }

            label.2   <- ifelse(lang=="de", "Mittelwert", "mean")

            if(verbose==2) print(paste("mean - measure.2: -- :", measure.2))
            if(verbose==2) print(paste("mean - label.2: -- :", label.2))
        }
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
        # Minimum                                       'min'                                     .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx 
        #
        if(measures[2]=="min")
        {
            if(dim.data==1)
            {
                measure.2 <- c(NA
                               , min(sub.data.1, na.rm=TRUE))
            }
            if(dim.data==2)
            {
                measure.2 <- c(NA
                               , min(sub.data.1, na.rm=TRUE)
                               , min(sub.data.2, na.rm=TRUE))
            }
            if(dim.data==3)
            {
                measure.2 <- c(NA
                               , min(sub.data.1, na.rm=TRUE)
                               , min(sub.data.2, na.rm=TRUE)
                               , min(sub.data.3, na.rm=TRUE))
            }
            if(dim.data==4)
            {
                measure.2 <- c(NA
                               , min(sub.data.1, na.rm=TRUE)
                               , min(sub.data.2, na.rm=TRUE)
                               , min(sub.data.3, na.rm=TRUE)
                               , min(sub.data.4, na.rm=TRUE))
            }
            if(dim.data==5)
            {
                measure.2 <- c(NA
                               , min(sub.data.1, na.rm=TRUE)
                               , min(sub.data.2, na.rm=TRUE)
                               , min(sub.data.3, na.rm=TRUE)
                               , min(sub.data.4, na.rm=TRUE)
                               , min(sub.data.5, na.rm=TRUE))
            }

            label.2   <- ifelse(lang=="de", "Minimum", "minimum")

            if(verbose==2) print(paste("min - measure.2: -- :", measure.2))
            if(verbose==2) print(paste("min - label.2: -- :", label.2))
        }
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
        # Maximum                                       'max'                                     .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx 
        #
        if(measures[2]=="max")
        {
            if(dim.data==1)
            {
                measure.2 <- c(NA
                               , max(sub.data.1, na.rm=TRUE))
            }
            if(dim.data==2)
            {
                measure.2 <- c(NA
                               , max(sub.data.1, na.rm=TRUE)
                               , max(sub.data.2, na.rm=TRUE))
            }
            if(dim.data==3)
            {
                measure.2 <- c(NA
                               , max(sub.data.1, na.rm=TRUE)
                               , max(sub.data.2, na.rm=TRUE)
                               , max(sub.data.3, na.rm=TRUE))
            }
            if(dim.data==4)
            {
                measure.2 <- c(NA
                               , max(sub.data.1, na.rm=TRUE)
                               , max(sub.data.2, na.rm=TRUE)
                               , max(sub.data.3, na.rm=TRUE)
                               , max(sub.data.4, na.rm=TRUE))
            }
            if(dim.data==5)
            {
                measure.2 <- c(NA
                               , max(sub.data.1, na.rm=TRUE)
                               , max(sub.data.2, na.rm=TRUE)
                               , max(sub.data.3, na.rm=TRUE)
                               , max(sub.data.4, na.rm=TRUE)
                               , max(sub.data.5, na.rm=TRUE))
            }

            label.2   <- ifelse(lang=="de", "Maximum", "maximum")

            if(verbose==2) print(paste("max - measure.2: -- :", measure.2))
            if(verbose==2) print(paste("max - label.2: -- :", label.2))
        }
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
        # Mittelwert gewichtet                          'wtd_mean'                                .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx 
        #
        if(measures[2]=="wtd_mean" & i.weights==1)  # nur wenn erste Kennzahl auch gewichtet
        {
            requireNamespace("Hmisc")

            if(dim.data==1)
            {
                measure.2 <- c(NA,
                               Hmisc::wtd.mean(x=sub.data.1, weights=weights.1, na.rm=TRUE))
            }
            if(dim.data==2)
            {
                measure.2 <- c(NA,
                               Hmisc::wtd.mean(x=sub.data.1, weights=weights.1, na.rm=TRUE),
                               Hmisc::wtd.mean(x=sub.data.2, weights=weights.2, na.rm=TRUE))
            }
            if(dim.data==3)
            {
                measure.2 <- c(NA,
                               Hmisc::wtd.mean(x=sub.data.1, weights=weights.1, na.rm=TRUE),
                               Hmisc::wtd.mean(x=sub.data.2, weights=weights.2, na.rm=TRUE),
                               Hmisc::wtd.mean(x=sub.data.3, weights=weights.3, na.rm=TRUE))
            }
            if(dim.data==4)
            {
                measure.2 <- c(NA,
                               Hmisc::wtd.mean(x=sub.data.1, weights=weights.1, na.rm=TRUE),
                               Hmisc::wtd.mean(x=sub.data.2, weights=weights.2, na.rm=TRUE),
                               Hmisc::wtd.mean(x=sub.data.3, weights=weights.3, na.rm=TRUE),
                               Hmisc::wtd.mean(x=sub.data.4, weights=weights.4, na.rm=TRUE))
            }
            if(dim.data==5)
            {
                measure.2 <- c(NA,
                               Hmisc::wtd.mean(x=sub.data.1, weights=weights.1, na.rm=TRUE),
                               Hmisc::wtd.mean(x=sub.data.2, weights=weights.2, na.rm=TRUE),
                               Hmisc::wtd.mean(x=sub.data.3, weights=weights.3, na.rm=TRUE),
                               Hmisc::wtd.mean(x=sub.data.4, weights=weights.4, na.rm=TRUE),
                               Hmisc::wtd.mean(x=sub.data.5, weights=weights.5, na.rm=TRUE))
            }

            label.2   <- ifelse(lang=="de", "gew. Mittelw.", "weighted mean")

            if(verbose==2) print(paste("wtd.mean - measure.2: -- :", measure.2))
            if(verbose==2) print(paste("wtd.mean - label.2: -- :", label.2))
        }
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
        # Median                                        'median'                                  .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx 
        #
        if(measures[2]=="median")
        {
            if(dim.data==1)
            {
                measure.2 <- c(NA
                               , median(sub.data.1, na.rm=TRUE))
            }
            if(dim.data==2)
            {
                measure.2 <- c(NA
                               , median(sub.data.1, na.rm=TRUE)
                               , median(sub.data.2, na.rm=TRUE))
            }
            if(dim.data==3)
            {
                measure.2 <- c(NA
                               , median(sub.data.1, na.rm=TRUE)
                               , median(sub.data.2, na.rm=TRUE)
                               , median(sub.data.3, na.rm=TRUE))
            }
            if(dim.data==4)
            {
                measure.2 <- c(NA
                               , median(sub.data.1, na.rm=TRUE)
                               , median(sub.data.2, na.rm=TRUE)
                               , median(sub.data.3, na.rm=TRUE)
                               , median(sub.data.4, na.rm=TRUE))
            }
            if(dim.data==5)
            {
                measure.2 <- c(NA
                               , median(sub.data.1, na.rm=TRUE)
                               , median(sub.data.2, na.rm=TRUE)
                               , median(sub.data.3, na.rm=TRUE)
                               , median(sub.data.4, na.rm=TRUE)
                               , median(sub.data.5, na.rm=TRUE))
            }

            label.2   <- ifelse(lang=="de", "Median", "median")

            if(verbose==2) print(paste("median - measure.2: -- :", measure.2))
            if(verbose==2) print(paste("median - label.2: -- :", label.2))
        }
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
        # Median gewichtet                              'wtd_median'                              .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
        #
        if(measures[2]=="wtd_median" & i.weights==1)    # nur wenn erste Kennzahl auch gewichtet
        {
            requireNamespace("Hmisc")

            if(dim.data==1)
            {
                measure.1 <- c(NA,
                               Hmisc::wtd.quantile(x=sub.data.1, weights=weights.1, probs=0.5, na.rm=TRUE))
            }
            if(dim.data==2)
            {
                measure.1 <- c(NA,
                               Hmisc::wtd.quantile(x=sub.data.1, weights=weights.1, probs=0.5, na.rm=TRUE),
                               Hmisc::wtd.quantile(x=sub.data.2, weights=weights.2, probs=0.5, na.rm=TRUE))
            }
            if(dim.data==3)
            {
                measure.1 <- c(NA,
                               Hmisc::wtd.quantile(x=sub.data.1, weights=weights.1, probs=0.5, na.rm=TRUE),
                               Hmisc::wtd.quantile(x=sub.data.2, weights=weights.2, probs=0.5, na.rm=TRUE),
                               Hmisc::wtd.quantile(x=sub.data.3, weights=weights.3, probs=0.5, na.rm=TRUE))
            }
            if(dim.data==4)
            {
                measure.1 <- c(NA,
                               Hmisc::wtd.quantile(x=sub.data.1, weights=weights.1, probs=0.5, na.rm=TRUE),
                               Hmisc::wtd.quantile(x=sub.data.2, weights=weights.2, probs=0.5, na.rm=TRUE),
                               Hmisc::wtd.quantile(x=sub.data.3, weights=weights.3, probs=0.5, na.rm=TRUE),
                               Hmisc::wtd.quantile(x=sub.data.4, weights=weights.4, probs=0.5, na.rm=TRUE))
            }
            if(dim.data==5)
            {
                measure.1 <- c(NA,
                               Hmisc::wtd.quantile(x=sub.data.1, weights=weights.1, probs=0.5, na.rm=TRUE),
                               Hmisc::wtd.quantile(x=sub.data.2, weights=weights.2, probs=0.5, na.rm=TRUE),
                               Hmisc::wtd.quantile(x=sub.data.3, weights=weights.3, probs=0.5, na.rm=TRUE),
                               Hmisc::wtd.quantile(x=sub.data.4, weights=weights.4, probs=0.5, na.rm=TRUE),
                               Hmisc::wtd.quantile(x=sub.data.5, weights=weights.5, probs=0.5, na.rm=TRUE))
            }

            label.2   <- ifelse(lang=="de", "gew. Median", "weighted median")

            if(verbose==2) print(paste("wtd_median - measure.2: -- :", measure.2))
            if(verbose==2) print(paste("wtd_median - label.2: -- :", label.2))
        }
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
        # Standardabweichung                            'sd'                                      .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx 
        #
        if(measures[2]=="sd")
        {
            if(dim.data==1)
            {
                measure.2 <- c(NA
                               , sd(sub.data.1, na.rm=TRUE))
            }
            if(dim.data==2)
            {
                measure.2 <- c(NA
                               , sd(sub.data.1, na.rm=TRUE)
                               , sd(sub.data.2, na.rm=TRUE))
            }
            if(dim.data==3)
            {
                measure.2 <- c(NA
                               , sd(sub.data.1, na.rm=TRUE)
                               , sd(sub.data.2, na.rm=TRUE)
                               , sd(sub.data.3, na.rm=TRUE))
            }
            if(dim.data==4)
            {
                measure.2 <- c(NA
                               , sd(sub.data.1, na.rm=TRUE)
                               , sd(sub.data.2, na.rm=TRUE)
                               , sd(sub.data.3, na.rm=TRUE)
                               , sd(sub.data.4, na.rm=TRUE))
            }
            if(dim.data==5)
            {
                measure.2 <- c(NA
                               , sd(sub.data.1, na.rm=TRUE)
                               , sd(sub.data.2, na.rm=TRUE)
                               , sd(sub.data.3, na.rm=TRUE)
                               , sd(sub.data.4, na.rm=TRUE)
                               , sd(sub.data.5, na.rm=TRUE))
            }

            label.2   <- ifelse(lang=="de", "Standardabw.", "sd.")

            if(verbose==2) print(paste("sd - measure.2: -- :", measure.2))
            if(verbose==2) print(paste("sd - label.2: -- :", label.2))
        }
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
        # IQR                                           'IQR'                                     .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx 
        #
        if(measures[2]=="IQR")
        {
            if(dim.data==1)
            {
                measure.2 <- c(NA
                               , IQR(sub.data.1, na.rm=TRUE))
            }
            if(dim.data==2)
            {
                measure.2 <- c(NA
                               , IQR(sub.data.1, na.rm=TRUE)
                               , IQR(sub.data.2, na.rm=TRUE))
            }
            if(dim.data==3)
            {
                measure.2 <- c(NA
                               , IQR(sub.data.1, na.rm=TRUE)
                               , IQR(sub.data.2, na.rm=TRUE)
                               , IQR(sub.data.3, na.rm=TRUE))
            }
            if(dim.data==4)
            {
                measure.2 <- c(NA
                               , IQR(sub.data.1, na.rm=TRUE)
                               , IQR(sub.data.2, na.rm=TRUE)
                               , IQR(sub.data.3, na.rm=TRUE)
                               , IQR(sub.data.4, na.rm=TRUE))
            }
            if(dim.data==5)
            {
                measure.2 <- c(NA
                               , IQR(sub.data.1, na.rm=TRUE)
                               , IQR(sub.data.2, na.rm=TRUE)
                               , IQR(sub.data.3, na.rm=TRUE)
                               , IQR(sub.data.4, na.rm=TRUE)
                               , IQR(sub.data.5, na.rm=TRUE))
            }

            label.2   <- ifelse(lang=="de", "IQR", "IQR")

            if(verbose==2) print(paste("IQR - measure.2: -- :", measure.2))
            if(verbose==2) print(paste("IQR - label.2: -- :", label.2))
        }
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
        # Angabe Kennzahlen                                                                       .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx 
        #
        # label.2     <- paste(measures[1], " (", measures[2], ")", sep="")
        var.measure <- paste(label.1,     " (", label.2, ")",     sep="")
        if(verbose==2) print(paste("var.measure.2: -- :", var.measure))
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
    }
    # ------------------------------------------------------------------------------------------- --
    # 3. Test Unterschiedlichkeit in den Subgruppen
    #    Nur wenn zwei oder drei Gruppen angegeben werden
    pValue   <- ""
    n.groups <- length(test.gr)
    if(verbose==2) print(paste("n.groups: -- :", n.groups))

    if(n.groups>1)
    {
        #
        # Zu testende Variablen zuordnen                                                          .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx 
        #
        if(test.gr[1]==1) test.gr.1 <- sub.data.1
        if(test.gr[1]==2) test.gr.1 <- sub.data.2
        if(test.gr[1]==3) test.gr.1 <- sub.data.3
        if(test.gr[1]==4) test.gr.1 <- sub.data.4
        if(test.gr[1]==5) test.gr.1 <- sub.data.5
        if(test.gr[2]==1) test.gr.2 <- sub.data.1
        if(test.gr[2]==2) test.gr.2 <- sub.data.2
        if(test.gr[2]==3) test.gr.2 <- sub.data.3
        if(test.gr[2]==4) test.gr.2 <- sub.data.4
        if(test.gr[2]==5) test.gr.2 <- sub.data.5
        if(n.groups==3)
        {
            if(test.gr[3]==1) test.gr.3 <- sub.data.1
            if(test.gr[3]==2) test.gr.3 <- sub.data.2
            if(test.gr[3]==3) test.gr.3 <- sub.data.3
            if(test.gr[3]==4) test.gr.3 <- sub.data.4
            if(test.gr[3]==5) test.gr.3 <- sub.data.5
        }
        if(n.groups==4)
        {
            if(test.gr[4]==1) test.gr.4 <- sub.data.1
            if(test.gr[4]==2) test.gr.4 <- sub.data.2
            if(test.gr[4]==3) test.gr.4 <- sub.data.3
            if(test.gr[4]==4) test.gr.4 <- sub.data.4
            if(test.gr[4]==5) test.gr.4 <- sub.data.5
        }
        if(n.groups==5)
        {
            if(test.gr[5]==1) test.gr.5 <- sub.data.1
            if(test.gr[5]==2) test.gr.5 <- sub.data.2
            if(test.gr[5]==3) test.gr.5 <- sub.data.3
            if(test.gr[5]==4) test.gr.5 <- sub.data.4
            if(test.gr[5]==5) test.gr.5 <- sub.data.5
        }
        if(verbose==2) print(paste("summary(test.gr.1): -- :"))
        if(verbose==2) print(summary(test.gr.1))
        if(verbose==2) print(paste("summary(test.gr.2): -- :"))
        if(verbose==2) print(summary(test.gr.2))
        if(n.groups>2) if(verbose==2) print(paste("summary(test.gr.3): -- :"))
        if(n.groups>2) if(verbose==2) print(summary(test.gr.3))
        if(n.groups==4) if(verbose==2) print(paste("summary(test.gr.4): -- :"))
        if(n.groups==4) if(verbose==2) print(summary(test.gr.4))
        if(n.groups==5) if(verbose==2) print(paste("summary(test.gr.5): -- :"))
        if(n.groups==5) if(verbose==2) print(summary(test.gr.5))
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
        # Gewichte zuordnen (falls noetig)                                                        .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx 
        #
        if(i.weights==1) {
            requireNamespace("Hmisc")
            if(test.gr[1]==1) test_wt_1 <- weights.1
            if(test.gr[1]==2) test_wt_1 <- weights.2
            if(test.gr[1]==3) test_wt_1 <- weights.3
            if(test.gr[1]==4) test_wt_1 <- weights.4
            if(test.gr[1]==5) test_wt_1 <- weights.5
            if(test.gr[2]==1) test_wt_2 <- weights.1
            if(test.gr[2]==2) test_wt_2 <- weights.2
            if(test.gr[2]==3) test_wt_2 <- weights.3
            if(test.gr[2]==4) test_wt_2 <- weights.4
            if(test.gr[2]==5) test_wt_2 <- weights.5
            if(n.groups==3)
            {
                if(test.gr[3]==1) test_wt_3 <- weights.1
                if(test.gr[3]==2) test_wt_3 <- weights.2
                if(test.gr[3]==3) test_wt_3 <- weights.3
                if(test.gr[3]==4) test_wt_3 <- weights.4
                if(test.gr[3]==5) test_wt_3 <- weights.5
            }
            if(n.groups==4)
            {
                if(test.gr[4]==1) test_wt_4 <- weights.1
                if(test.gr[4]==2) test_wt_4 <- weights.2
                if(test.gr[4]==3) test_wt_4 <- weights.3
                if(test.gr[4]==4) test_wt_4 <- weights.4
                if(test.gr[4]==5) test_wt_4 <- weights.5
            }
            if(n.groups==5)
            {
                if(test.gr[5]==1) test_wt_5 <- weights.1
                if(test.gr[5]==2) test_wt_5 <- weights.2
                if(test.gr[5]==3) test_wt_5 <- weights.3
                if(test.gr[5]==4) test_wt_5 <- weights.4
                if(test.gr[5]==5) test_wt_5 <- weights.5
            }
        }
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
        # Test fuer Faktorvariablen      = 'Chi-Quadrat-Test'                                     .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx 
        #
        if(var.is.factor && var.measure.name!="i")
        {
            if(verbose==2) print(paste("performing test for factors: -- :"))
            #
            # Faktoren koennen 'Anteile' und 'Anzahlen' haben !
            # ................................................................................... ..
            if(verbose==2) print(paste("performing chisq.test: -- :"))
            #
            # ................................................................................... ..
            # Mehrere Auspraegungungen, z.B. Faktorvariable wie Altersklasse
            if(n.groups==2)
            {
                test.tafel <- as.data.frame(measure.1[c(test.gr[1]+1
                                                        , test.gr[2]+1)])
                # + 1 gibt Position im Vektor wegen NA in der ersten Position !
            }
            if(n.groups==3)
            {
                test.tafel <- as.data.frame(measure.1[c(test.gr[1]+1
                                                        , test.gr[2]+1
                                                        , test.gr[3]+1)])
            }
            if(n.groups==4)
            {
                test.tafel <- as.data.frame(measure.1[c(test.gr[1]+1
                                                        , test.gr[2]+1
                                                        , test.gr[3]+1
                                                        , test.gr[4]+1)])
            }
            if(n.groups==5)
            {
                test.tafel <- as.data.frame(measure.1[c(test.gr[1]+1
                                                        , test.gr[2]+1
                                                        , test.gr[3]+1
                                                        , test.gr[4]+1
                                                        , test.gr[5]+1)])
            }
            # Mehrere Auspraegungungen, z.B. Faktorvariable wie Altersklasse
            # ................................................................................... ..
            #
            # ................................................................................... ..
            # absolute Haeufigkeiten anstatt Prozenzahlen fuer Anteile
            if(measures[1]=="portion")
            {
                if(n.groups==2)
                {
                    test.tafel[, 1] <- test.tafel[, 1] * length(test.gr.1) / 100
                    test.tafel[, 2] <- test.tafel[, 2] * length(test.gr.2) / 100
                    tt <- test.tafel[, 1] + test.tafel[, 2]
                }
                if(n.groups==3)
                {
                    test.tafel[, 1] <- test.tafel[, 1] * length(test.gr.1) / 100
                    test.tafel[, 2] <- test.tafel[, 2] * length(test.gr.2) / 100
                    test.tafel[, 3] <- test.tafel[, 3] * length(test.gr.3) / 100
                    tt <- test.tafel[, 1] + test.tafel[, 2] + test.tafel[, 3]
                }
                if(n.groups==4)
                {
                    test.tafel[, 1] <- test.tafel[, 1] * length(test.gr.1) / 100
                    test.tafel[, 2] <- test.tafel[, 2] * length(test.gr.2) / 100
                    test.tafel[, 3] <- test.tafel[, 3] * length(test.gr.3) / 100
                    test.tafel[, 4] <- test.tafel[, 4] * length(test.gr.4) / 100
                    tt <- test.tafel[, 1] + test.tafel[, 2] + test.tafel[, 3] + test.tafel[, 4]
                }
                if(n.groups==5)
                {
                    test.tafel[, 1] <- test.tafel[, 1] * length(test.gr.1) / 100
                    test.tafel[, 2] <- test.tafel[, 2] * length(test.gr.2) / 100
                    test.tafel[, 3] <- test.tafel[, 3] * length(test.gr.3) / 100
                    test.tafel[, 4] <- test.tafel[, 4] * length(test.gr.4) / 100
                    test.tafel[, 5] <- test.tafel[, 5] * length(test.gr.5) / 100
                    tt <- test.tafel[, 1] + test.tafel[, 2] + test.tafel[, 3] + test.tafel[, 4] + test.tafel[, 5]
                }

                test.tafel <- test.tafel[which(tt!=0), ]
            }
            # absolute Haeufigkeiten anstatt Prozenzahlen fuer Anteile
            # ................................................................................... ..
            #
            # ................................................................................... ..
            # Chi-Quadrat Test durchfuehren
            if(exists("test.tafel"))
            {
                chitest <- withWarnings(chisq.test(test.tafel))
                i.chisq <- 1
                pVal.suffix  <- " "
                # notify warnings!
                # at is.na(chitest$value$p.value) no warnings-message
                if(!is.null(chitest$warnings) && !is.na(chitest$value$p.value))
                {
                    i.test.warning <- 1
                    pVal.suffix    <- "*"
                }
                chitest <- chitest$value
                # chitest$p.value
                if(is.na(chitest$p.value))
                {
                    pValue <- p.na
                } else {
                    pValue <- paste(formatPValue(x=chitest$p.value, digits=3), "(c)")
                    pValue <- paste0(pValue, pVal.suffix)
                }
            } else {
                    pValue <- ""
            }
            #
            if(verbose==2) print(paste("test.tafel: -- :"))
            if(verbose==2) print(test.tafel)
            if(verbose==2) print(paste("test-output chitest: -- :"))
            if(verbose==2) print(chitest)
            # Chi-Quadrat Test durchfuehren
            # ................................................................................... ..
        }
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
        # Test fuer numerische Variablen = 'Wilcoxon-Rangsummen-Test' und 'Kruskal-Wallis-Test'   .. # {{{
        # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx 
        #
        if(!var.is.factor && var.measure.name!="i")
        {
            if(verbose==2) print(paste("performing test for non-factors: -- :"))
            #
            # ................................................................................... ..
            if(i.weights==0)        # ohne Gewichte     ...............
            {
                if(n.groups==2)
                {
                    if(verbose==2) print(paste("performing wilcox.test: -- :"))
                    if(verbose==2) print(paste("summary(test.gr.1): -- :", summary(test.gr.1)))
                    if(verbose==2) print(paste("summary(test.gr.2): -- :", summary(test.gr.2)))
                    if(length(test.gr.1)>0 && length(test.gr.2)>0 &&
                       sum(is.na(test.gr.1))!=length(test.gr.1)   &&
                       sum(is.na(test.gr.2))!=length(test.gr.2))
                    {
                        w.test      <- withWarnings(wilcox.test(x=test.gr.1, y=test.gr.2))
                        i.wilcoxon  <- 1
                        if(!is.null(w.test$warnings)) i.test.warning <- 1
                        pVal.suffix <- ifelse(is.null(w.test$warnings), " ", "*")
                        w.test      <- w.test$value
                        if(verbose==2) w.test$method
                        if(verbose==2) w.test$p.value
                        if(verbose==2) w.test$conf.int
                        if(verbose==2) print(paste("test-output w.test: -- :"))
                        if(verbose==2) print(w.test)
                        pv <- formatPValue(x=w.test$p.value, digits=3)
                        if(pv!="--")
                        {
                            pValue <- paste(pv, "(w)")
                            pValue <- paste0(pValue, pVal.suffix)
                        } else {
                            pValue <- p.na
                        }
                    } else {
                        pValue <- p.na
                    }
                }
                if(n.groups==3)
                {
                    if(verbose==2) print(paste("performing kruskal.test: -- :"))
                    if(verbose==2) print(paste("summary(test.gr.1): -- :", summary(test.gr.1)))
                    if(verbose==2) print(paste("summary(test.gr.2): -- :", summary(test.gr.2)))
                    if(verbose==2) print(paste("summary(test.gr.3): -- :", summary(test.gr.3)))
                    if(length(test.gr.1)>0 && length(test.gr.2)>0 && length(test.gr.3)>0 &&
                       sum(is.na(test.gr.1))!=length(test.gr.1)   &&
                       sum(is.na(test.gr.2))!=length(test.gr.2)   &&
                       sum(is.na(test.gr.3))!=length(test.gr.3))
                    {
                        k.test    <- withWarnings(kruskal.test(x=list(test.gr.1, test.gr.2, test.gr.3)))
                        i.kruskal <- 1
                        if(!is.null(k.test$warnings)) i.test.warning <- 1
                        pVal.suffix <- ifelse(is.null(k.test$warnings), " ", "*")
                        k.test <- k.test$value
                        if(verbose==2) k.test$method
                        if(verbose==2) k.test$p.value
                        if(verbose==2) k.test$conf.int
                        if(verbose==2) print(paste("test-output k.test: -- :"))
                        if(verbose==2) print(t.test)
                        pv <- formatPValue(x=k.test$p.value, digits=3)
                        if(pv!="--")
                        {
                            pValue <- paste(pv, "(k)")
                            pValue <- paste0(pValue, pVal.suffix)
                        } else {
                            pValue <- p.na
                        }
                    } else {
                        pValue <- p.na
                    }
                    #
                }
                if(n.groups==4)
                {
                    if(verbose==2) print(paste("performing kruskal.test: -- :"))
                    if(verbose==2) print(paste("summary(test.gr.1): -- :", summary(test.gr.1)))
                    if(verbose==2) print(paste("summary(test.gr.2): -- :", summary(test.gr.2)))
                    if(verbose==2) print(paste("summary(test.gr.3): -- :", summary(test.gr.3)))
                    if(verbose==2) print(paste("summary(test.gr.4): -- :", summary(test.gr.4)))
                    if(length(test.gr.1)>0 && length(test.gr.2)>0 && length(test.gr.3)>0 &&
                       length(test.gr.4)>0 &&
                       sum(is.na(test.gr.1))!=length(test.gr.1)   &&
                       sum(is.na(test.gr.2))!=length(test.gr.2)   &&
                       sum(is.na(test.gr.3))!=length(test.gr.3)   &&
                       sum(is.na(test.gr.4))!=length(test.gr.4))
                    {
                        k.test    <- withWarnings(kruskal.test(x=list(test.gr.1, test.gr.2, test.gr.3, test.gr.4)))
                        i.kruskal <- 1
                        if(!is.null(k.test$warnings)) i.test.warning <- 1
                        pVal.suffix <- ifelse(is.null(k.test$warnings), " ", "*")
                        k.test <- k.test$value
                        if(verbose==2) k.test$method
                        if(verbose==2) k.test$p.value
                        if(verbose==2) k.test$conf.int
                        if(verbose==2) print(paste("test-output k.test: -- :"))
                        if(verbose==2) print(t.test)
                        pv <- formatPValue(x=k.test$p.value, digits=3)
                        if(pv!="--")
                        {
                            pValue <- paste(pv, "(k)")
                            pValue <- paste0(pValue, pVal.suffix)
                        } else {
                            pValue <- p.na
                        }
                    } else {
                        pValue <- p.na
                    }
                    #
                }
                if(n.groups==5)
                {
                    if(verbose==2) print(paste("performing kruskal.test: -- :"))
                    if(verbose==2) print(paste("summary(test.gr.1): -- :", summary(test.gr.1)))
                    if(verbose==2) print(paste("summary(test.gr.2): -- :", summary(test.gr.2)))
                    if(verbose==2) print(paste("summary(test.gr.3): -- :", summary(test.gr.3)))
                    if(verbose==2) print(paste("summary(test.gr.4): -- :", summary(test.gr.4)))
                    if(verbose==2) print(paste("summary(test.gr.5): -- :", summary(test.gr.5)))
                    if(length(test.gr.1)>0 && length(test.gr.2)>0 && length(test.gr.3)>0 &&
                       length(test.gr.4)>0 && length(test.gr.5)>0 &&
                       sum(is.na(test.gr.1))!=length(test.gr.1)   &&
                       sum(is.na(test.gr.2))!=length(test.gr.2)   &&
                       sum(is.na(test.gr.3))!=length(test.gr.3)   &&
                       sum(is.na(test.gr.4))!=length(test.gr.4)   &&
                       sum(is.na(test.gr.5))!=length(test.gr.5))
                    {
                        k.test    <- withWarnings(kruskal.test(x=list(test.gr.1, test.gr.2, test.gr.3, test.gr.4, test.gr.5)))
                        i.kruskal <- 1
                        if(!is.null(k.test$warnings)) i.test.warning <- 1
                        pVal.suffix <- ifelse(is.null(k.test$warnings), " ", "*")
                        k.test <- k.test$value
                        if(verbose==2) k.test$method
                        if(verbose==2) k.test$p.value
                        if(verbose==2) k.test$conf.int
                        if(verbose==2) print(paste("test-output k.test: -- :"))
                        if(verbose==2) print(t.test)
                        pv <- formatPValue(x=k.test$p.value, digits=3)
                        if(pv!="--")
                        {
                            pValue <- paste(pv, "(k)")
                            pValue <- paste0(pValue, pVal.suffix)
                        } else {
                            pValue <- p.na
                        }
                    } else {
                        pValue <- p.na
                    }
                    #
                }
            } else {                # mit Gewichten     ...............
                                    # ev. mit Hmisc:wtd.rank
                                    # oder coin:wilcox_test; survey:survranktest; beide mit Formeln
              # x      <- wtd.rank(x=test.gr.1, weights=test_wt_1)
              # y      <- wtd.rank(x=test.gr.2, weights=test_wt_2)
              # w.test <- wilcox.test(x=x, y=y)
              # w.test$method
              # w.test$p.value
              # w.test$conf.int
              # ifelse(w.test$p.value<0.001, pValue <- "< 0.001"
              #                         , pValue <- sprintf("%.3f", w.test$p.value))
              #
              # gewichteter T-Test aus Paket 'weights'
              # pValue <- paste(pValue, "(w)")
              #
              # t_test <- wtd.t.test(x=test.gr.1, y=test.gr.2, weight=test_wt_1, weighty=test_wt_2,
              #                      samedata=FALSE, alternative="two.tailed")
              # pValue <- t_test$coefficients[3]
              # ifelse(pValue<0.001, pValue <- "< 0.001", pValue <- sprintf("%.3f", pValue))
              # pValue <- paste(pValue, "(t)")
                pValue <- ""
            }
        }
        #
        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        ## }}}
      # if(measures[1]=="count") pValue <- ""
    }
    # ------------------------------------------------------------------------------------------- --
    # 4. Ausgabe des Resultates
    #    Ausgabeformat festlegen                                                                  .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    # Breite fuer Text (Attributbezeichnung) ohne Kennzahlenangabe ..................................
    label.width.loc <- label.width - nchar(var.measure)
    #
    # Eine Kennzahl ................................................................................
    prec.digit.1 <- prec.digit[1]
                               sprintfFormat1 <- paste("%.", max(0, prec.digit.1), "f", sep="")
    if(measures[1]=="portion") sprintfFormat1 <- paste("%.", max(0, prec.digit.1), "f%%", sep="")
    #
    if(verbose==2) print(paste("sprintfFormat1: -- :", sprintfFormat1))
    if(verbose==2) print(paste("measure.1: -- :", measure.1))
    if(verbose==2) print(paste("length(measure.1[1]): -- :", length(measure.1[1])))
    if(verbose==2) print(paste("class(measure.1): -- :", class(measure.1)))
    if(verbose==2) print(paste("prec.digit.1: -- :", prec.digit.1))
    #
    # Zwei Kennzahlen ..............................................................................
    if(length(measures)>1)
    {
        ifelse(length(prec.digit)>1, prec.digit.2 <- prec.digit[2],
                                     prec.digit.2 <- prec.digit[1])
        #
                                   sprintfFormat2 <- paste("%.", max(0, prec.digit.2), "f", sep="")
        if(measures[2]=="portion") sprintfFormat2 <- paste("%.", max(0, prec.digit.2), "f%%", sep="")
        #
        if(verbose==2) print(paste("sprintfFormat2: -- :", sprintfFormat2))
        if(verbose==2) print(paste("measure.2: -- :", measure.2))
        if(verbose==2) print(paste("length(measure.2[1]): -- :", length(measure.2[1])))
        if(verbose==2) print(paste("class(measure.2): -- :", class(measure.2)))
        if(verbose==2) print(paste("prec.digit.2: -- :", prec.digit.2))
    }
    #   prec.digit
    #   sprintfFormat1
    #   sprintfFormat2
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    #    Resultat formatieren                                                                     .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    if(length(measures)>1)
    {
    # Ausgaben von zwei Kennzahlen pro Zeile .......................................................
        #
        if(length(dim(measure.1))>1)
        {
        # mehrere Zeilen ( Faktorstufen ) ..........................................................
            if(!is.na(measure.1[, 1]) && measure.1[, 1]=="'1'") measure.1[, 1] <- "   "
            if(!is.na(measure.1[, 1]) && measure.1[, 1]=="''")  measure.1[, 1] <- "  "
            if(label.compact==TRUE)
            {
                label1 <- paste(sprintf(paste("%-" , label.width.loc , "s" , sep=""),
                                        paste(var.measure.label,
                                              ifelse(is.na(measure.1[, 1]),
                                                     "",
                                                     measure.1[, 1])
                                              )),
                                var.measure)
                label2 <- var.measure
            } else {
                label1 <- paste(var.measure.label,
                                ifelse(is.na(measure.1[, 1]),
                                       "",
                                       measure.1[, 1])
                                )
                label2 <- var.measure
            }

            return.df <- data.frame(label1=label1, label2=label2,
                                    dat1=paste(sprintf(sprintfFormat1, round(measure.1[, 2], prec.digit.1)),
                                               " (",
                                               sprintf(sprintfFormat2, round(measure.2[, 2], prec.digit.2)),
                                               ")", sep=""))

            if(dim.data>1)
            {
                return.df <- cbind(return.df, paste(sprintf(sprintfFormat1, round(measure.1[, 3], prec.digit.1)),
                                                    " (",
                                                    sprintf(sprintfFormat2, round(measure.2[, 3], prec.digit.2)),
                                                    ")", sep="")
                )
                colnames(return.df)[4] <- "dat2"
            }
            if(dim.data>2)
            {
                return.df <- cbind(return.df, paste(sprintf(sprintfFormat1, round(measure.1[, 4], prec.digit.1)),
                                                    " (",
                                                    sprintf(sprintfFormat2, round(measure.2[, 4], prec.digit.2)),
                                                    ")", sep="")
                )
                colnames(return.df)[5] <- "dat3"
            }
            if(dim.data>3)
            {
                return.df <- cbind(return.df, paste(sprintf(sprintfFormat1, round(measure.1[, 5], prec.digit.1)),
                                                    " (",
                                                    sprintf(sprintfFormat2, round(measure.2[, 5], prec.digit.2)),
                                                    ")", sep="")
                )
                colnames(return.df)[6] <- "dat4"
            }
            if(dim.data>4)
            {
                return.df <- cbind(return.df, paste(sprintf(sprintfFormat1, round(measure.1[, 6], prec.digit.1)),
                                                    " (",
                                                    sprintf(sprintfFormat2, round(measure.2[, 6], prec.digit.2)),
                                                    ")", sep="")
                )
                colnames(return.df)[7] <- "dat5"
            }
            return.df[ , dim.data+3]        <- " "
            return.df[1, dim.data+3]        <- pValue
            colnames(return.df)[dim.data+3] <- "p-value"
        # mehrere Zeilen ( Faktorstufen ) ..........................................................
        } else {
        # eine Zeile ( numerishce Werte ) ..........................................................
            if(!is.na(measure.1[1]) && measure.1[1]=="'1'") measure.1[1] <- "   "
            if(!is.na(measure.1[1]) && measure.1[1]=="''")  measure.1[1] <- "  "
            if(label.compact==TRUE)
            {
                label1 <- paste(sprintf(paste("%-", label.width.loc, "s", sep=""),
                                        paste(var.measure.label,
                                              ifelse(is.na(measure.1[1]),
                                                     "",
                                                     measure.1[1])
                                              )),
                                var.measure)
                label2 <- var.measure
            } else {
                label1 <- paste(var.measure.label,
                                ifelse(is.na(measure.1[1]),
                                       "",
                                       measure.1[1])
                                )
                label2 <- var.measure
            }
            return.df <- data.frame(label1=label1, label2=label2,
                                    dat1=paste(sprintf(sprintfFormat1, round(measure.1[2], prec.digit.1)),
                                               " (",
                                               sprintf(sprintfFormat2, round(measure.2[2], prec.digit.2)),
                                               ")", sep=""))
            if(dim.data>1)
            {
                return.df <- cbind(return.df, paste(sprintf(sprintfFormat1, round(measure.1[3], prec.digit.1)),
                                                    " (",
                                                    sprintf(sprintfFormat2, round(measure.2[3], prec.digit.2)),
                                                    ")", sep="")
                )
                colnames(return.df)[4] <- "dat2"
            }
            if(dim.data>2)
            {
                return.df <- cbind(return.df, paste(sprintf(sprintfFormat1, round(measure.1[4], prec.digit.1)),
                                                    " (",
                                                    sprintf(sprintfFormat2, round(measure.2[4], prec.digit.2)),
                                                    ")", sep="")
                )
                colnames(return.df)[5] <- "dat3"
            }
            if(dim.data>3)
            {
                return.df <- cbind(return.df, paste(sprintf(sprintfFormat1, round(measure.1[5], prec.digit.1)),
                                                    " (",
                                                    sprintf(sprintfFormat2, round(measure.2[5], prec.digit.2)),
                                                    ")", sep="")
                )
                colnames(return.df)[6] <- "dat4"
            }
            if(dim.data>4)
            {
                return.df <- cbind(return.df, paste(sprintf(sprintfFormat1, round(measure.1[6], prec.digit.1)),
                                                    " (",
                                                    sprintf(sprintfFormat2, round(measure.2[6], prec.digit.2)),
                                                    ")", sep="")
                )
                colnames(return.df)[7] <- "dat5"
            }
            return.df[dim.data+3]           <- pValue
            colnames(return.df)[dim.data+3] <- "p-value"
        }
        # eine Zeile ( numerishce Werte ) ..........................................................
    # Ausgaben von zwei Kennzahlen pro Zeile .......................................................
    } else {
    # Ausgaben von einer Kennzahlen pro Zeile ......................................................
        if(length(dim(measure.1))>1)
        {
        # eine Zeile ( Faktorstufen ) ..............................................................
            if(!is.na(measure.1[, 1]) && measure.1[, 1]=="'1'") measure.1[, 1] <- "   "
            if(!is.na(measure.1[, 1]) && measure.1[, 1]=="''")  measure.1[, 1] <- "  "
            if(label.compact==TRUE)
            {
                label1 <- paste(sprintf(paste("%-", label.width.loc, "s", sep=""),
                                        paste(var.measure.label,
                                              ifelse(is.na(measure.1[, 1]),
                                                     "",
                                                     measure.1[, 1])
                                              )),
                                var.measure)
                label2 <- var.measure
            } else {
                label1 <- paste(var.measure.label,
                                ifelse(is.na(measure.1[, 1]),
                                       "",
                                       measure.1[, 1])
                                )
                label2 <- var.measure
            }
                 return.df <- data.frame(label1=label1, label2=label2,
                                         dat1=sprintf(sprintfFormat1, round(measure.1[, 2], prec.digit.1)))
            if(dim.data>1)
            {
                return.df <- cbind(return.df, sprintf(sprintfFormat1, round(measure.1[, 3], prec.digit.1)))
                colnames(return.df)[4] <- "dat2"
            }
            if(dim.data>2)
            {
                return.df <- cbind(return.df, sprintf(sprintfFormat1, round(measure.1[, 4], prec.digit.1)))
                colnames(return.df)[5] <- "dat3"
            }
            if(dim.data>3)
            {
                return.df <- cbind(return.df, sprintf(sprintfFormat1, round(measure.1[, 5], prec.digit.1)))
                colnames(return.df)[6] <- "dat4"
            }
            if(dim.data>4)
            {
                return.df <- cbind(return.df, sprintf(sprintfFormat1, round(measure.1[, 6], prec.digit.1)))
                colnames(return.df)[7] <- "dat5"
            }
            return.df[ , dim.data+3]        <- " "
            return.df[1, dim.data+3]        <- pValue
            colnames(return.df)[dim.data+3] <- "p-value"
        # eine Zeile ( Faktorstufen ) ..............................................................
        } else {
        # eine Zeile ( numerische Werte ) ..........................................................
            if(!is.na(measure.1[1]) && measure.1[1]=="'1'") measure.1[1] <- "   "
            if(!is.na(measure.1[1]) && measure.1[1]=="''")  measure.1[1] <- "  "
            if(label.compact==TRUE)
            {
                label1 <- paste(sprintf(paste("%-", label.width.loc, "s", sep=""),
                                        paste(var.measure.label,
                                              ifelse(is.na(measure.1[1]),
                                                     "",
                                                     measure.1[1])
                                              )),
                                var.measure)
                label2 <- var.measure
            } else {
                label1 <- paste(var.measure.label,
                                ifelse(is.na(measure.1[1]),
                                       "",
                                       measure.1[1])
                                )
                label2 <- var.measure
            }
                 return.df <- data.frame(label1=label1, label2=label2,
                                         dat1=sprintf(sprintfFormat1, round(measure.1[2], prec.digit.1)))
            if(dim.data>1)
            {
                return.df <- cbind(return.df, sprintf(sprintfFormat1, round(measure.1[3], prec.digit.1)))
                colnames(return.df)[4] <- "dat2"
            }
            if(dim.data>2)
            {
                return.df <- cbind(return.df, sprintf(sprintfFormat1, round(measure.1[4], prec.digit.1)))
                colnames(return.df)[5] <- "dat3"
            }
            if(dim.data>3)
            {
                return.df <- cbind(return.df, sprintf(sprintfFormat1, round(measure.1[5], prec.digit.1)))
                colnames(return.df)[6] <- "dat4"
            }
            if(dim.data>4)
            {
                return.df <- cbind(return.df, sprintf(sprintfFormat1, round(measure.1[6], prec.digit.1)))
                colnames(return.df)[7] <- "dat5"
            }
            return.df[dim.data+3]           <- pValue
            colnames(return.df)[dim.data+3] <- "p-value"
        }
        # eine Zeile ( numerische Werte ) ..........................................................
    # Ausgaben von einer Kennzahlen pro Zeile ......................................................
    }
    #
    # Ev. NAs angeben ..............................................................................
    if(sum(res.na>0))
    {
        res.na.df <- data.frame(t(c(var.measure.label, ifelse(lang=="de", "N 'NA'", "n 'NA'"), res.na, "")))
        if(verbose==2) print(paste("res.na: -- :", res.na))
        if(verbose==2) print(paste("res.na.df: -- :", res.na.df))
        if(verbose==2) print(paste("return.df: -- :", return.df))
        colnames(res.na.df) <- colnames(return.df)
        return.df <- rbind(res.na.df, return.df)
    }
    #
    if(label.compact==TRUE) return.df$label2 <- NULL
    if(verbose==2) print(paste("return.df: -- :", return.df))
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    #
    #    Ausgabe des Resultates                                                                   .. # {{{
    # xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx x
    #
    if(verbose>0) print(return.df)
    #
    if(missing(descr.table) || is.null(descr.table))
    {
        # Resultattabelle noch nicht vorhanden.
        return(list(return.df, c(i.chisq, i.wilcoxon, i.kruskal, i.test.warning)))
    } else {
        #
        # Resultattabelle schon vorhanden.
        # ev. zusaetzliche Spalte aufgrund von label.compact=TRUE hinzufuegen
        if(verbose==2) print(paste("class(descr.table)=='data.frame' :==", class(descr.table)=="data.frame"))
        ifelse(class(descr.table)=="data.frame"
               , coln <- colnames(descr.table)
               , coln <- colnames(descr.table[[1]])
               )
        if(length(coln) < ncol(return.df))
        {
            ifelse(lang=="de",
                   descr.table <- cbind(data.frame(a=descr.table[, 1], Messgroesse="")
                                        , descr.table[, 2:ncol(descr.table)]),
                   descr.table <- cbind(data.frame(a=descr.table[, 1], measure="")
                                        , descr.table[, 2:ncol(descr.table)])
                   )
            colnames(descr.table)[1] <- coln[1]
            coln <- colnames(descr.table)
        }
        if(verbose==2) print(paste("colnames(descr.table):==", colnames(descr.table)))
        if(verbose==2) print(paste("coln                 :==", coln))
        if(verbose==2) print(paste("colnames(return.df)  :==", colnames(return.df)))
        colnames(return.df) <- coln
        if(verbose==2) print(paste("colnames(return.df)  :==", colnames(return.df)))
        if(verbose==2) print(paste("ncol(descr.table)    :==", ncol(descr.table)))
        if(verbose==2) print(paste("ncol(return.df)      :==", ncol(return.df)))
        #
        # neue Resultate an bestehende anfuegen.
        descr.table <- rbind(descr.table, return.df)
        #
        # Wenn erste Zeile leer ist, dann soll sie geloescht werden.
        if(sum(descr.table[1, ]=="")==ncol(descr.table))
        {
            descr.table <- descr.table[2:nrow(descr.table), ]
        }
        return(list(descr.table, c(i.chisq, i.wilcoxon, i.kruskal, i.test.warning)))
    }
    #
    if(verbose==2) print(descr.table)
    #
    # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    ## }}}
    # ------------------------------------------------------------------------------------------- --
}
#
# --------------- descrMeasures  ----------------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
## }}}
# ==================================================================================================
#
# --------------- descrTable                     Kennzahlen deskriptiv -- allgemeine Funktion     .. # {{{
# RR 20150325     ------------------------------------------------------------------------------- --
#
# Kennzahlen fuer Subgruppen
#
# Moegliche Kennzahlenaufrufe:
#   - count             Faktor oder Wert mit 1 Auspraegung (<- noch zu kontrollieren!!)
#   - count_distinct    Faktor
#   - portion           Faktor -- vertikal 100%, sonst horizontal 100%
#   - mean              numerisch
#   - wtd_mean          numerisch
#   - sd                numerisch
#   - median            numerisch
#   - wtd_median        numerisch
#   - IQR               numerisch
# Als zweite Kennzahl zusaetzlich zu einer bisherigen Kennzahl
#       - portion       Faktor -- vertikal 100%, sonst horizontal 100%
#       - mean          numerisch
#       - wtd_mean      numerisch
#       - sd            numerisch
#       - median        numerisch
#       - wtd_median    numerisch
#       - IQR           numerisch
#
# Spezialfall: Variable 'i' --> wird als Indikator 'Recordeinheit' aufgefasst und als
#                               'Kollektivgroesse' ausgegeben!
#
# descrTable  <- function(descr.table=NULL            # Resultattabelle (vorformatiert)
descrTable  <- function(def.measures                # Tabelle mit Kennzahlendefinitionen
                        , sub.d1                    # Daten Spalte 1
                        , sub.d2=NULL               # Daten Spalte 2
                        , sub.d3=NULL               # Daten Spalte 3
                        , sub.d4=NULL               # Daten Spalte 4
                        , sub.d5=NULL               # Daten Spalte 5
                        , weights.1=NULL            # Gewichte (Daten Spalte 1)
                        , weights.2=NULL            # Gewichte (Daten Spalte 2)
                        , weights.3=NULL            # Gewichte (Daten Spalte 3)
                        , weights.4=NULL            # Gewichte (Daten Spalte 4)
                        , weights.5=NULL            # Gewichte (Daten Spalte 5)
                        , label.compact=FALSE       # Variablenliste und Messgroessen zusammen
                        , label.width=48            # Breite der ersten Spalte
                        , test.gr=NULL              # Welche Datenspalten sollten verglichen (getestet) werden?
                        , lang="de"                 # Sprache, 'de' oder 'en'
                        , verbose=0)                # 0 bis 2: Ausgabe Zwischenresultate
{
    #
    if(missing(def.measures) || is.null(def.measures)) def.measures <- createDefMeasures(d.data=sub.d1)
    #
    # ------------------------------------------------------------------------------------------- --
    #  Definition lokaler Funktionen                                                              ..
    #
    trim.loc <- function (x) gsub("^\\s+|\\s+$", "", x)
    #
    #
    #
    # ------------------------------------------------------------------------------------------- --
    #  Resultattabelle erstellen                    -  descr.table <- descrMeasures(...)          ..
    #
    i.test <- c(0, 0, 0, 0)
    for(i in 1:nrow(def.measures))
    {
        if(!exists("descr.table")) descr.table <- NULL
        descr.measures <- descrMeasures(descr.table=descr.table
                                        , var.measure.label=def.measures$measure_label[i]
                                        , var.measure.name=def.measures$measure_name[i]
                                        , measures=c(def.measures$measure_1[i], def.measures$measure_2[i])
                                        , measure.ref.level=def.measures$measure_ref_level[i]
                                        , prec.digit=c(def.measures$measure_prec_1[i], def.measures$measure_prec_2[i])
                                        , sub.d1=sub.d1
                                        , sub.d2=sub.d2
                                        , sub.d3=sub.d3
                                        , sub.d4=sub.d4
                                        , sub.d5=sub.d5
                                        , weights.1=weights.1
                                        , weights.2=weights.2
                                        , weights.3=weights.3
                                        , weights.4=weights.4
                                        , weights.5=weights.5
                                        , label.compact=label.compact
                                        , label.width=label.width
                                        , test.gr=test.gr
                                        , lang=lang
                                        , verbose=verbose)
        descr.table <- descr.measures[[1]]
        i.test      <- pmax(i.test, descr.measures[[2]])
    }
    descr.table[, 1] <- trim.loc(descr.table[, 1])
    descr.table[, 2] <- trim.loc(descr.table[, 2])
    #
    # ------------------------------------------------------------------------------------------- --
    if(lang=="de")
    {
        test.str <- c("(c) = Chi-Quadrat-Test", "(w) = Wilcoxon-Rangsummentest"
                      , "(k) = Kruskal-Wallis-Test", "'*' Test ergab Warnungen")
        test.str <- test.str[as.logical(i.test)]
        test.str <- paste(test.str, collapse=", ")
    } else {
        test.str <- c("(c) = Chi-squared test", "(w) = Wilcoxon rank-sum test"
                      , "(k) = Kruskal-Wallis test", "'*' test resulted warnings")
        test.str <- test.str[as.logical(i.test)]
        test.str <- paste(test.str, collapse=", ")
    }
    # ------------------------------------------------------------------------------------------- --
    return(list(descr.table, test.str))
    # ------------------------------------------------------------------------------------------- --
}
#
# --------------- descrTable -------------------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
#
# ==================================================================================================
## }}}
# ==================================================================================================
#
