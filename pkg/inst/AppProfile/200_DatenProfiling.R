# --------------------------------------------------------------------------------------------------
# Daten Profiling                                          öäüéè  --  UTF-8/UNIX  --  :e ++enc=utf-8
# --------------------------------------------------------------------------------------------------
#
# Für R-Daten kann ein Profiling, eine Analyse zur Bildung von Kennzahlen durchgeführt werden. Dies
# soll als Grundlage dienen um z.B. zwei, nicht identische, Datensätze zu vergleichen. Es Werden
# Kennzahlen wie z.B. Anzahl Null-Werte, Wertebereiche, Anzahl verschiedene Ausprägungen etc.
# berechnet.
# Die Profiling-Daten werden in das gleiche Vezeichnis geschrieben wo sich auch die analysierte
# Daten-Datei befindet.
#
# Funktion profileData()                                                                          ..{{{
# xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
#
profileData <- function(fn_Data = NULL,       # Daten-Filename
                        DataObj = NULL,       # Datenobjekt
                        NameOutput = NULL,    # Resultat-Datei
                        PathOutput = NULL,    # Resultat-Verzeichnis
                        overwrite = FALSE) {

  print("Start profileData()")
  # print(paste("fn_Data =", fn_Data))
  # print(paste("NameOutput =", NameOutput))
  print(paste("PathOutput =", PathOutput))

  library(data.table)

  # Hilfsfunktion 'fillList()' zum auffüllen der Records (Listen) mit vordefiniertem Platzhalter ...
  fillList <- function(CurrList, nList, entry=NA) {
    # nList <- 5; entry = "-"
    while (length(CurrList) < nList) {
      CurrList[[length(CurrList)+1]] <- rep(entry, max(1, length(CurrList[[1]])))
    }
    return(CurrList)
  }

  # Daten laden ....................................................................................
  if (!is.null(fn_Data)) {
    print(paste(".. Datei wird geladen .."))
    File <- load(fn_Data)
    assign(x="Data", get(File))
    rm(list=File)
    rm(File)
    rm(fn_Data)
  } else {
    Data <- DataObj
    rm(DataObj)
  }
  setDT(Data)

  # Statistiken berechnen ..........................................................................
  # Übersicht ......................................................................................
  print("Start 'profiling'")
  # Profile-Objekt und Hilfsobjekte erstellen                                                     ..{{{
  #x xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
  #
  ProfileSup  <- "-"
  Profile     <- data.table(ID  = 1,
                            Descriptions = "Daten Objekt",
                            Obs = ifelse(is.null(NameOutput), "Resultat", NameOutput),
                            Ant = ProfileSup,
                            SupA = ProfileSup)
  RowNum      <- nrow(Data)
  AttrNum     <- ncol(Data)
  AttrNames   <- names(Data)
  AttrClass   <- sapply(Data, class)
  # Datum - Einträge können zwei Klassen haben "POSIXct" "POSIXt", hier auf eine Bezeichnung kürzen
  AttrClass   <- sapply(AttrClass, paste, collapse = " - ")
  table(AttrClass, useNA="ifany", deparse.level=2)
  #
  ProfileWd   <- ncol(Profile)
  #
  # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  #}}}
  # Dimensionen und Attributtypen anfügen                                                         ..{{{
  #x xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
  #
  Profile     <- rbind(Profile, fillList(CurrList = list(2, "Klasse", paste(class(Data), collapse = ", ")),
                                         nList = ProfileWd, entry = ProfileSup))
  Profile     <- rbind(Profile, fillList(CurrList = list(3, "Kollektivgr\u00f6sse (Nrow)", RowNum),
                                         nList = ProfileWd, entry = ProfileSup))
  Profile     <- rbind(Profile, fillList(CurrList = list(4, "Anzahl Attribute (Ncol)",  AttrNum),
                                         nList = ProfileWd, entry = ProfileSup))
  Profile     <- rbind(Profile, fillList(CurrList = list(5, "Anzahl numerische Attribute",
                                                         sum(AttrClass == "numeric"),
                                                         sum(AttrClass == "numeric") / AttrNum),
                                         nList = ProfileWd, entry = ProfileSup))
  Profile     <- rbind(Profile, fillList(CurrList = list(6, "Anzahl Integer-Attribute",
                                                         sum(AttrClass == "integer"),
                                                         sum(AttrClass == "integer") / AttrNum),
                                         nList = ProfileWd, entry = ProfileSup))
  Profile     <- rbind(Profile, fillList(CurrList = list(7, "Anzahl Faktor-Attribute",
                                                         sum(AttrClass == "factor"),
                                                         sum(AttrClass == "factor") / AttrNum),
                                         nList = ProfileWd, entry = ProfileSup))
  Profile     <- rbind(Profile, fillList(CurrList = list(8, "Anzahl Text-Attribute",
                                                         sum(AttrClass == "character"),
                                                         sum(AttrClass == "character") / AttrNum),
                                         nList = ProfileWd, entry = ProfileSup))
  Profile     <- rbind(Profile, fillList(CurrList = list(9, "Anzahl bin\u00e4re Attribute",
                                                         sum(AttrClass == "logical"),
                                                         sum(AttrClass == "logical") / AttrNum),
                                         nList = ProfileWd, entry = ProfileSup))
  Profile     <- rbind(Profile, fillList(CurrList = list(10, "Anzahl Datum-Attribute",
                                                         length(AttrClass[grep("[POSIX]|Date", AttrClass)]) ,
                                                         length(AttrClass[grep("[POSIX]|Date", AttrClass)]) / AttrNum),
                                         nList = ProfileWd, entry = ProfileSup))
  #
  # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  #}}}
  # Attributen mit Null-Varianz anfügen                                                           ..{{{
  #x xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
  #
  ZeroVarN <- 0
  ZeroVarA <- NA
  ZeroVarV <- NA
  icount   <- 0
  cl       <- AttrNames[1]
  for (cl in AttrNames)
  {
    if (uniqueN(Data[, ..cl][[1]]) == 1)
    {
      icount   <- icount   + 1
      ZeroVarN <- ZeroVarN + 1
      ZeroVarA[icount] <- cl
      ZeroVarV[icount] <- Data[1, ..cl][[1]]
    }
  }
  if (ZeroVarN > 0) {
    Profile <- rbind(Profile, fillList(CurrList = list(rep(11, ZeroVarN),
                                                       rep("Anzahl 0-Varianz Attribute (uniform)", ZeroVarN),
                                                       rep(ZeroVarN, ZeroVarN),
                                                       ZeroVarA,
                                                       ZeroVarV),
                                       nList = ProfileWd, entry = ProfileSup))
  }
  rm(icount, ZeroVarN, ZeroVarA, ZeroVarV)
  #
  # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  #}}}
  # Vollständigkeit der Spalten resp. Spalten mit NAs                                             ..{{{
  #x xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
  #
  NaVarN  <- 0
  NaVarN1 <- 0
  NaVarN2 <- 0
  NaVarN3 <- 0
  AttrNa  <- c(NA)
  AttrNa1 <- c(NA)
  AttrNa2 <- c(NA)
  AttrNa3 <- c(NA)
  AntNa   <- c(NA)
  AntNa1  <- c(NA)
  AntNa2  <- c(NA)
  AntNa3  <- c(NA)
  icount  <- 0
  icount1 <- 0
  icount2 <- 0
  icount3 <- 0
  cl      <- AttrNames[1]
  cl      <- AttrNames[28] # 'austrittsgrund' -- ABT -- mit NAs
  for (cl in AttrNames)
  {
    # Anzahl NAs in Spalte
    NaCl <- sum(is.na(Data[, c(cl), with = FALSE]))
    if (NaCl > 0) {                     # NAs found
      icount <- icount + 1
      NaVarN <- NaVarN + 1
      AttrNa[icount] <- cl
      # AntNa[icount]  <- sprintf("%.0f%%", NaCl * 100 / RowNum)
      AntNa[icount]  <- NaCl / RowNum

      if (NaCl <= (RowNum * 0.5)) {     # NAs <= 50%
        icount1 <- icount1 + 1
        NaVarN1 <- NaVarN1 + 1
        AttrNa1[icount1] <- cl
        # AntNa1[icount1]  <- sprintf("%.0f%%", NaCl * 100 / RowNum)
        AntNa1[icount1]  <- NaCl / RowNum
      }

      if (NaCl  > (RowNum * 0.5)) {     # NAs  > 50%
        icount2 <- icount2 + 1
        NaVarN2 <- NaVarN2 + 1
        AttrNa2[icount2] <- cl
        # AntNa2[icount2]  <- sprintf("%.0f%%", NaCl * 100 / RowNum)
        AntNa2[icount2]  <- NaCl / RowNum
      }

      if (NaCl  > (RowNum * 0.9)) {     # NAs  > 90%
        icount3 <- icount3 + 1
        NaVarN3 <- NaVarN3 + 1
        AttrNa3[icount3] <- cl
        # AntNa3[icount3]  <- sprintf("%.0f%%", NaCl * 100 / RowNum)
        AntNa3[icount3]  <- NaCl / RowNum
      }
    }
    rm(NaCl)
  }
  Profile <- rbind(Profile, fillList(CurrList = list(rep(12, max(1, NaVarN)),
                                                     rep("Anzahl Attribute mit NAs", max(1, NaVarN)),
                                                     rep(NaVarN, max(1, NaVarN)),
                                                     ifelse(is.na(AttrNa), ProfileSup, AttrNa),
                                                     ifelse(is.na(AntNa), ProfileSup, AntNa)),
                                     nList = ProfileWd, entry = ProfileSup))
  Profile <- rbind(Profile, fillList(CurrList = list(rep(13, max(1, NaVarN1)),
                                                     rep("Anzahl Attribute mit <= 50% NAs", max(1, NaVarN1)),
                                                     rep(NaVarN1, max(1, NaVarN1)),
                                                     ifelse(is.na(AttrNa1), ProfileSup, AttrNa1),
                                                     ifelse(is.na(AntNa1), ProfileSup, AntNa1)),
                                     nList = ProfileWd, entry = ProfileSup))
  Profile <- rbind(Profile, fillList(CurrList = list(rep(14, max(1, NaVarN2)),
                                                     rep("Anzahl Attribute mit  > 50% NAs", max(1, NaVarN2)),
                                                     rep(NaVarN2, max(1, NaVarN2)),
                                                     ifelse(is.na(AttrNa2), ProfileSup, AttrNa2),
                                                     ifelse(is.na(AntNa2), ProfileSup, AntNa2)),
                                     nList = ProfileWd, entry = ProfileSup))
  Profile <- rbind(Profile, fillList(CurrList = list(rep(15, max(1, NaVarN3)),
                                                     rep("Anzahl Attribute mit  > 90% NAs", max(1, NaVarN3)),
                                                     rep(NaVarN3, max(1, NaVarN3)),
                                                     ifelse(is.na(AttrNa3), ProfileSup, AttrNa3),
                                                     ifelse(is.na(AntNa3), ProfileSup, AntNa3)),
                                     nList = ProfileWd, entry = ProfileSup))
  rm(icount, icount1, icount2, icount3)
  rm(NaVarN, NaVarN1, NaVarN2, NaVarN3)
  rm(AttrNa, AttrNa1, AttrNa2, AttrNa3)
  rm(AntNa,  AntNa1,  AntNa2,  AntNa3)
  #
  # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  #}}}
    # Profile

  # Text - Attribute ...............................................................................
  print("Start Text - Attribute  ( 1 von 7 ) ")
  # Text - Attribute                                                                              ..{{{
  #x xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
  #
  AttrCharacter <- names(AttrClass[AttrClass == "character"])
  ProfileChar   <- data.table(name = character(), distinct = character(), countNA = character(),
                              likeNumber = character())
  returnWarning <- function(x) {
    return("A")
  }
  returnError <- function(x) {
    return("A")
  }
  AttrCharLikeNum <- c()

  Attr   <- AttrCharacter[3]
  Attr   <- AttrCharacter[4]
  for (Attr in AttrCharacter)
  {
    # Text-Attribut das wie ein Zahl-Attribut ausschaut
    AttrVal <- Data[, ..Attr][[1]]
    if (sum(is.na(AttrVal)) > 0) {
        AttrVal <- AttrVal[!is.na(AttrVal)]
    }
    likeNum <- tryCatch(as.numeric(AttrVal), warning = returnWarning, error = returnError)[1]
    likeNum <- ifelse(is.na(likeNum), "A", likeNum)

    # Kennzahlen
    AttrVal  <- Data[, ..Attr][[1]]
    ProfAttr <- list(Attr,
                     uniqueN(  AttrVal),
                     sum(is.na(AttrVal)),
                     ifelse(likeNum == "A", FALSE, TRUE))
    ProfileChar <- rbind(ProfileChar, ProfAttr)

    # print(Attr)
    # print(likeNum)
    if(likeNum != "A") AttrCharLikeNum <- c(AttrCharLikeNum, Attr)
  }

  ProfileChar$distinct   <- as.numeric(ProfileChar$distinct)
  ProfileChar$countNA    <- as.numeric(ProfileChar$countNA)
  ProfileChar$likeNumber <- as.logical(ProfileChar$likeNumber)
  #
  # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  #}}}
    # ProfileChar

  # Faktor - Attribute .............................................................................
  print("Start Faktor - Attribute  ( 2 von 7 ) ")
  # Faktor - Attribute                                                                            ..{{{
  #x xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
  #
  AttrFactor    <- names(AttrClass[AttrClass == "factor"])
  ProfileFactor <- data.table(name     = character(), countNA = character(),
                              distinct = character(), levels  = character())

  Attr   <- AttrFactor[1]
  Attr   <- AttrFactor[4]
  for (Attr in AttrFactor)
  {
    # Kennzahlen
    AttrVal  <- Data[, ..Attr][[1]]
    ProfAttr <- list(Attr,
                     sum(is.na(AttrVal)),
                     uniqueN(  AttrVal, na.rm = TRUE),
                     length(levels(AttrVal)))
    ProfileFactor <- rbind(ProfileFactor, ProfAttr)
  }

  ProfileFactor$countNA    <- as.numeric(ProfileFactor$countNA)
  ProfileFactor$distinct   <- as.numeric(ProfileFactor$distinct)
  ProfileFactor$levels     <- as.numeric(ProfileFactor$levels)
  #
  # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  #}}}
    # ProfileFactor

  # Numerische Attribute ...........................................................................
  print("Start Numerische - Attribute  ( 3 von 7 ) ")
  # Numerische Attribute                                                                          ..{{{
  #x xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
  #
  AttrNumeric <- names(AttrClass[AttrClass == "numeric"])
  ProfileNum  <- data.table(name      = character(), distinct = character(), countNA = character(),
                            countZero = character(), min      = character(), max     = character(),
                            mean      = character(), median   = character(),
                            q25       = character(), q75      = character())
  Attr   <- AttrNumeric[30]
  Attr   <- AttrNumeric[83]
  for (Attr in AttrNumeric)
  {
    # print(AttrVal)
    AttrVal  <- Data[, ..Attr][[1]]
    SumNA    <- sum(is.na(AttrVal))
    ProfAttr <- list(Attr,
                     uniqueN(AttrVal),
                     SumNA,
                     sum(!is.na(AttrVal) & AttrVal == 0),
                     ifelse(SumNA == length(AttrVal), NA, min(   AttrVal, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, max(   AttrVal, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, mean(  AttrVal, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, median(AttrVal, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, quantile(AttrVal, 0.25, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, quantile(AttrVal, 0.75, na.rm = TRUE)))
    ProfileNum <- rbind(ProfileNum, ProfAttr)
  }

  ProfileNum$distinct  <- as.numeric(ProfileNum$distinct)
  ProfileNum$countNA   <- as.numeric(ProfileNum$countNA)
  ProfileNum$countZero <- as.numeric(ProfileNum$countZero)
  ProfileNum$min       <- as.numeric(ProfileNum$min)
  ProfileNum$max       <- as.numeric(ProfileNum$max)
  ProfileNum$mean      <- as.numeric(ProfileNum$mean)
  ProfileNum$median    <- as.numeric(ProfileNum$median)
  ProfileNum$q25       <- as.numeric(ProfileNum$q25)
  ProfileNum$q75       <- as.numeric(ProfileNum$q75)
  #
  # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  #}}}
    # ProfileNum

  # Text Attribute mit numerischen Werten ..........................................................
  print("Start Text - Attribute mit numerischen Werten  ( 4 von 7 ) ")
  # Numerische Attribute                                                                          ..{{{
  #x xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
  #
  ProfileCharLikeNum  <- data.table(name      = character(), distinct  = character(),
                                    countNA   = character(), countZero = character(),
                                    min       = character(), max       = character(),
                                    mean      = character(), median    = character(),
                                    q25       = character(), q75       = character())
  for (Attr in AttrCharLikeNum)
  {
    # print(AttrVal)
    AttrVal  <- Data[, ..Attr][[1]]
    AttrVal  <- as.numeric(AttrVal)
    SumNA    <- sum(is.na(AttrVal))
    ProfAttr <- list(Attr,
                     uniqueN(AttrVal),
                     SumNA,
                     sum(!is.na(AttrVal) & AttrVal == 0),
                     ifelse(SumNA == length(AttrVal), NA, min(   AttrVal, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, max(   AttrVal, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, mean(  AttrVal, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, median(AttrVal, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, quantile(AttrVal, 0.25, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, quantile(AttrVal, 0.75, na.rm = TRUE)))
    ProfileCharLikeNum <- rbind(ProfileCharLikeNum, ProfAttr)
  }

  ProfileCharLikeNum$distinct  <- as.numeric(ProfileCharLikeNum$distinct)
  ProfileCharLikeNum$countNA   <- as.numeric(ProfileCharLikeNum$countNA)
  ProfileCharLikeNum$countZero <- as.numeric(ProfileCharLikeNum$countZero)
  ProfileCharLikeNum$min       <- as.numeric(ProfileCharLikeNum$min)
  ProfileCharLikeNum$max       <- as.numeric(ProfileCharLikeNum$max)
  ProfileCharLikeNum$mean      <- as.numeric(ProfileCharLikeNum$mean)
  ProfileCharLikeNum$median    <- as.numeric(ProfileCharLikeNum$median)
  ProfileCharLikeNum$q25       <- as.numeric(ProfileCharLikeNum$q25)
  ProfileCharLikeNum$q75       <- as.numeric(ProfileCharLikeNum$q75)
  #
  # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  #}}}
    # ProfileCharLikeNum

  # Integer Attribute ..............................................................................
  print("Start Integer - Attribute  ( 5 von 7 ) ")
  # Integer Attribute                                                                             ..{{{
  #x xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
  #
  AttrInteger <- names(AttrClass[AttrClass == "integer"])
  ProfileInt  <- data.table(name      = character(), distinct = character(), countNA = character(),
                            countZero = character(), min      = character(), max     = character(),
                            mean      = character(), median   = character(),
                            q25       = character(), q75      = character())
  Attr   <- AttrInteger[30]
  Attr   <- AttrInteger[83]
  for (Attr in AttrInteger)
  {
    # print(AttrVal)
    AttrVal  <- Data[, ..Attr][[1]]
    SumNA    <- sum(is.na(AttrVal))
    ProfAttr <- list(Attr,
                     uniqueN(AttrVal),
                     SumNA,
                     sum(!is.na(AttrVal) & AttrVal == 0),
                     ifelse(SumNA == length(AttrVal), NA, min(   AttrVal, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, max(   AttrVal, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, mean(  AttrVal, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, median(AttrVal, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, quantile(AttrVal, 0.25, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, quantile(AttrVal, 0.75, na.rm = TRUE)))
    ProfileInt <- rbind(ProfileInt, ProfAttr)
  }

  ProfileInt$distinct  <- as.numeric(ProfileInt$distinct)
  ProfileInt$countNA   <- as.numeric(ProfileInt$countNA)
  ProfileInt$countZero <- as.numeric(ProfileInt$countZero)
  ProfileInt$min       <- as.numeric(ProfileInt$min)
  ProfileInt$max       <- as.numeric(ProfileInt$max)
  ProfileInt$mean      <- as.numeric(ProfileInt$mean)
  ProfileInt$median    <- as.numeric(ProfileInt$median)
  ProfileInt$q25       <- as.numeric(ProfileInt$q25)
  ProfileInt$q75       <- as.numeric(ProfileInt$q75)
  #
  # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  #}}}
    # ProfileInt

  # Datum - Attribute ..............................................................................
  print("Start Datum - Attribute  ( 6 von 7 ) ")
  # Datum - Attribute                                                                             ..{{{
  #x xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
  #
  library(lubridate)
  #
  # grep("Date", AttrClass)
  # grep("[POSIX]", AttrClass)
  AttrDate <- names(AttrClass[grep("[POSIX]|Date", AttrClass)])
  # summary(Data[, ..AttrDate])

  ProfileDate  <- data.table(name      = character(), distinct = character(), countNA = character(),
                             countZero = character(), min      = character(), max     = character(),
                             mean      = character(), median   = character(),
                             q25       = character(), q75      = character())
  Attr   <- AttrDate[1]
  Attr   <- AttrDate[30]
  Attr   <- AttrDate[83]
  for (Attr in AttrDate)
  {
    # print(AttrVal)
    # Attr <- AttrDate[1]
    # tt   <- ifelse(SumNA == length(AttrVal), NA, min(as.numeric(   AttrVal, na.rm = TRUE)))
    # tt                                 # [1] -22612
    # as.Date(tt, origin = "1970-01-01") # [1] "1908-02-04"
    AttrVal  <- Data[, ..Attr][[1]]
    AttrVal  <- as_date(AttrVal)
    AttrVal  <- as.numeric(AttrVal)
    SumNA    <- sum(is.na(AttrVal))
    ProfAttr <- list(Attr,
                     uniqueN(AttrVal),
                     SumNA,
                     sum(!is.na(AttrVal) & AttrVal == 0),
                     ifelse(SumNA == length(AttrVal), NA, min(     AttrVal,       na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, max(     AttrVal,       na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, mean(    AttrVal,       na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, median(  AttrVal,       na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, quantile(AttrVal, 0.25, na.rm = TRUE)),
                     ifelse(SumNA == length(AttrVal), NA, quantile(AttrVal, 0.75, na.rm = TRUE)))
    ProfileDate <- rbind(ProfileDate, ProfAttr)
  }

  ProfileDate$distinct  <- as.numeric(ProfileDate$distinct)
  ProfileDate$countNA   <- as.numeric(ProfileDate$countNA)
  ProfileDate$countZero <- as.numeric(ProfileDate$countZero)
  ProfileDate$min       <- as_date(as.numeric(ProfileDate$min),    origin = "1970-01-01")
  ProfileDate$max       <- as_date(as.numeric(ProfileDate$max),    origin = "1970-01-01")
  ProfileDate$mean      <- as_date(as.numeric(ProfileDate$mean),   origin = "1970-01-01")
  ProfileDate$median    <- as_date(as.numeric(ProfileDate$median), origin = "1970-01-01")
  ProfileDate$q25       <- as_date(as.numeric(ProfileDate$q25),    origin = "1970-01-01")
  ProfileDate$q75       <- as_date(as.numeric(ProfileDate$q75),    origin = "1970-01-01")
  #
  # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  #}}}
    # ProfileDate

  # Personen - Statistiken -- ev. Schlüsselattribute einführen für Personen-, Gruppen-Statistiken ..
  # Spezielle Auswertung für INT_raw und MONEY_raw .................................................{{{
  #x xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
  #
  # if (NameOutput %in% c("INT_raw", "MONEY_raw")) {
  #   if (NameOutput == "INT_raw") {
  #     setnames(Data, "EI_PARTNERNR", "PARTNERNR")
  #     setnames(Data, "EI_TYPE_CD"  , "TYPE_CD")
  #     setnames(Data, "EI_YEAR"     , "YEAR")
  #     setnames(Data, "EI_VALUE"    , "VALUE")
  #   }
  #   if (NameOutput == "MONEY_raw") {
  #     setnames(Data, "EM_PARTNERNR", "PARTNERNR")
  #     setnames(Data, "EM_TYPE_CD"  , "TYPE_CD")
  #     setnames(Data, "EM_YEAR"     , "YEAR")
  #     setnames(Data, "EM_VALUE"    , "VALUE")
  #   }
  #   # str(Data)
  #   #
  #   # Schlüssel für Typ einfügen -- aus COD
  #   Data[, TYP := ifelse(TYPE_CD == 1, "LEI", "")]
  #   Data[, TYP := ifelse(TYPE_CD == 2, "SPTG", TYP)]
  #   Data[, TYP := ifelse(TYPE_CD == 3, "KONS", TYP)]
  #   Data[, TYP := ifelse(TYPE_CD == 4, "BEL", TYP)]
  #   Data[, TYP := ifelse(TYPE_CD == 5, "PRA", TYP)]
  #   Data[, TYP := ifelse(TYPE_CD == 6, "PRO", TYP)]
  #   Data[, TYP := ifelse(TYPE_CD == 7, "RSK", TYP)]
  #   #
  #   # Summen pro Partner, Typ und Jahr berechnen
  #   ProfilePersStat  <- Data[, .(SUM_VALUE = sum(VALUE)), by = c("PARTNERNR", "TYP", "TYPE_CD", "YEAR")]
  #   #
  #   # Statistiken obiger Summen berechnen pro Typ und Jahr, d.h. Variationen pro Personen pro Typ
  #   # und Jahr
  #   ProfilePersStat1 <- ProfilePersStat[, .(min    = min(SUM_VALUE)),            by = c("TYP", "TYPE_CD", "YEAR")]
  #   ProfilePersStat2 <- ProfilePersStat[, .(max    = max(SUM_VALUE)),            by = c("TYP", "TYPE_CD", "YEAR")]
  #   ProfilePersStat3 <- ProfilePersStat[, .(mean   = mean(SUM_VALUE)),           by = c("TYP", "TYPE_CD", "YEAR")]
  #   ProfilePersStat4 <- ProfilePersStat[, .(median = median(SUM_VALUE)),         by = c("TYP", "TYPE_CD", "YEAR")]
  #   ProfilePersStat5 <- ProfilePersStat[, .(q25    = quantile(SUM_VALUE, 0.25)), by = c("TYP", "TYPE_CD", "YEAR")]
  #   ProfilePersStat6 <- ProfilePersStat[, .(q75    = quantile(SUM_VALUE, 0.75)), by = c("TYP", "TYPE_CD", "YEAR")]
  #   ProfilePersStat  <- merge(merge(merge(merge(merge(ProfilePersStat1,  ProfilePersStat2),
  #                                                     ProfilePersStat3), ProfilePersStat4),
  #                                                     ProfilePersStat5), ProfilePersStat6)
  # } else {
  #   ProfilePersStat <- NA
  # }
  #
  # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  #}}}
  # print("Start Attribute der Personen-Statistiken  ( 7 von 7 ) ")
  # ProfilePersStat

  # Resultate ......................................................................................
  assign("Profile_List", list(Profile,
                              ProfileChar,
                              ProfileNum,
                              ProfileFactor,
                              ProfileDate,
                              ProfileInt,
                              ProfileCharLikeNum))
  # ProfilePersStat))

  # Resultat-Datei sichern ...........................................................................
  if (!is.null(PathOutput)) {
    print("Resultat sichern")
    pfilen <- file.path(PathOutput, paste0("Profile_", NameOutput, ".RData"))

    if(file.exists(pfilen) & overwrite == FALSE) {
      print(paste("Datei", pfilen, "existiert schon und wird nicht neu geschrieben"))
    } else {
      save(Profile_List, version = 2, file = pfilen)
    }
  }
  #
  print("Ende profileData()")
  return(Profile_List)
}
#
# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#}}}
#
# Daten-Profiling durchführen mit 'profileData()'                                                 ..
# -- Beispiel --
if (FALSE) {
  getwd()
  list.files()
  DataPath   <- "G:/Vt/Tm/Allg/RRapold/01_R_Tools_Auswertungen"
  DataPath   <- getwd()
  fn_Data    <- file.path(DataPath, "ABT_kunde_1.RData"); file.exists(fn_Data)
  fn_Data    <- file.path(DataPath, "ABT_kunde_2.RData"); file.exists(fn_Data)
  NameOutput <- gsub(".RData", "", basename(fn_Data))
  PathOutput <- dirname(fn_Data)
  #
  load(fn_Data)
  str(ABT.kunde)
  #
  profileData(fn_Data = NULL,           # Daten-Filename
              DataObj = ABT.kunde,      # Datenobjekt
              NameOutput = NameOutput,  # Resultat-Datei
              PathOutput = NULL,        # Resultat-Verzeichnis
              overwrite = FALSE)
}
#
#
