# --------------------------------------------------------------------------------------------------
# Daten - Profiling mit Siny und Rmarkdown                 öäüéè  --  UTF-8/UNIX  --  :e ++enc=utf-8
# --------------------------------------------------------------------------------------------------
#
# Vorbereitung und Starten der Shiny - App .........................................................
library(shiny)
library(data.table)
library(knitr)
library(kableExtra)
library(lattice)
library(latticeExtra)
library(lubridate)
library(rmarkdown)
# library(shinyjs)
options(shiny.reactlog = TRUE)    # Rective Log anschauen mit Ctrl + F3
options(shiny.error = browser)    # Startet Debugger bei Fehler
options(shiny.maxRequestSize = 30*1024^2) # Maximale Filegrösse für upload auf 30MB setzen

# Liste der Berichtformate -- PDF braucht 'pdflatex'
LaTeXPath <- Sys.which("pdflatex")
if (nchar(LaTeXPath) > 1) {
  ReportChoices <- list("PDF", "HTML", "Word")
} else {
  ReportChoices <- list("HTML", "Word")
}

if (FALSE) {
  #
  # Gestartet in rrMisc ............................................................................
  # PathRun <- system.file('AppProfile', package = 'rrMisc')
  # [1] "/home/roland/R/x86_64-pc-linux-gnu-library/4.0/rrMisc/AppProfile"
  #
  # Für Entwicklung auf 'backup' ...................................................................
  PathRun <- "/you_home/roland/Dokumente_verteilt/Statistik/R_rrMisc/rrmisc/pkg/inst/AppProfile/"
  shiny::runApp(appDir = PathRun)
  # runApp(DirLoc, display.mode = "showcase")
  #
  # Ablage-Verzeichnisse
  # - Zwischendateien:  tempdir()
  # - Berichte:         /home/roland/Schreibtisch/
  tempdir()
  list.files(tempdir())
  list.files(file.path(tempdir(), "TmpDir_zu_loeschen"))
  list.files("/tmp")
}

# ToDo's ...........................................................................................
# - Anzeige Fortschritt-Indikator aka 'progress bar'
# - Abbildungen Wertebereiche aufteilen so dass Attribute mit etwa derselben
#   Wertebereich-Spannweite in der gleichen Abbildung vorkommen
# - Attribute für Statistiken (s. alter Report mit Statistiken pro Person)

# Allgemeine Funktions-Definitionen ................................................................
# Daten-Profiling -- Aus Bericht:
#   Mit diesem Report zeigt ein grobes, automatisches Daten-Profiling.
#   Es können ein oder zwei Datensätze betrachtet werden.
#   Die Datenattribute werden nach Datentyp unterschieden und ausgewertet.
#   Als erste Übersicht werden die Anzahl und Anteile der Attribute nach Datentyp ausgegeben.
#   Danach werden für jeden Datentyp die Anzahl verschiedener Werte, die Anzahl NA aufgeführt sowie
#   ein Indikator der für nicht-numerische Attribute zeigt ob der Inhalt als Zahl interpretiert
#   werden kann.

# Allgemeine Vorbereitungen ........................................................................
# Parameterwerte setzen ............................................................................{{{
# xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
#
# shinyjs Elemente (für das Schliessen der Web-Page)
# jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# def_fn_ProfileData1    <- "Data1_Profile.RData"
# def_fn_ProfileData2    <- "Data2_Profile.RData"
def_fn_ProfileData1    <- gsub("\\\\", "/", tempfile(fileext = ".RData"))
def_fn_ProfileData2    <- gsub("\\\\", "/", tempfile(fileext = ".RData"))
def_BeschreibungDaten1 <- "Daten A"
def_BeschreibungDaten2 <- "Daten B"
fn_ProfileData1        <- NA
fn_ProfileData2        <- NA
BeschreibungDaten1     <- NA
BeschreibungDaten2     <- NA
fn_Data1               <- NA
fn_Data2               <- NA

# Author                 <- "R-R"
MaxN                   <- 42
WidthNames             <- 35
PrettyN                <-  5

RmdFile               <- "202_DatenProfiling.Rmd"
# verwendet auch         "202_DatenProfilingSubmod.Rmd"
RmdPFile              <- RmdFile
ProfilingFile         <- "200_DatenProfiling.R"
ProfilingPFile        <- ProfilingFile
source(ProfilingPFile)

if (FALSE) {
  # DataExample <-
  undebug(profileData)
  Data1_Profile <- profileData(fn_Data = NULL,
                               DataObj = DataExample,
                               PathOutput = NULL,
                               overwrite = FALSE)
}
#
# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#}}}

# Shiny - Applikation ..............................................................................
# User Interface Elemente ..........................................................................{{{
# xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
#
ui <- fluidPage(
                # shinyjs Elemente (für das Schliessen der Web-Page)
                # useShinyjs(),
                # extendShinyjs(text = jscode, functions = c("closeWindow")),

                # uiOutput('markdown'),

                titlePanel(h1("Daten - Profiling")),

                sidebarPanel(p("In dieser Shiny - App kann ein Daten-Profiling von R-Daten
                               durchgeführt werden."),
                               p("Das Daten-Profiling wird in zwei Schritten vorgenonmen.
                                 Erstens werden die Input-Daten analysiert und zweitens wird
                                 damit ein Report erstellt"),
                                 # Input Daten auswählen und laden ----
                                 fileInput(inputId = "I_FileName1", label = "Auswahl erste 'Rdata' Datei"),
                                 fileInput(inputId = "I_FileName2", label = "Auswahl zweite 'Rdata' Datei"),
                                 # actionButton(inputId = "I_ladeDatei1", label = "Daten 1 laden"),
                                 # actionButton(inputId = "I_ladeDatei2", label = "Daten 2 laden"),
                                 hr(),
                                 # Data Profiling starten ----
                                 actionButton(inputId = "I_profileData1", label = "Profiling für Daten1"),
                                 actionButton(inputId = "I_profileData2", label = "Profiling für Daten2"),
                                 hr(),
                                 # Report erstellen ----
                                 textInput(inputId = "I_Title", "Titel wie er im Bericht sein sollte", "Daten-Profiling"),
                                 textInput(inputId = "I_Author", "Dein Name wie er im Bericht sein sollte"),
                                 selectInput(inputId = "I_ReportFormat",
                                             label   = "Report - Format",
                                             choices = ReportChoices),
                                 downloadButton(outputId = "O_erstelleReport", label = "Report erstellen"),
                                 hr(),
                                 # Applikation schliessen ----
                                 # actionButton(inputId = "I_stopApp", label = "Applikation beenden")
                                 ),

                               mainPanel(
                                         # Übersicht über die geladenen Daten ----
                                         h3("Übersicht Daten 1"),
                                         verbatimTextOutput(outputId = "O_Data1_Summary"),
                                         h3("Übersicht Daten 2"),
                                         verbatimTextOutput(outputId = "O_Data2_Summary"),

                                         # Übersicht über die Daten - Profile ----
                                         h3("Übersicht Profiling der Daten 1"),
                                         verbatimTextOutput(outputId = "O_Data1_Profile"),

                                         h3("Übersicht Profiling der Daten 2"),
                                         verbatimTextOutput(outputId = "O_Data2_Profile")
                                         )
                               )
#
# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#}}}
# R Code ...........................................................................................{{{
# xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
#
# 'session' für shinyjs Elemente (für das Schliessen der Web-Page)
server <- function(input, output, session) {

  # Erster Datensatz laden
  Data1 <- eventReactive(input$I_FileName1, {
             withProgress(message = "Erster Datensatz wird geladen", {
                            locFile1 <- input$I_FileName1$datapath
                            e = new.env()
                            name1 <- load(locFile1, envir = e)
                            Data1 <- e[[name1]]
                            # cat(file = stderr(), paste("Daten 1 werden geladen von", input$I_FileName1$datapath, "\n\n"))     # Status
                           })
                 })

  # Zweiter Datensatz laden
  Data2 <- eventReactive(input$I_FileName2, {
             withProgress(message = "Zweiter Datensatz wird geladen", {
                            locFile2 <- input$I_FileName2$datapath
                            e = new.env()
                            name2 <- load(locFile2, envir = e)
                            Data2 <- e[[name2]]
                            # cat(file = stderr(), paste("Daten 2 werden geladen von", input$I_FileName2$datapath, "\n\n"))     # Status
                                         })
               })

  # Übersichten erster und zweiter Datensatz
  observeEvent(input$I_FileName1, {
                 if (ncol(Data1()) < 8) {
                   output$O_Data1_Summary <- renderPrint({ head(Data1()) })
                 } else {
                   output$O_Data1_Summary <- renderPrint({ paste("Anzahl Zeilen:",  nrow(Data1()),
                                                                 "Anzahl Spalten:", ncol(Data1())) })
                 }
               })
  observeEvent(input$I_FileName2, {
                 if (ncol(Data2()) < 8) {
                   output$O_Data2_Summary <- renderPrint({ head(Data2()) })
                 } else {
                   output$O_Data2_Summary <- renderPrint({ paste("Anzahl Zeilen:",  nrow(Data2()),
                                                                 "Anzahl Spalten:", ncol(Data2())) })
                 }
               })

  # Ausführen des Data-Profiling
  observeEvent(input$I_profileData1, {
                 withProgress(message = "Daten 1 werden analysiert", {
                                Data1_Profile <<- profileData(fn_Data = NULL,
                                                              DataObj = Data1(),
                                                              PathOutput = NULL,
                                                              overwrite = FALSE)

                                fn_Data1           <<- input$I_FileName1$name

                                fn_ProfileData1    <<- def_fn_ProfileData1
                                BeschreibungDaten1 <<- def_BeschreibungDaten1
                                save(Data1_Profile, file = fn_ProfileData1)
                        })
                 })
  observeEvent(input$I_profileData2, {
                 withProgress(message = "Daten 2 werden analysiert", {
                                Data2_Profile <<- profileData(fn_Data = NULL,
                                                              DataObj = Data2(),
                                                              PathOutput = NULL,
                                                              overwrite = FALSE)

                                fn_Data2           <<- input$I_FileName2$name

                                fn_ProfileData2    <<- def_fn_ProfileData2
                                BeschreibungDaten2 <<- def_BeschreibungDaten2
                                save(Data2_Profile, file = fn_ProfileData2)
                        })
                 })

  # Anzeige der Profiling - Daten
  observeEvent(input$I_profileData1, {
                 # if (FALSE) {
                 #   load(file = file.path(getwd(), DirLoc, "Data1_Profile.RData"))
                 # }
                 output$O_Data1_Profile <- renderPrint({
                   unique(Data1_Profile[[1]][-1, c(2:3)])
                 })
                 })
  observeEvent(input$I_profileData2, {
                 # if (FALSE) {
                 #   load(file = file.path(getwd(), DirLoc, "Data2_Profile.RData"))
                 # }
                 output$O_Data2_Profile <- renderPrint({
                   unique(Data2_Profile[[1]][-1, c(2:3)])
                 })
                 })

  # Name der Analystin, des Analysten
  V_Title        <- eventReactive( input$I_Title,     { input$I_Title     })
  V_Author       <- eventReactive( input$I_Author,    { input$I_Author    })

  # Parameter für Report aufbereiten
  V_ReportType   <- eventReactive( input$I_ReportFormat, {
                                    switch(input$I_ReportFormat,
                                           "PDF"  = "pdf",
                                           "HTML" = "html",
                                           "Word" = "docx") })

  V_ReportFormat <- eventReactive( input$I_ReportFormat, {
                                    ifelse ((V_ReportType() %in% c("doc", "docx")),
                                            "word_document",
                                            paste0(V_ReportType(), "_document")) })

  # V_filename <- eventReactive( input$I_ReportFormat, {
  #                                   paste0("DataProfiling.", V_ReportType()) })

  # PDF - Report erstellen
  output$O_erstelleReport <- downloadHandler(
                                      filename = function() {paste0("DataProfiling.", V_ReportType())},
                                      # filename = V_filename(),
                                      content = function(file) {
                                        # Copy the report file to a temporary directory before
                                        # processing it, in case we do not have write permissions to
                                        # the current working dir (which can happen when deployed).
                                        # tempReport <- file.path(tempdir(), "report.Rmd")
                                        # file.copy("202_DatenProfiling.Rmd",  tempReport, overwrite = TRUE)

                                        # Set up parameters to pass to Rmd document
                                        # >> Due to an issue in 'rmarkdown' the default value of a
                                        #    parameter in the YAML header cannot be NULL, so use NA
                                        params <- list(Title              = V_Title(),
                                                       Author             = V_Author(),
                                                       fn_Data1           = fn_Data1,
                                                       fn_Data2           = fn_Data2,
                                                       OutputFormat       = V_ReportType(),
                                                       # DataPath           = getwd(),
                                                       fn_ProfileData1    = fn_ProfileData1,
                                                       fn_ProfileData2    = fn_ProfileData2,
                                                       BeschreibungDaten1 = BeschreibungDaten1,
                                                       BeschreibungDaten2 = BeschreibungDaten2,
                                                       MaxN               = MaxN,
                                                       WidthNames         = WidthNames,
                                                       PrettyN            = PrettyN
                                                       )

                                        withProgress(message = "Bericht wird erstellt", {

                                        # Knit the document, passing in the 'params' list, and eval
                                        # it in a child of the global environment (this isolates the
                                        # code in the document from the code in this app)
                                      # rmarkdown::render(tempReport,
                                        rmarkdown::render(RmdPFile,
                                                          output_file = file,
                                                          # output_format = "all",
                                                          output_format = V_ReportFormat(),
                                                          params = params,
                                                          envir = new.env(parent = globalenv())
                                                          )
                                        })

                                      }
  )

  session$onSessionEnded(function() {
                           stopApp()
                })
  # Applikation schliessen
  # observeEvent(input$I_stopApp, {
  #                js$closeWindow()
  #                stopApp()
  #              })

}
#
# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#}}}
# Shiny App ........................................................................................{{{
# xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx
#
shinyApp(ui, server)
#
# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#}}}

