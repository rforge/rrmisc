# ==================================================================================================
# --------------- profileData                       launch shiny app to profile data              ..{{{
# RR 20210331     ------------------------------------------------------------------------------- --
#
# Manual          ------------------------------------------------------------------------------- --
#
#' @title Run shiny-appt to profile data and create report
#'
#' @description Run shiny-appt to profile data and create report
#'
#' @param \dots other arguments
#' @return shiny app and reports in different formats
#' @note under continuous developement
#' @author Roland Rapold
#' @references none
#' @export
  profileData <- function(...) {
    PathRun <- system.file('AppProfile', package = 'rrMisc')
    shiny::runApp(appDir = PathRun)
  }
#
# --------------- profileData ------------------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
# # }}}
# ==================================================================================================
