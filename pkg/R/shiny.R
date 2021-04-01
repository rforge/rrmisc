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
    shiny::runApp(appDir = system.file('AppProfile', package = 'rrMisc'))
  }
#
# --------------- profileData ------------------------------------------------------------------- --
# ENDE DER FUNKTION ----------------------------------------------------------------------------- --
# # }}}
# ==================================================================================================
