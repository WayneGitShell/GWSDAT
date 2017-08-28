#  require(sm)
#  require(zoo)
#  require(splancs)
#  require(Kendall)
#  require(deldir)
#  require(maptools)
#  require(geometry)
#  require(Matrix)
#  require(shiny)
#  require(shinyjs)
#  require(shinydashboard)
#  require(shinycssloaders)
#  require(rhandsontable)
#  require(readxl)

#' @import stats grDevices graphics
#' @import MASS shiny shinyjs shinydashboard shinycssloaders geometry zoo readxl Matrix maptools
launchApp <- function(x, ...) {
  shiny::runApp(appDir = system.file("application", package = "gwsdat"), ...)
}
