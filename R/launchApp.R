#  require(sm)
#  require(zoo)
#  require(splancs)
#  require(Kendall)
#  require(deldir)
#  require(maptools)
#  require(geometry)
#  require(Matrix), only sparseMatrix
#  require(shiny)
#  require(shinyjs)
#  require(shinydashboard)
#  require(shinycssloaders)
#  require(rhandsontable)
#  require(readxl)

#' @import stats grDevices graphics
#' @import MASS shiny shinycssloaders geometry zoo readxl maptools
launchApp <- function(x, ...) {
  shiny::runApp(appDir = system.file("application", package = "gwsdat"), ...)
}
