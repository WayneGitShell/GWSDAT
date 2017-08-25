

launchApp <- function(x, ...) {
  shiny::runApp(appDir = system.file("application", package = "gwsdat"), ...)
}