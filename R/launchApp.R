
# Print warnings when they occur.
options(warn = 1)

#' Launch GWSDAT into server or ExcelMode.
#'
#' @param GWSDAT_Options A list of start options (see createOptions).
#' @param session_file   A .RData file with a GWSDAT analysis session.
#' 
#' @export
#' 
#' @import stats grDevices graphics MASS shiny shinycssloaders geometry zoo readxl rhandsontable sf
#' @importFrom shinyjs show hide delay onclick
#' @importFrom utils sessionInfo write.csv
launchApp <- function(GWSDAT_Options, session_file) {

    # Need this here or shinyjs breaks if operating as package, e.g. shinyjs::show() 
    # in server() does nothing.
    # As a standard, this call goes into the ui but when the package is build 
    # something with the order messes up. 
    shinyjs::useShinyjs()

   
    
    if (missing(GWSDAT_Options) && missing(session_file)) {

      .GlobalEnv$APP_RUN_MODE <- "MultiData"
      
      shinyApp(ui = uiFull, server = server)
      
    } else {

      .GlobalEnv$APP_RUN_MODE <- "SingleData"
      
      if (!missing(session_file)) {
        .GlobalEnv$session_file <- normalizePath(session_file)
      } else {
        .GlobalEnv$GWSDAT_Options <- GWSDAT_Options    
      }

      options(shiny.launch.browser = TRUE)
      
      shinyApp(ui = uiSimple, server = server)
    }
    
}
