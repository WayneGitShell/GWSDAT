
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
#' @importFrom shinyjs show hide delay onclick useShinyjs
#' @importFrom utils sessionInfo write.csv packageVersion
launchApp <- function(GWSDAT_Options, session_file) {

    # For R package: Need this here or shinyjs won't work and the connection 
    # breaks - reason unknown. 
    # The Browser log will say: SCRIPT5009: 'shinyjs' is undefined.
    # The index.html will look fine. did put shinyjs::useShinyjs() into the the 
    # start of the ui() function where it belongs on default.
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
