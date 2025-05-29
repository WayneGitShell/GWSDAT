

# Print warnings when they occur.
options(warn = 1)

#' Launches the GWSDAT Shiny application.
#' 
#' The shiny application can run in multi or single data mode. If no parameter is 
#' specified with \code{launchApp}, the application starts in multi data mode, which 
#' includes a data manager and several data import facilities. If the parameter \code{session_file}
#' was specified, the application launches in single data mode, which is limited to the 
#' analysis of the data specified by \code{session_file}. 
#' 
#'
#' @param GWSDAT_Options A list of start options created with \code{\link{createOptions}}.
#' @param session_file   Path to .rds file containing a GWSDAT analysis session.
#'
#' @return None
#'   
#' @export
#' 
#' @import stats grDevices graphics MASS shiny shinycssloaders geometry zoo rhandsontable sf
#' @importFrom shinyjs show hide delay onclick useShinyjs
#' @importFrom utils sessionInfo write.csv packageVersion
#' @importFrom readxl excel_sheets
#' 
#' @examples 
#' if(interactive()) {
#' launchApp(session_file = "path_to_GWSDAT_session.rds") # launch in single data mode.
#' launchApp()  # launch in multi data mode
#' }
launchApp <- function(GWSDAT_Options, session_file) {
  
  # For R package: Need this here or shinyjs won't work and the connection 
  # breaks - reason unknown. 
  # The Browser log will say: SCRIPT5009: 'shinyjs' is undefined.
  # The index.html will look fine. did put shinyjs::useShinyjs() into the the 
  # start of the ui() function where it belongs on default.
  shinyjs::useShinyjs()
  shiny::addResourcePath("www", system.file("www", package = "GWSDAT")) 
  if (missing(GWSDAT_Options) && missing(session_file)) {
    
    .GlobalEnv$APP_RUN_MODE <- "MultiData"
    if(!"shiny.useragg" %in% names(options())){options(shiny.useragg = FALSE)} #to avoid artefacts in spatial plots
    
    shinyApp(ui = uiFull(), server = server)
    
  } else {
    
    .GlobalEnv$APP_RUN_MODE <- "SingleData"
    
    # Capturing the instance where session_file argument is not specified in the call to launchApp but still exists as a global variable.
    # This could be improved upon!
    if(missing(session_file) & exists("session_file", envir = .GlobalEnv)){ 
      
      print("Warning: Deleting global variable name 'session_file'")
      rm("session_file",envir = .GlobalEnv)
      
    }
      
      
    if (!missing(session_file)) {
      .GlobalEnv$session_file <- normalizePath(session_file)
    } else {
      .GlobalEnv$GWSDAT_Options <- GWSDAT_Options    
    }
    
    options(shiny.launch.browser = TRUE)
    if(!"shiny.useragg" %in% names(options())){options(shiny.useragg = FALSE)} #to avoid artefacts in spatial plots
    
    shinyApp(ui = uiSimple(), server = server)
  }
  
}
