
#' Launch GWSDAT into server or ExcelMode.
#'
#' @param GWSDAT_Options A list of start options (see createOptions).
#' @param session_file   A .RData file with a GWSDAT analysis session.
#' 
#' @export
#' 
#' @import stats grDevices graphics
#' @import MASS shiny shinycssloaders geometry zoo readxl maptools rhandsontable
#'
# #' @example launchApp(createOptions("Site Name"))
launchApp <- function(GWSDAT_Options = NULL, session_file = NULL) {

    # Need this here or shinyjs breaks if operating as package.
    # As a standard, this call goes into the ui but when building the package this
    # probably comes too late (have shinyjs calls inside server()).
    shinyjs::useShinyjs()

    
    if (is.null(GWSDAT_Options) && is.null(session_file)) {

      shinyApp(ui = uiFull, server = server)
    
    } else {
      
        if (!is.null(session_file)) {
          .GlobalEnv$session_file <- normalizePath(session_file)
        }
       
        .GlobalEnv$GWSDAT_Options <- GWSDAT_Options    
        shinyApp(ui = uiSimple, server = server)
    }
    
}
