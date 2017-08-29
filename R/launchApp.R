
#' Launch GWSDAT into server or ExcelMode.
#'
#' @param GWSDAT_Options A list of options. 
#' 
#' @export
#' 
#' @import stats grDevices graphics
#' @import MASS shiny shinycssloaders geometry zoo readxl maptools rhandsontable
#'
# #' @example launchApp(createOptions("Site Name"))
launchApp <- function(GWSDAT_Options = NULL) {

    # shiny::runApp(appDir = system.file("application", package = "GWSDAT"), ...)
    # GWSDAT_Options <- createOptions("a site")
    
    if (is.null(GWSDAT_Options)) {
        shinyApp(ui = uiFull, server = server)
    } else {
        .GlobalEnv$GWSDAT_Options <- GWSDAT_Options    
        shinyApp(ui = uiSimple, server = server)
    }
    
}
