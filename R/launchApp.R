
#' Launch GWSDAT into server or ExcelMode.
#'
#' @param GWSDAT_Options If this is specified, ExcelMode is executed. 
#' 
#' @export
#' 
#' @import stats grDevices graphics
#' @import MASS shiny shinycssloaders geometry zoo readxl maptools rhandsontable
#'
# #' @example launchApp(createOptions("Site Name"))
launchApp <- function(GWSDAT_Options = NULL) {

    # Need this here or shinyjs breaks if operating as package.
    # As a standard, this call goes into the ui but when building the package this
    # probably comes too late (have shinyjs call inside server()).
    shinyjs::useShinyjs()

    
    # shiny::runApp(appDir = system.file("application", package = "GWSDAT"), ...)
    # GWSDAT_Options <- createOptions("a site")
    
    if (is.null(GWSDAT_Options)) {
        shinyApp(ui = uiFull, server = server)
    } else {
        .GlobalEnv$GWSDAT_Options <- GWSDAT_Options    
        shinyApp(ui = uiSimple, server = server)
    }
    
}
