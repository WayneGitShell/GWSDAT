


#' Start shiny in Excel mode.
#'
#' @param GWSDAT_Options A list of GWSDAT Options.
#'
#' @return
#' @export
#'
#' @examples
startExcelMode <- function(GWSDAT_Options = NULL) {

  if (is.null(GWSDAT_Options))
    stop("Need to specify GWSDAT_Options when running ExcelMode.")
  
  
  GWSDAT_Options[['ExcelMode']] <- TRUE
  
   
  # Turning server mode (headless) off causes tcltk messages and dialogs to be supressed.
  if (is.null(GWSDAT_Options$HeadlessMode)) 
    GWSDAT_Options[['HeadlessMode']] <- FALSE
  
  
  # Put into global environment, so the shiny server can see it. 
  .GlobalEnv$GWSDAT_Options <- GWSDAT_Options
 
    
  require(shiny)
  runApp(launch.browser = TRUE )
  
}

