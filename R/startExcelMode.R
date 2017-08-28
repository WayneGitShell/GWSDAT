


#' Start shiny in Excel mode.
#'
#' @param GWSDAT_Options A list of GWSDAT Options.
#'
#' @export
startExcelMode <- function(GWSDAT_Options = NULL) {

  if (is.null(GWSDAT_Options))
    stop("Need to specify GWSDAT_Options when running ExcelMode.")
  
  GWSDAT_Options[['ExcelMode']] <- TRUE
 
  
  # Put into global environment, so the shiny server can see it. 
  .GlobalEnv$GWSDAT_Options <- GWSDAT_Options
 

  runApp(launch.browser = TRUE )
  
}

