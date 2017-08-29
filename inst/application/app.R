

# Print warnings when they happen.
#options(warn = 1)
#options(error = recover)







########################### UI Section #############################################################



#ExcelMode <- FALSE

#if (exists("GWSDAT_Options", envir = .GlobalEnv))
#  if ("ExcelMode" %in% names(GWSDAT_Options))
#    ExcelMode <- GWSDAT_Options$ExcelMode


#if (ExcelMode) {
#    shinyApp(ui = ui_analysis_only, server = server)
#} else {

shinyApp(ui = uiMain, server = server)


#}


