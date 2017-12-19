

# Gives conflict with graphics::box:
# #' @importFrom shinydashboard box 
uiAnalyse <- function(csite, img_frmt, APP_RUN_MODE) {
  
  corner_element = paste0(csite$ui_attr$site_name, " - ", csite$Aquifer)
  
  # If in MultiData mode, include a BACK button.
  if (APP_RUN_MODE == "MultiData")
    #corner_element <- HTML(paste0(actionButton("GoToDataSelect", "", icon = icon("arrow-left"), style = "height: 30px"), "&nbsp;&nbsp;&nbsp", csite$ui_attr$site_name, " - ", csite$Aquifer)) 
    corner_element <- HTML(paste0(actionButton("GoToDataSelect", "", icon = icon("arrow-left"), style = "height: 30px"), "&nbsp;&nbsp;&nbsp", csite$ui_attr$site_name, " - ", csite$data_id)) 
  
  
  # tags$a(id = "GoToDataSelect", "<- Back", href = "#"), " ", csite$ui_attr$site_name)) 
  navbarPage(corner_element, windowTitle = csite$ui_attr$site_name, id = "analyse_panel",              
              
              tabPanel("Spatial Plot", fluid = TRUE,
                      uiSpatialImage(csite, img_frmt)),
             
             tabPanel("Time-Series", fluid = TRUE, 
                      uiTimeSeries(csite, img_frmt)),
                       
              tabPanel("Trends & Thresholds", fluid = TRUE,
                      uiTrendTable(csite)),
             
              navbarMenu("More",
                  tabPanel("Well Report", fluid = TRUE, 
                           uiWellReport(csite, img_frmt) ),
                  tabPanel("Plume Diagnostic", fluid = TRUE, 
                           uiPlumeDiagnostics(csite, img_frmt) ),
                  tabPanel("Spatiotemporal Predictions", fluid = TRUE, 
                           uiSTPredictions(csite, img_frmt) ),
                  "----",
                  tabPanel("Save Session", fluid = TRUE,
                           uiSession() ),  
                  tabPanel("Options", fluid = TRUE,
                           uiAnalyseOptions(csite)
                  )
              ) # end navbarMenu
              
  ) # end TabPanel
}

