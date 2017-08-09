
uiAnalyseOptions <- function() {
  
  box(title = "Options", width = 8, #solidHeader = TRUE,
        
      "The following options will affect the currently selected data and model. For 
      global settings visit the _Settings_ page.",
      
      
      div(style = "margin-top:30px", h4("Plume Diagnostic")),
      p(style = "", paste("Specify plume threshold values in ", pnl$rgUnits, ".", sep = "")),
      
      uiOutput("thres_plume_select"),
      
      sliderInput("ground_porosity", "Ground Porosity (%)",
                  min = 0, 
                  max = 1, 
                  value = pnl$Porosity,
                  width = '150px'
      ), 
      
      ## Default Image Resolution Settings #############################################
      
      div(style = "margin-top:30px, margin-bottom:10px", h4("Image Resolution (Default)")),
      p("Resolution for file formats: png and jpeg."),
      
      div(style = "display: inline-block;", 
          numericInput("img_width_px", label = "Width (pixel)", value = 800, width = "100px")
      ),
      
      div(style = "display: inline-block;", 
          numericInput("img_height_px", label = "Height (pixel)", value = 600, width = "100px") 
      ),
      
      # Disabled because it does not work, yet.
      disabled(div(style = "display: inline-block;", 
          checkboxInput("img_asp_px", label = "Keep Aspect Ratio", value = FALSE)
      )),
      
      p("Size for file formats: pdf, ps, wmf, and ppt."),
      
      div(style = "display: inline-block;", 
          numericInput("img_width_inch", label = "Width (inch)", value = 7, width = "100px")
      ),
      
      div(style = "display: inline-block;", 
          numericInput("img_height_inch", label = "Height (inch)", value = 5, width = "100px") 
      ),
      
      # Disabled because it does not work, yet.
      disabled(div(style = "display: inline-block;", 
                   checkboxInput("img_asp_inch", label = "Keep Aspect Ratio", value = FALSE)
      )),
      
      
      ## Wide Image Resolution Settings #############################################
      
      div(style = "margin-top:30px, margin-bottom:10px", 
          h4("Image Resolution for Wide Plots (Well Report, Plume Diagnostic)")),
      p("Resolution for file formats: png and jpeg."),
      
      div(style = "display: inline-block;", 
          numericInput("img_width_px_wide", label = "Width (pixel)", value = 1200, width = "100px")
      ),
      
      div(style = "display: inline-block;", 
          numericInput("img_height_px_wide", label = "Height (pixel)", value = 600, width = "100px") 
      ),
      
      # Disabled because it does not work, yet.
      disabled(div(style = "display: inline-block;", 
                   checkboxInput("img_asp_px", label = "Keep Aspect Ratio", value = FALSE)
      )),
      
      p("Size for file formats: pdf, ps, wmf, and ppt."),
      
      div(style = "display: inline-block;", 
          numericInput("img_width_inch_wide", label = "Width (inch)", value = 9, width = "100px")
      ),
      
      div(style = "display: inline-block;", 
          numericInput("img_height_inch_wide", label = "Height (inch)", value = 4, width = "100px") 
      ),
      
      # Disabled because it does not work, yet.
      disabled(div(style = "display: inline-block;", 
                   checkboxInput("img_asp_inch", label = "Keep Aspect Ratio", value = FALSE)
      )),
      
      
      ## Image Quality #########################################################
      
      div(style = "margin-top:20px", 
          sliderInput("img_jpg_quality", "JPEG Quality (%)",
                  min = 1, 
                  max = 100, 
                  value = 90,
                  width = '150px'
      )), 
      
      actionButton("save_analyse_options",
                   label = "Save", icon = icon("save"), 
                   style = "color: #fff; background-color: Coral; border-color: Chocolate; float: right")
      
      
  )
  
}

