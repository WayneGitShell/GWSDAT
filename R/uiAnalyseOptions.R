
uiAnalyseOptions <- function() {
  
  box(title = "Options", width = 8, #solidHeader = TRUE,
        
      "The following options will affect the currently selected data and model. For 
      global settings visit the _Settings_ page.",
      
      
      ## Concentration Thresholds ##############################################
      
      div(style = "margin-top:30px", 
          h4("Concentration Thresholds")),
      p(style = "", paste("Specify the thresholds in ", csite$ui_attr$conc_unit_selected, ".", sep = "")),
      
      uiOutput("thres_conc_select"),
      
      ## Plume Diagnostic ######################################################
      
      div(style = "margin-top:30px", 
          h4("Plume Diagnostic")),
      p(style = "", paste("Specify plume threshold values in ug/l.", sep = "")),
      
      uiOutput("thres_plume_select"),
      
      sliderInput("ground_porosity", "Ground Porosity (%)",
                  min = 0, 
                  max = 1, 
                  value = csite$ui_attr$ground_porosity,
                  width = '150px'
      ), 
      
      ## Image Export Settings #################################################
      
      div(style = "margin-top:50px", 
          h4("Image Export")),
      p("Default resolution for most images"), # Resolution for file formats: png and jpeg."),
      
      div(style = "display: inline-block;", 
          numericInput("img_width_px", label = "Width (pixel)", value = csite$ui_attr$img_width_px, width = "100px")
      ),
      
      div(style = "display: inline-block;", 
          numericInput("img_height_px", label = "Height (pixel)", value = csite$ui_attr$img_height_px, width = "100px") 
      ),
      
      # Disabled because it does not work, yet.
      disabled(div(style = "display: inline-block;", 
          checkboxInput("img_asp_px", label = "Keep Aspect Ratio", value = FALSE)
      )),
      
      
      ## Wide Image Resolution Settings #############################################
      
      div(style = "margin-top:30px, margin-bottom:10px", 
          p("Resolution for wide plots, such as Well Report and Plume Diagnostic.")
      ),

      div(style = "display: inline-block;", 
          numericInput("img_width_px_wide", label = "Width (pixel)", value = csite$ui_attr$img_width_px_wide, width = "100px")
      ),
      
      div(style = "display: inline-block;", 
          numericInput("img_height_px_wide", label = "Height (pixel)", value = csite$ui_attr$img_height_px_wide, width = "100px") 
      ),
      
      # Disabled because it does not work, yet.
      disabled(div(style = "display: inline-block;", 
                   checkboxInput("img_asp_px", label = "Keep Aspect Ratio", value = FALSE)
      )),
      
      
      
      ## Image Quality #########################################################
      
      div(style = "margin-top:20px", 
          sliderInput("img_jpg_quality", "JPEG Quality (%)",
                  min = 1, 
                  max = 100, 
                  value = csite$ui_attr$img_jpg_quality,
                  width = '150px'
      )), 
      
      
      
      actionButton("save_analyse_options",
                   label = "Save", icon = icon("save"), 
                  # style = "color: #fff; background-color: Coral; border-color: Chocolate; float: right")
                   style = "float: right"),
      hidden( 
        div(id = "options_save_msg",
            column(2, 
                   textOutput("options_saved"), style = "float: right; color: Green;")
            )
      )
      
      
  )
  
}

