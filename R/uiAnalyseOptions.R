
uiAnalyseOptions <- function(csite) {
  
  fluidRow(
    tags$head(
      tags$style(HTML('#save_analyse_options{background-color: #6dc0ff;
                                             border: 1px solid #030a0f;
                                             color: black;}
                       #save_analyse_options:hover{
                                background-color: #156daf;
                                color: white;
                            }
                      '))
    ),
  shinydashboard::box(title = "Options", width = 6, solidHeader = TRUE, status = "primary",
        
      #"The following options will only affect the currently selected data set.",
      div(style = "float: right",
          actionButton("save_analyse_options", label = "Save", icon = icon("save"), width = "100px" 
                   #style = "background-color: Green")
                   )), # "color: #fff; background-color: Coral; border-color: Chocolate; float: right")
      shinyjs::hidden( 
        div(id = "options_save_msg",
            column(2, textOutput("options_saved"), style = "float: right; color: Green;"))),
      
      ## Concentration Thresholds ##############################################
      
      div(style = "margin-top:40px", 
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
      p("Specify the resolution for images that contain the default plot dimensions."), # Resolution for file formats: png and jpeg."),
      
      div(style = "display: inline-block;", 
          numericInput("img_width_px", label = "Width (pixel)", value = csite$ui_attr$img_width_px, width = "100px")
      ),
      
      div(style = "display: inline-block;", 
          numericInput("img_height_px", label = "Height (pixel)", 
                       value = csite$ui_attr$img_height_px, width = "100px") 
      ),
      
      # Disabled because it does not work, yet.
      shinyjs::disabled(div(style = "display: inline-block;", 
          checkboxInput("img_asp_px", label = "Keep Aspect Ratio", value = FALSE)
      )),
      
      
      ## Wide Image Resolution Settings ########################################
      
      div(style = "margin-top:30px, margin-bottom:10px", 
          p("Resolution for wide plots, such as Well Report and Plume Diagnostic.")
      ),

      div(style = "display: inline-block;", 
          numericInput("img_width_px_wide", label = "Width (pixel)", value = csite$ui_attr$img_width_px_wide, width = "100px")
      ),
      
      div(style = "display: inline-block;", 
          numericInput("img_height_px_wide", label = "Height (pixel)", value = csite$ui_attr$img_height_px_wide, width = "100px") 
      ),
      
      
      ## DPI for PDF and Postscript files ######################################
      # [MIGHT GO HERE BUT NOT MANY USERS WILL REQUIRE THIS.]
      #
      #div(style = "margin-top:30px, margin-bottom:10px", 
      #    p("Dots per inch for PDF and Postscript files.")
      #),
      #
      #div(style = "display: inline-block;", 
      #    numericInput("img_width_px_wide", label = "Width (pixel)", value = csite$ui_attr$img_width_px_wide, width = "100px")
      #),
      
      # Disabled because it does not work, yet.
      shinyjs::disabled(div(style = "display: inline-block;", 
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
      
      ## Model Settings ########################################################
      div(style = "margin-top:50px", 
          h4("Model Settings")),
      p("Select the resolution for the PSpline model fit. The resolution is defined by the number of segments, which can also be directly defined in the right input field below (min:2, max: 12)."),
      
      div(style = "display:inline-block; margin-right: 10px",
          selectInput("psplines_resolution", "PSplines Resolution", choices = c("Default", "High"),
                    selected = "Default", width = '150px', selectize = FALSE)),
      div(style = "display:inline-block; margin-right: 10px", "or"),
      div(style = "display:inline-block", 
          textInput("psplines_knots", "Number of knots", value = csite$GWSDAT_Options[['PSplineVars']][['nseg']], width = '150px')
      )
        
    )
  )
  
}

