


uiWellReport <- function(csite) {
  
  fluidRow(
    box(width = 3, title = "Setting", 
        solidHeader = T,
      "Exclude solutes by moving them to the right.",
      chooserInput("solute_chooser", 
                 leftLabel = "Include in Report", 
                 rightLabel = "Exclude from Report", 
                 leftChoices = csite$ui_attr$substance_names, 
                 rightChoices = c(), 
                 size = min(10, length(csite$ui_attr$substance_names) + 3), multiple = TRUE 
                ),
    
      "Exclude wells by moving them to the right.",
      chooserInput("well_chooser", 
                 leftLabel = "Include in Report", 
                 rightLabel = "Exclude from Report", 
                 leftChoices = csite$ui_attr$sample_loc_names, 
                 rightChoices = c(), 
                 size = min(15, length(csite$ui_attr$sample_loc_names) + 3), multiple = TRUE 
                ),
      
      radioButtons("well_report_logscale", label = "Use Log-Scale",
                 choices = list("Yes", "No"), 
                 selected = "Yes")
    
      # Took this one out because report is generated on the fly.
      #actionButton("actionBtn_wellreport", label = "Generate Report", 
      #             icon = icon("line-chart"), 
      #             style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    
    ), # end box

    box(width = 9, title = "Well Report", # height = 600 
          plotOutput("well_report_plot"),  # , width = "120%")
          
          div(style = "display: inline-block;",
              selectInput("export_format_wr", label = "Image format", 
                          choices = csite$ui_attr$img_formats, 
                          selected = csite$ui_attr$img_formats[[1]]
              )
          ),
          
          div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
              downloadButton("save_wellreport_plot", label = "Save Plot")
          )
    )
  )
}