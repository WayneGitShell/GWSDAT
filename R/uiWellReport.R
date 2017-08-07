


uiWellReport <- function() {
  
  fluidRow(
    box(width = 3, title = "Setting", 
        solidHeader = T,
      "Exclude solutes by moving them to the right.",
      chooserInput("solute_chooser", 
                 leftLabel = "Include in Report", 
                 rightLabel = "Exclude from Report", 
                 leftChoices = names(pnl$Fitted.Data), 
                 rightChoices = c(), 
                 size = min(10, length(names(pnl$Fitted.Data)) + 3), multiple = TRUE 
                ),
    
      "Exclude wells by moving them to the right.",
      chooserInput("well_chooser", 
                 leftLabel = "Include in Report", 
                 rightLabel = "Exclude from Report", 
                 leftChoices = sort(as.character(pnl$All.Data$All.Wells)), 
                 rightChoices = c(), 
                 size = min(15, length(pnl$All.Data$All.Wells) + 3), multiple = TRUE 
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
                          choices = pnl$image_formats, 
                          selected = pnl$image_formats[[1]]
              )
          ),
          
          div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
              downloadButton("save_wellreport_plot", label = "Save Plot")
          )
    )
  )
}