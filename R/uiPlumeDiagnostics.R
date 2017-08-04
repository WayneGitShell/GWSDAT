


uiPlumeDiagnostics <- function() {
  
  fluidRow(
    column(3,
           wellPanel(
    #box(width = 3, title = "Setting", 
        solidHeader = T,
        selectInput("solute_select_plume", label = "Substance", 
                    choices = names(pnl$Fitted.Data),
                    selected = names(pnl$Fitted.Data)[1], width = '80%'),

                "Pick a unit: (include Input)"

    )), # end box
    
    box(width = 8, title = "Plume Diagnostic", 
        plotOutput("plume_diagn_plot")  
        
        #div(style = "display: inline-block;",
        #    selectInput("export_format_wr", label = "Image format", 
        #                choices = pnl$image_formats, 
        #                selected = pnl$image_formats[[1]]
        #    )
        #),
        
        #div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
        #    downloadButton("save_wellreport_plot", label = "Save Plot")
        #)
    )
  )
}