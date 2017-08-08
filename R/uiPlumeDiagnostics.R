


uiPlumeDiagnostics <- function() {
  
  fluidRow(
    column(2,
           wellPanel(solidHeader = T,
            selectInput("solute_select_plume_pd", label = "Substance", 
                        choices  = names(pnl$Fitted.Data),
                        selected = names(pnl$Fitted.Data)[1], width = '100%'),

            numericInput("plume_threshold_pd", label = "Plume Threshold", value = pnl$GWSDAT_Options$DefPlumeThresh),
            
            sliderInput("ground_porosity_pd", "Ground Porosity (%)",
                        min = 0, 
                        max = 1, 
                        value = pnl$Porosity,
                        width = '150px'
            ),
            
            actionButton("update_plume_ts",
                         label = "Update", icon = icon("cogs") 
                         )
            
           )
      ), # end column
    
    box(width = 10, title = "Plume Diagnostic", 
        div(id = "plume_diagn_plot_div",
          plotOutput("plume_diagn_plot"),

          div(style = "display: inline-block;",
              selectInput("export_format_pd", label = "Image format", 
                          choices = pnl$image_formats, 
                          selected = pnl$image_formats[[1]]
              )
          ),
          
          div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
              downloadButton("save_plumestats_plot", label = "Save Plot")
          ),
          
          div(style = "display: inline-block; vertical-align:top; margin-top: 25px", 
              downloadButton("save_plumestats_csv", label = "Save as .CSV")
          )
          
        ),
        
        hidden(
          div(id = "plume_diagn_msg_div",
              textOutput("plume_diagn_msg")
              )
        )

    )
  )
}