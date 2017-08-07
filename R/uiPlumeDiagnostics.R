


uiPlumeDiagnostics <- function() {
  
  fluidRow(
    column(2,
           wellPanel(
    #box(width = 3, title = "Setting", 
        solidHeader = T,
        selectInput("solute_select_plume", label = "Substance", 
                    choices = names(pnl$Fitted.Data),
                    selected = names(pnl$Fitted.Data)[1], width = '100%'),

                "Pick a unit: (include Input)"

    )), # end box
    
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