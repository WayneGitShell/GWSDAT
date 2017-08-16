


uiPlumeDiagnostics <- function(csite) {
  
  fluidRow(
    column(2,
           wellPanel(solidHeader = T,
            selectInput("solute_select_plume_pd", label = "Substance", 
                        choices  = csite$ui_attr$substance_names,
                        selected = csite$ui_attr$substance_selected, width = '100%'),

            numericInput("plume_threshold_pd", label = "Plume Threshold", 
                         value = csite$ui_attr$plume_thresh[1]),
            
            sliderInput("ground_porosity_pd", "Ground Porosity (%)",
                        min = 0, 
                        max = 1, 
                        value = csite$ui_attr$ground_porosity,
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
                          choices = csite$ui_attr$img_formats, 
                          selected = csite$ui_attr$img_formats[[1]]
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