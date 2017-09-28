


uiWellReport <- function(csite, img_frmt) {
  
    fluidRow(
      shinydashboard::box(width = 3, title = "Settings", 
        status = "warning", 
        div(style = "margin-bottom:30px",
            "Exclude contaminants and wells by selecting and pressing the Delete key. Use Ctrl and Shift to select multiple elements."),
        
      selectInput("solute_mult_select", 'Contaminants', choices = csite$ui_attr$substance_names,
                  selected = csite$ui_attr$substance_names, multiple = TRUE, selectize = TRUE),
      selectInput("well_mult_select", 'Wells', choices = csite$ui_attr$sample_loc_names,
                   selected = csite$ui_attr$sample_loc_names, multiple = TRUE, selectize = TRUE),
            
      radioButtons("well_report_logscale", label = "Use Log-Scale",
                 choices = list("Yes", "No"), 
                 selected = "Yes")
    
    ), # end box

    shinydashboard::box(width = 9, status = "primary", 
          plotOutput("well_report_plot", height = 600),  
          
          div(style = "display: inline-block;",
              selectInput("export_format_wr", label = "Image format", 
                          choices = img_frmt, 
                          selected = img_frmt[[1]]
              )
          ),
          
          div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
              downloadButton("save_wellreport_plot", label = "Save Plot")
          )
    )
  )
}
