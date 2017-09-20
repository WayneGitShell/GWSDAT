

uiSTPredictions <- function(csite) {
  
  fluidRow(
    shinydashboard::box(width = 3, title = "Settings", 
                        status = "warning", 
                        selectInput("solute_select_stp", label = "Solute", choices = csite$ui_attr$substance_names,
                                    selected = csite$ui_attr$substance_selected, width = '80%'),
                        div(style = "margin-bottom:15px; margin-top:10px",
                            "Exclude wells by selecting and pressing the Delete key. Use Ctrl and Shift to select multiple elements."),
                        selectInput("well_mult_select_stp", 'Wells', choices = csite$ui_attr$sample_loc_names,
                                    selected = csite$ui_attr$sample_loc_names, multiple = TRUE, selectize = TRUE),
                        radioButtons("logscale_stp", label = "Use Log-Scale",
                                     choices = list("Yes", "No"), 
                                     selected = "Yes"),
                        radioButtons("solute_conc_stp", label = "Solute Conc. Unit",
                                     choices = csite$ui_attr$conc_unit_list, 
                                     selected = csite$ui_attr$conc_unit_selected)
    ), # end box
    
    shinydashboard::box(width = 9, status = "primary", 
                        plotOutput("stpredictions_plot"),  
                        
                        div(style = "display: inline-block;",
                            selectInput("export_format_stp", label = "Image format", 
                                        choices = csite$ui_attr$img_formats, 
                                        selected = csite$ui_attr$img_formats[[1]]
                            )
                        ),
                        
                        div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
                            downloadButton("save_stpredictions_plot", label = "Save Plot")
                        )
    )
  )
}
