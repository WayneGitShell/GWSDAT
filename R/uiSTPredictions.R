

uiSTPredictions <- function(csite, img_frmt) {
  
  fluidRow(
    shinydashboard::box(width = 3, title = "Settings", 
                        status = "warning", 
                        selectInput("solute_select_stp", label = "Solute", choices = csite$ui_attr$solute_names,
                                    selected = csite$ui_attr$solute_select_stp, width = '80%'),
                        
                        div(style = "margin-bottom:15px; margin-top:10px",
                            "Exclude wells by selecting and pressing the Delete key. Use Ctrl and Shift to select multiple elements."),
                        
                        selectInput("sample_loc_select_stp", 'Wells', choices = csite$ui_attr$sample_loc_names,
                                    selected = csite$ui_attr$sample_loc_select_stp, multiple = TRUE, selectize = TRUE),
                        
                        radioButtons("logscale_stp", label = "Use Log-Scale",
                                     choices = list("Yes", "No"), 
                                     selected = csite$ui_attr$logscale_stp),
                        
                        radioButtons("solute_conc_stp", label = "Solute Conc. Unit",
                                     choices = csite$ui_attr$conc_unit_list, 
                                     selected = csite$ui_attr$solute_conc_stp)
    ), # end box
    
    shinydashboard::box(width = 9, status = "primary", 
                        withSpinner(plotOutput("stpredictions_plot", height = 600)),  
                        
                        div(style = "display: inline-block;",
                            selectInput("export_format_stp", label = "Image format", 
                                        choices = img_frmt, 
                                        selected = img_frmt[[1]]
                            )
                        ),
                        
                        div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
                            downloadButton("save_stpredictions_plot", label = "Save Plot")
                        )
    )
  )
}
