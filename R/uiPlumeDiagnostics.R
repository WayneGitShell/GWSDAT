


uiPlumeDiagnostics <- function(csite, img_frmt) {
  
  fluidRow(
    shinydashboard::box(width = 3, status = "warning", title = "Settings",
            selectInput("solute_select_pd", label = "Substance", 
                        choices  = csite$ui_attr$solute_names,
                        selected = csite$ui_attr$solute_select_pd, width = '100%'),

            numericInput("plume_thresh_pd", label = "Plume Threshold", 
                         value = csite$ui_attr$plume_thresh_pd),
            
            sliderInput("ground_porosity_pd", "Ground Porosity (%)",
                        min = 0, 
                        max = 100, 
                        value = csite$ui_attr$ground_porosity_pd,
                        width = '150px'
            )#,
            #div(style = "float: right",
            #  actionButton("update_plume_ts", label = "Update", icon = icon("cogs"))
            #)
                         
    ), # end box
    
    shinydashboard::tabBox(title = "Plume Diagnostic", width = 9, id = "plume_tab_box",
      
      tabPanel("Mass, Area & Concentration",
                             div(id = "plume_diagn_plot_div", withSpinner(plotOutput("plume_diagn_plot"))),
                             shinyjs::hidden(div(id = "plume_diagn_msg_div", htmlOutput("plume_diagn_msg"))),
               
                             div(id = "plume_save_btn_div",
                              div(style = "display: inline-block;",
                                 selectInput("export_format_pd", label = "Image format", 
                                             choices = img_frmt, 
                                             selected = img_frmt[[1]])
                              ),
                              div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
                                             downloadButton("save_plumestats_plot", label = "Save Plot")
                              ),
                              div(style = "display: inline-block; vertical-align:top; margin-top: 25px", 
                                             downloadButton("save_plumestats_csv", label = "Save as .CSV")
                              )
                             )
      ),
      tabPanel("Boundary Estimate", value = "plume_pnl_2",
                              withSpinner(plotOutput("plume_estimate_plot"))
      )
    )
    
  )
}
