
uiTimeSeries <- function(csite, img_frmt) {
    
  fluidRow(
    shinydashboard::box(width = 3, status = "warning", title = "Settings",
                        
                        selectInput("sample_loc_select_ts", label = "Select Monitoring Well", choices = csite$ui_attr$sample_loc_names,
                                    selected = csite$ui_attr$sample_loc_select_ts, width = "80%"),
                        
                        selectInput("solute_select_ts", label = "Substance", choices = csite$ui_attr$solute_names,
                                    selected = csite$ui_attr$solute_select_ts, width = '80%'),
                        
                        radioButtons("solute_conc", label = "Solute Conc. Unit",
                                     choices = csite$ui_attr$conc_unit_list, 
                                     selected = csite$ui_attr$conc_unit_selected),
                        
                        checkboxInput("check_threshold", label = "Display threshold", 
                                      value = csite$ui_attr$show_thresh_ts ),
                        
                        checkboxGroupInput("ts_true_options", label = "Time Series Plot Options", 
                                           choices = names(csite$ui_attr$ts_options),
                                           selected = names(which(csite$ui_attr$ts_options == TRUE)))
                        
    ),
    
    shinydashboard::box(width = 9, status = "primary",
                        plotOutput("time_series"),
                        
                        div(style = "display: inline-block;",
                            selectInput("export_format_ts", label = "Image format", 
                                        choices = img_frmt[-which(img_frmt == "tif")], #csite$ui_attr$img_formats, 
                                        selected = img_frmt[[1]] #csite$ui_attr$img_formats[[1]]
                            )
                        ),
                        
                        div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
                            downloadButton("save_timeseries_plot", label = "Save Plot")
                        ),
                        if (existsPPT()) {
                          div(id = "save_timeseries_ppt_anim", style = "display: inline-block; vertical-align:top; margin-top: 25px;",
                              
                
                              actionButton("Optionsgenerate_timeseries_anim_ppt","Generate Well Report")
                          ) }
                        
    )
  )

}
