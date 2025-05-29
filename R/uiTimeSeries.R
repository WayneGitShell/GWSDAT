
uiTimeSeries <- function(csite, img_frmt) {
    
  fluidRow(
    shinydashboard::box(width = 2, status = "warning", title = "Settings",
                        # Modify the UI to allow multiple selections
                        selectInput("sample_loc_select_ts", label = "Select Monitoring Well", choices = csite$ui_attr$sample_loc_names,
                                    selected = csite$ui_attr$sample_loc_select_ts, width = "80%", multiple = TRUE),
                        # Modify the UI to allow multiple selections
                        selectInput("solute_select_ts", label = "Substance", choices = csite$ui_attr$solute_names,
                                    selected = csite$ui_attr$solute_select_ts, width = '80%', multiple = TRUE),
                        
                        radioButtons("solute_conc", label = "Solute Conc. Unit",
                                     choices = csite$ui_attr$conc_unit_list, 
                                     selected = csite$ui_attr$conc_unit_selected),
                        
                        checkboxInput("check_threshold", label = "Display threshold", 
                                      value = csite$ui_attr$show_thresh_ts ),
                        
                        checkboxGroupInput("ts_true_options", label = "Time Series Plot Options", 
                                           choices = names(csite$ui_attr$ts_options),
                                           selected = names(which(csite$ui_attr$ts_options == TRUE)))
                        
    ),
    
    shinydashboard::box(width = 10, status = "primary",
                        div(   ### Increasing the size of the outputwindow in plottimeseries
                          plotOutput("time_series",
                                     height = 700)%>% withSpinner(color="#0dc5c1"),      ### Alignment should be done to properly distribute the unused space
                        
                        div(style = "display: inline-block;",
                            selectInput("export_format_ts", label = "Image format", 
                                        choices = img_frmt[-which(img_frmt == "tif")], #csite$ui_attr$img_formats, 
                                        selected = img_frmt[[1]] #csite$ui_attr$img_formats[[1]]
                            )
                        ),
                        
                        div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
                            downloadButton("save_timeseries_plot", label = "Save Plot")
                        ),
                        ),
                        if (existsPPT()) {
                          div(id = "save_timeseries_ppt_anim", style = "display: inline-block; vertical-align:top; margin-top: 25px;",
                              
                
                              actionButton("Optionsgenerate_timeseries_anim_ppt","Generate Well Report")
                          ) }
                        
    ),
    absolutePanel(id = "timecontrol_ts", class = "panel panel-default", 
                  fixed = TRUE, draggable = TRUE, top = "auto", 
                  left = "auto", right = 20, bottom = 20,
                  width = 350, height = 140,  
                  
                  div(style = "margin-left: 15px; margin-top: 5px",
                      h4(textOutput("timepoint_ts_idx_label")),
                      sliderInput("timepoint_ts_idx",
                                  label="",
                                  #label = paste0("Time: ", pasteAggLimit(csite$ui_attr$timepoints[csite$ui_attr$timepoint_sp_idx], csite$GWSDAT_Options$Aggby)),
                                  #min = 1,
                                  #min = min(as.Date(csite$ui_attr$timepoints, "%d-%m-%Y")),
                                  #max = max(as.Date(csite$ui_attr$timepoints, "%d-%m-%Y")),
                                  min = min(c(csite$All.Data$Cont.Data$SampleDate, csite$All.Data$GW.Data$SampleDate),na.rm = T),
                                  max = max(c(csite$All.Data$Cont.Data$SampleDate, csite$All.Data$GW.Data$SampleDate),na.rm = T),
                                  step = 1,
                                  #value = c(1,csite$ui_attr$timepoint_sp_idx),
                                  #value = length(csite$ui_attr$timepoints),
                                  #value = c(min(as.Date(csite$ui_attr$timepoints, "%d-%m-%Y")),max(as.Date(csite$ui_attr$timepoints, "%d-%m-%Y")))
                                  value=range(c(csite$All.Data$Cont.Data$SampleDate, csite$All.Data$GW.Data$SampleDate),na.rm = T)
                                  #,animate = animationOptions(loop = TRUE, interval = 1500)
                      ) # ,
                  )
    )
  )

}
