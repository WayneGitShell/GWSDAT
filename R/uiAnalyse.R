


uiAnalyse <- function(csite) {

 
  navbarPage(title = csite$ui_attr$site_name, id = "analyse_panel",              
              
              tabPanel("Time-Series", id = "ts_tab", fluid = TRUE,
                       
                       box(width = 3, status = "warning", title = "Settings",
                       #column(3,
                       #        wellPanel(
                                #
                                # Did not decided yet where to put this (here or data manager).
                                #
                                #div(id = "select_aquifer_timeseries", 
                                #    selectInput("aquifer_timeseries", label = "Aquifer Group", choices = csite$All.Data$Aq_list,
                                #                selected = csite$All.Data$Aq.sel, width = '80%') ),
                                
                                selectInput("well_select", label = "Select Monitoring Well", choices = csite$ui_attr$sample_loc_names,
                                            selected = csite$ui_attr$sample_loc_selected, width = "80%"),
                                
                                selectInput("solute_select", label = "Solute", choices = csite$ui_attr$substance_names,
                                            selected = csite$ui_attr$substance_selected, width = '80%'),
                                
                                radioButtons("solute_conc", label = "Solute Conc. Unit",
                                             choices = csite$ui_attr$conc_unit_list, 
                                             selected = csite$ui_attr$conc_unit_selected),
                                
                                checkboxInput("check_threshold", label = "Display threshold", value = FALSE ),
                                
                                checkboxGroupInput("ts_true_options", label = "Time Series Plot Options", 
                                                   choices = names(csite$ui_attr$ts_options),
                                                   selected = names(which(csite$ui_attr$ts_options == TRUE)))
                                
                               
                       #       )
                       ),
                       
                       box(width = 7, status = "primary",
                           withSpinner(plotOutput("time_series")),
                           
                           div(style = "display: inline-block;",
                               selectInput("export_format_ts", label = "Image format", 
                                           choices = csite$ui_attr$img_formats, 
                                           selected = csite$ui_attr$img_formats[[1]]
                               )
                           ),
                           
                           div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
                               downloadButton("save_timeseries_plot", label = "Save Plot")
                           )
                            
                              
                       )
                       
                       
              ),
              
              
              
              
              tabPanel("Spatial Plot", id = "contour_tab", fluid = TRUE,
                       
                       box(width = 3, status = "warning", title = "Settings",
                                #
                                # Did not decided yet where to put this (here or data manager).
                                #
                                #div(id = "select_aquifer_contour", 
                                #    selectInput("aquifer_contour", label = "Aquifer Group", choices = csite$All.Data$Aq_list,
                                #                                       selected = csite$All.Data$Aq.sel, width = '80%') ),
                                selectInput("solute_select_contour", label = "Substance", choices = csite$ui_attr$substance_names,
                                            selected = csite$ui_attr$substance_selected, width = '80%'),
                                
                                radioButtons("solute_conc_contour", label = "Solute Conc. Unit",
                                             choices  = csite$ui_attr$conc_unit_list, 
                                             selected = csite$ui_attr$conc_unit_selected),
                                
                                selectInput("imageplot_type", label = "Plot Type", choices = csite$ui_attr$contour_types,
                                            selected = csite$ui_attr$contour_selected, width = "80%"),
                                
                                
                                checkboxGroupInput("imageplot_options", label = "Plot Options", 
                                                   choices = names(csite$ui_attr$spatial_options),
                                                   selected = names(which(csite$ui_attr$spatial_options == TRUE))),
                                
                                radioButtons("gw_flows", label = "Groundwater Flows",
                                             choices  = csite$ui_attr$gw_options, 
                                             selected = csite$ui_attr$gw_selected)
                                
                                
                              
                       ),
                       
                       
                       box(width = 7, status = "primary",
                            withSpinner(plotOutput("image_plot")),
                           
                            div(style = "display: inline-block;", 
                                selectInput("export_format_sp", label = "Image format", 
                                            choices  = csite$ui_attr$img_formats, 
                                            selected = csite$ui_attr$img_formats[[1]]
                                )
                            ),
                           
                            div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
                                downloadButton("save_spatial_plot", label = "Save Plot")
                            ),
                           
                           
                            hidden( 
                              div(id = "save_spatial_ppt_anim", style = "display: inline-block; vertical-align:top; margin-top: 25px;",
                                  
                                     actionButton("generate_spatial_anim_ppt", 
                                                  label = "Generate PPT Animation", icon = icon("file-movie-o"))
                              )
                              
                            )
                            
                       ),
                       
                       column(2, 
                              sliderInput("time_steps", "Time Step",
                                          min = csite$ui_attr$timestep_range[1], 
                                          max = csite$ui_attr$timestep_range[2], 
                                          value = csite$ui_attr$spatial_timestep_selected, 
                                          step = 1,
                                          animate = animationOptions(loop = TRUE)),
                              selectInput("aggregate_data", label = "Aggregate Data", 
                                          choices  = csite$ui_attr$aggregate_list,
                                          selected = csite$ui_attr$aggregate_selected , 
                                          width = "100%")
                       )
              ), # end tabPanel
              
              
              
              tabPanel("Trends & Thresholds", fluid = TRUE,
                       
                       box(width = 3, status = "warning", title = "Settings",
                                #
                                # Did not decided yet where to put this (here or data manager).
                                #
                                #div(id = "select_aquifer_traffic", 
                                #    selectInput("aquifer_traffic", label = "Aquifer Group", choices = csite$All.Data$Aq_list,
                                #                selected = csite$All.Data$Aq.sel, width = '80%') ),
                                
                                radioButtons("trend_or_threshold", label = "Display Table",
                                             choices  = csite$ui_attr$trend_thresh_list, 
                                             selected = csite$ui_attr$trend_thresh_selected),
                                
                                selectInput("traffic_color", label = "Show color", choices = csite$ui_attr$trend_color_list,
                                            selected = csite$ui_attr$trend_color_selected, width = "80%")
                       ),
                       
                       
                       tabBox("Trend Table", width = 7, 
                              
                        tabPanel(title = "Trends", 
                                 
                                 withSpinner(plotOutput("traffic_table")),

                                div(style = "display: inline-block;",
                                  selectInput("export_format_tt", label = "Image format",
                                              choices  = csite$ui_attr$img_formats,
                                              selected = csite$ui_attr$img_formats[[1]]
                                              )
                                ),

                                div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px",
                                  downloadButton("save_trend_table", label = "Save Plot")
                                ),

                                hidden(
                                  div(id = "save_trendtable_ppt_anim", style = "display: inline-block; vertical-align:top; margin-top: 25px;",
                                    actionButton("generate_trendtable_anim_ppt",
                                                 label = "Generate PPT Animation", icon = icon("file-movie-o"))
                                  )
                                )
                        ),
                        
                        tabPanel(title = "Legend", 
                                 
                                 plotOutput("plot_legend_traffic") 
                                 
                                 )
                       ),
                       
                       column(2,
                              sliderInput("time_steps_traffic", "Time Step",
                                          min   = csite$ui_attr$timestep_range[1], 
                                          max   = csite$ui_attr$timestep_range[2], 
                                          value = csite$ui_attr$trend_timestep_selected, 
                                          step = 1,
                                          animate = animationOptions(loop = TRUE)),
                              selectInput("aggregate_data_traffic", label = "Aggregate Data", 
                                          choices  = csite$ui_attr$aggregate_list,
                                          selected = csite$ui_attr$aggregate_selected, 
                                          width = "100%")
                       ) 
                       
              ),
              #, # end tabPanel
             
              navbarMenu("More",
                  tabPanel("Well Report", fluid = TRUE, 
                           uiWellReport(csite) ),
                  tabPanel("Plume Time Series", fluid = TRUE, 
                           uiPlumeDiagnostics(csite) ),
                  "----",
                  tabPanel("Options", fluid = TRUE,
                           uiAnalyseOptions(csite)
                  )
              ) # end navbarMenu
              
  ) # end TabPanel
}

