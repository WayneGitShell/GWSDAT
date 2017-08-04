


uiAnalyse <- function() {

  #tabsetPanel(id = "plot_tabs",
  navbarPage(title = pnl$GWSDAT_Options$SiteName, id = "analyse_panel",              
              
              tabPanel("Time-Series", id = "ts_tab", fluid = TRUE,
                       
                       column(3,
                              wellPanel(
                                #
                                # Did not decided yet where to put this (here or data manager).
                                #
                                #div(id = "select_aquifer_timeseries", 
                                #    selectInput("aquifer_timeseries", label = "Aquifer Group", choices = pnl$All.Data$Aq_list,
                                #                selected = pnl$All.Data$Aq.sel, width = '80%') ),
                                
                                selectInput("well_select", label = "Select Monitoring Well", choices = sort(as.character(pnl$All.Data$All.Wells)),
                                            selected = pnl$Well, width = "80%"),
                                
                                selectInput("solute_select", label = "Solute", choices = names(pnl$Fitted.Data),
                                            selected = pnl$Cont.rg, width = '80%'),
                                
                                radioButtons("solute_conc", label = "Solute Conc. Unit",
                                             choices = pnl$rgUnits_choice, 
                                             selected = pnl$rgUnits),
                                
                                checkboxInput("check_threshold", label = "Display threshold", value = FALSE ),
                                
                                checkboxGroupInput("ts_true_options", label = "Time Series Plot Options", 
                                                   choices = names(pnl$dlines),
                                                   selected = names(which(pnl$dlines == TRUE)))
                                
                               
                              )
                       ),
                       
                       box(width = 7,
                           plotOutput("time_series"),
                           
                           div(style = "display: inline-block;",
                               selectInput("export_format_ts", label = "Image format", 
                                           choices = pnl$image_formats, 
                                           selected = pnl$image_formats[[1]]
                               )
                           ),
                           
                           div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
                               downloadButton("save_timeseries_plot", label = "Save Plot")
                           )
                            
                              
                       )
                       
                       
              ),
              
              
              
              
              tabPanel("Spatial Plot", id = "contour_tab", fluid = TRUE,
                       
                       column(3, 
                              
                              wellPanel(
                                #
                                # Did not decided yet where to put this (here or data manager).
                                #
                                #div(id = "select_aquifer_contour", 
                                #    selectInput("aquifer_contour", label = "Aquifer Group", choices = pnl$All.Data$Aq_list,
                                #                                       selected = pnl$All.Data$Aq.sel, width = '80%') ),
                                selectInput("solute_select_contour", label = "Substance", choices = names(pnl$Fitted.Data),
                                            selected = names(pnl$Fitted.Data)[1], width = '80%'),
                                
                                radioButtons("solute_conc_contour", label = "Solute Conc. Unit",
                                             choices = pnl$rgUnits_choice, 
                                             selected = pnl$rgUnits),
                                
                                selectInput("imageplot_type", label = "Plot Type", choices = pnl$Color.type_choice,
                                            selected = pnl$Color.type, width = "80%"),
                                
                                
                                checkboxGroupInput("imageplot_options", label = "Plot Options", 
                                                   choices = names(pnl$ScaleCols),
                                                   selected = names(which(pnl$ScaleCols == TRUE))),
                                
                                radioButtons("gw_flows", label = "Groundwater Flows",
                                             choices = pnl$GW.disp_choice, 
                                             selected = pnl$GW.disp)
                                
                              )          
                              
                       ),
                       
                       
                       box(width = 7,
                            plotOutput("image_plot"),
                           
                            div(style = "display: inline-block;", 
                                selectInput("export_format_sp", label = "Image format", 
                                            choices = pnl$image_formats, 
                                            selected = pnl$image_formats[[1]]
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
                                          min = pnl$timestep_range[1], 
                                          max = pnl$timestep_range[2], 
                                          value = pnl$timestep_range[1], 
                                          step = 1,
                                          animate = animationOptions(loop = TRUE)),
                              selectInput("aggregate_data", label = "Aggregate Data", 
                                          choices = c("All Dates", "Monthly", "Quarterly"),
                                          selected = pnl$GWSDAT_Options$Aggby, 
                                          width = "100%")
                              
                       ) # end column
                       
              ), # end tabPanel
              
              
              
              tabPanel("Trends & Thresholds", fluid = TRUE,
                       
                       column(3,
                              
                              wellPanel(
                                #
                                # Did not decided yet where to put this (here or data manager).
                                #
                                #div(id = "select_aquifer_traffic", 
                                #    selectInput("aquifer_traffic", label = "Aquifer Group", choices = pnl$All.Data$Aq_list,
                                #                selected = pnl$All.Data$Aq.sel, width = '80%') ),
                                
                                radioButtons("trend_or_threshold", label = "Display Table",
                                             choices = pnl$rg1_choice, 
                                             selected = pnl$rg1),
                                
                                selectInput("traffic_color", label = "Show color", choices = pnl$ColTrafficListbox_choice,
                                            selected = pnl$ColTrafficListbox, width = "80%")
                                
                              )
                              
                       ),
                       
                       
                       tabBox("Trend Table", width = 7,
                              
                        tabPanel(title = "Trends", 
                                 
                                plotOutput("traffic_table"),

                                div(style = "display: inline-block;",
                                  selectInput("export_format_tt", label = "Image format",
                                              choices = pnl$image_formats,
                                              selected = pnl$image_formats[[1]]#,
                                              #width = "95%"   # <- this won't work
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
                                          min = pnl$timestep_range[1], 
                                          max = pnl$timestep_range[2], 
                                          value = pnl$timestep_range[1], 
                                          step = 1,
                                          animate = animationOptions(loop = TRUE)),
                              selectInput("aggregate_data_traffic", label = "Aggregate Data", 
                                          choices = c("All Dates", "Monthly", "Quarterly"),
                                          selected = pnl$GWSDAT_Options$Aggby, 
                                          width = "100%")
                       ) 
                       
              ),
              #, # end tabPanel
             
              navbarMenu("More",
                  tabPanel("Well Report", fluid = TRUE, 
                           uiWellReport() ),
                  tabPanel("Plume Time Series", fluid = TRUE, 
                           uiPlumeDiagnostics() ),
                  "----",
                  tabPanel("Options", fluid = TRUE,
                           uiAnalyseOptions()
                  )
              ) # end navbarMenu
              
  ) # end TabPanel
}

uiAnalyseOptions <- function() {
  
  box(title = "Options", width = 8, #solidHeader = TRUE,
    #hr(),  
    "The following options will affect the currently selected data and model. For 
    global settings visit the _Settings_ page.",
    
    
    h4("Plume Thresholds"),
    "Unit values are in ..",
    uiOutput("thres_plume_select"),
    
    numericInput("ground_porosity_input", 
                 label = "Ground Porosity (%)", 
                 value = pnl$Porosity, 
                 width = "100px"),
    
    actionButton("save_analyse_options",
                 label = "Save", icon = icon("save"), 
                 style = "color: #fff; background-color: Coral; border-color: Chocolate; float: right")
    
    
  )
  
}

