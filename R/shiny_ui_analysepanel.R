


shiny_ui_analysepanel <- function() {

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
                                
                                
                                #h4("Status:"),
                                #textOutput("status")
                                
                              )
                       ),
                       
                       column(7,
                              plotOutput("time_series"),
                              downloadButton("download_timeseries_plot", label = "Save Plot")
                              
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
                                selectInput("solute_select_contour", label = "Solute", choices = names(pnl$Fitted.Data),
                                            selected = pnl$Cont.rg, width = '80%'),
                                
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
                       
                       column(7, 
                              plotOutput("image_plot")
                       ),
                       
                       column(2, 
                              sliderInput("time_steps", "Time Step",
                                          min = pnl$timestep_range[1], 
                                          max = pnl$timestep_range[2], 
                                          value = pnl$timestep, 
                                          step = 1,
                                          #pre = "$", sep = ",", 
                                          animate = TRUE),
                              selectInput("aggregate_data", label = "Aggregate Data", 
                                          choices = c("All Dates", "Monthly", "Quarterly"),
                                          selected = pnl$GWSDAT_Options$Aggby, 
                                          width = "100%"),
                              
                              downloadButton("download_contour_plot", label = "Save Plot"),
                              
                              hidden( p(id = "generate_ppt_anim", 
                                    actionButton("generate_spatial_anim_ppt", 
                                                 label = "Generate PPT Animation", icon = icon("file-powerpoint-o"))    
                                      )
                              )
                              
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
                       
                       column(7,
                              plotOutput("traffic_table"),
                              plotOutput("plot_legend_traffic")
                       ),
                       
                       column(2,
                              sliderInput("time_steps_traffic", "Time Step",
                                          min = pnl$timestep_range[1], 
                                          max = pnl$timestep_range[2], 
                                          value = pnl$timestep, 
                                          step = 1,
                                          animate = TRUE),
                              selectInput("aggregate_data_traffic", label = "Aggregate Data", 
                                          choices = c("All Dates", "Monthly", "Quarterly"),
                                          selected = pnl$GWSDAT_Options$Aggby, 
                                          width = "100%"),
                              downloadButton("download_traffictable", label = "Save Plot")
                       )
                       
              ),
              #, # end tabPanel
             
              navbarMenu("Options",
                  "test 1",
                  "test 2",
                  "----",
                  tabPanel("Solutes & Thresholds", fluid = TRUE,
                           h3("Page for some settings."),
                           "Here we could define sub sets of solutes, edit thresholds, etc.."
                  )
              ) # end navbarMenu
              
  ) # end TabPanel
}


