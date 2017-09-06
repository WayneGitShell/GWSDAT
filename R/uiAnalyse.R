

#' @importFrom shinyjs hidden
# #' @importFrom shinydashboard box 
uiAnalyse <- function(csite) {
  
  #
  # This code is from the Superzip Shiny Gallery App.
  #  It is supposed to change the CSS style of a panel (the dragable time control).
  #  Unfortunately, it does not work here.. maybe move it somewhere else or load
  #  from .css file.
  #
  #   shiny::tags$head(
  #     shiny::tags$style(HTML("
  #                             
  #                            #timecontrol_sp {
  #                            /* Appearance */
  #                            background-color: white;
  #                            padding: 0 20px 20px 20px;
  #                            cursor: move;
  #                            /* Fade out while not hovering */
  #                            opacity: 0.65;
  #                            zoom: 0.9;
  #                            transition: opacity 500ms 1s;
  # }
  # 
  # #timecontrol_sp {
  # /* Fade in while hovering */
  # opacity: 0.95;
  # transition-delay: 0;
  # }"
  #   )))
  #   

 #corner_element = HTML(paste0(tags$a(id = "GoToDataSelect", "<- Back", href = "#"), " ", csite$ui_attr$site_name)) 
  corner_element <- HTML(paste0(actionButton("GoToDataSelect", "", icon = icon("arrow-left"), style = "height: 30px"), "&nbsp;&nbsp;&nbsp", csite$ui_attr$site_name)) # tags$a(id = "GoToDataSelect", "<- Back", href = "#"), " ", csite$ui_attr$site_name)) 
  navbarPage(corner_element, windowTitle = csite$ui_attr$site_name, id = "analyse_panel",              
              
              tabPanel("Time-Series", id = "ts_tab", fluid = TRUE,
                       
                       shinydashboard::box(width = 3, status = "warning", title = "Settings",
                       
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
                                
                       ),
                       
                       shinydashboard::box(width = 9, status = "primary",
                           plotOutput("time_series"),
                           
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
                       
                       shinydashboard::box(width = 3, status = "warning", title = "Settings",
                               
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
                       
                       
                       shinydashboard::box(width = 9, status = "primary",
                            plotOutput("image_plot"),
                           
                            div(style = "display: inline-block;", 
                                selectInput("export_format_sp", label = "Image format", 
                                            choices  = csite$ui_attr$img_formats, 
                                            selected = csite$ui_attr$img_formats[[1]]
                                )
                            ),
                           
                            div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
                                downloadButton("save_spatial_plot", label = "Save Plot")
                            ),
                           
                           
                            shinyjs::hidden( 
                              div(id = "save_spatial_ppt_anim", style = "display: inline-block; vertical-align:top; margin-top: 25px;",
                                  
                                     actionButton("generate_spatial_anim_ppt", 
                                                  label = "Generate PPT Animation", icon = icon("file-movie-o"))
                              )
                              
                            )
                            
                       ),
                       absolutePanel(id = "timecontrol_sp", class = "panel panel-default", 
                                     fixed = TRUE, draggable = TRUE, top = "auto", 
                                     left = "auto", right = 20, bottom = 20,
                                     width = 330, height = 110,
                       
                              div(style = "margin-left: 15px; margin-top: 5px",
                              sliderValues(
                                inputId = "timepoint_sp", label = "Time Point", width = "95%",
                                values = csite$ui_attr$timepoints, 
                                from = csite$ui_attr$timepoint_sp,
                                #to = csite$ui_attr$choices_month[6],
                                grid = TRUE, animate = animationOptions(interval = 1500, loop = TRUE)
                              ))
                              #sliderInput("timepoint_sp", "Time Point",
                              #            min = csite$ui_attr$timepoint_range[1],
                              #            max = csite$ui_attr$timepoint_range[2],
                              #            value = csite$ui_attr$timepoint_spatial_selected,
                              #            timeFormat = "%d-%m-%Y",
                              #            animate = animationOptions(loop = TRUE)
                              #            ) # ,
                              
                              # selectInput("aggregate_data", label = "Aggregate Data", 
                              #             choices  = csite$ui_attr$aggregate_list,
                              #             selected = csite$ui_attr$aggregate_selected , 
                              #             width = "100%")
                       )
              ), # end tabPanel
              
              
              
              tabPanel("Trends & Thresholds", fluid = TRUE,
                       
                       shinydashboard::box(width = 3, status = "warning", title = "Settings",
                                
                                radioButtons("trend_or_threshold", label = "Display Table",
                                             choices  = csite$ui_attr$trend_thresh_list, 
                                             selected = csite$ui_attr$trend_thresh_selected),
                                
                                selectInput("traffic_color", label = "Show color", choices = csite$ui_attr$trend_color_list,
                                            selected = csite$ui_attr$trend_color_selected, width = "80%")
                       ),
                       
                       
                       shinydashboard::tabBox(title = "Trends & Thresholds", width = 9, 
                              
                        tabPanel("Indicator Table", 
                                 
                                plotOutput("traffic_table"),

                                div(style = "display: inline-block;",
                                  selectInput("export_format_tt", label = "Image format",
                                              choices  = csite$ui_attr$img_formats,
                                              selected = csite$ui_attr$img_formats[[1]]
                                              )
                                ),

                                div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px",
                                  downloadButton("save_trend_table", label = "Save Plot")
                                ),

                                shinyjs::hidden(
                                  div(id = "save_trendtable_ppt_anim", style = "display: inline-block; vertical-align:top; margin-top: 25px;",
                                    actionButton("generate_trendtable_anim_ppt",
                                                 label = "Generate PPT Animation", icon = icon("file-movie-o"))
                                  )
                                )
                        ),
                        
                        tabPanel("Legend", 
                                 
                                 plotOutput("plot_legend_traffic") 
                                 
                                 )
                       ),
                       absolutePanel(id = "timecontrol_tt", class = "panel panel-default", 
                                     fixed = TRUE, draggable = TRUE, top = "auto", 
                                     left = "auto", right = 20, bottom = 20,
                                     width = 330, height = 110,
                                     
                                     div(style = "margin-left: 15px; margin-top: 5px",
                                         sliderValues(
                                           inputId = "timepoint_tt", label = "Time Point", width = "95%",
                                           values = csite$ui_attr$timepoints, 
                                           from = csite$ui_attr$timepoint_tt,
                                           #to = csite$ui_attr$choices_month[6],
                                           grid = TRUE, animate = animationOptions(interval = 1500, loop = TRUE)
                                         ))
                       )
                       # column(2,
                       #        sliderInput("timepoint_tt", "Time Point",
                       #                    min = csite$ui_attr$timepoint_range[1],
                       #                    max = csite$ui_attr$timepoint_range[2],
                       #                    value = csite$ui_attr$timepoint_spatial_selected,
                       #                    timeFormat = "%d-%m-%Y",
                       #                    animate = animationOptions(loop = TRUE)
                       #        )#,
                       #        
                       #        # selectInput("aggregate_data_traffic", label = "Aggregate Data", 
                       #        #             choices  = csite$ui_attr$aggregate_list,
                       #        #             selected = csite$ui_attr$aggregate_selected, 
                       #        #             width = "100%")
                       # ) 
                       
              ), # end tabPanel

              navbarMenu("More",
                  tabPanel("Well Report", fluid = TRUE, 
                           uiWellReport(csite) ),
                  tabPanel("Plume Diagnostic", fluid = TRUE, 
                           uiPlumeDiagnostics(csite) ),
                  "----",
                  tabPanel("Save Session", fluid = TRUE,
                           uiSession() ),  
                  tabPanel("Options", fluid = TRUE,
                           uiAnalyseOptions(csite)
                  )
              ) # end navbarMenu
              
  ) # end TabPanel
}

