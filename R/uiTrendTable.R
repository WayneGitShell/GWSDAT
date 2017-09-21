

uiTrendTable <- function(csite) {
    
  #tabPanel("Trends & Thresholds", fluid = TRUE,
         fluidRow(
         shinydashboard::box(width = 3, status = "warning", title = "Settings",
                             selectInput("aggregate_data_tt", label = "Aggregate by", 
                                         choices  = csite$ui_attr$aggregate_list,
                                         selected = csite$ui_attr$aggregate_selected, 
                                         width = "80%"),
                             
                             radioButtons("trend_or_threshold", label = "Display Table",
                                          choices  = csite$ui_attr$trend_thresh_list, 
                                          selected = csite$ui_attr$trend_thresh_selected),
                             
                             selectInput("traffic_color", label = "Show color", choices = csite$ui_attr$trend_color_list,
                                         selected = csite$ui_attr$trend_color_selected, width = "80%")
         ),
         
         
         shinydashboard::tabBox(title = "Trends & Thresholds", width = 9, 
                                
                                tabPanel("Indicator Table", 
                                         
                                         htmlOutput("trend_table")  #,
                                         
                                         # div(style = "display: inline-block;",
                                         #     selectInput("export_format_tt", label = "Image format",
                                         #                 choices  = csite$ui_attr$img_formats,
                                         #                 selected = csite$ui_attr$img_formats[[1]]
                                         #     )
                                         # ),
                                         # 
                                         # div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px",
                                         #     downloadButton("save_trend_table", label = "Save Plot")
                                         # ),
                                         # 
                                         # shinyjs::hidden(
                                         #   div(id = "save_trendtable_ppt_anim", style = "display: inline-block; vertical-align:top; margin-top: 25px;",
                                         #       actionButton("generate_trendtable_anim_ppt",
                                         #                    label = "Generate PPT Animation", icon = icon("file-movie-o"))
                                         #   )
                                         # )
                                ),
                                
                                tabPanel("Legend", 
                                         
                                        # plotOutput("plot_legend_traffic") 
                                        htmlOutput("trend_legend")
                                        
                                         
                                )
         ),
         absolutePanel(id = "timecontrol_tt", class = "panel panel-default", 
                       fixed = TRUE, draggable = TRUE, top = "auto", 
                       left = "auto", right = 20, bottom = 20,
                       width = 350, height = 110,
                       
                       div(style = "margin-left: 15px; margin-top: 5px",
                           sliderInput("timepoint_tt_idx", 
                                       label = paste0("Time: ", pasteAggLimit(csite$ui_attr$timepoints[csite$ui_attr$timepoint_sp_idx], csite$GWSDAT_Options$Aggby)),
                                       min = 1,
                                       max = length(csite$ui_attr$timepoints),
                                       step = 1,
                                       value = csite$ui_attr$timepoint_tt_idx,
                                       animate = animationOptions(loop = TRUE, interval = 1500)
                           ) 
                       )
                       # sliderValues(
                       #   inputId = "timepoint_tt", label = "Time Point", width = "95%",
                       #   values = csite$ui_attr$timepoints, 
                       #   from = csite$ui_attr$timepoint_tt,
                       #   #to = csite$ui_attr$choices_month[6],
                       #   grid = if (length(csite$ui_attr$timepoints) < 20) {TRUE} else {FALSE},
                       #   animate = animationOptions(interval = 1500, loop = TRUE)
                       # ))
         )
         )
}
         
#), # end tabPanel
