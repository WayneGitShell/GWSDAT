

uiTrendTable <- function(csite) {
    
  
  fluidRow(
        shinydashboard::box(width = 3, status = "warning", title = "Settings",
                             selectInput("aggregate_select_tt", label = "Aggregate by", 
                                         choices  = csite$ui_attr$aggregate_list,
                                         selected = csite$ui_attr$aggregate_selec, 
                                         width = "80%"),

                             selectInput("color_select_tt", label = "Show color", choices = csite$ui_attr$trend_color_list,
                                         selected = csite$ui_attr$color_select_tt, width = "80%"),
                             
                             radioButtons("trend_or_threshold", label = "Display Table",
                                          choices  = csite$ui_attr$trend_thresh_list, 
                                          selected = csite$ui_attr$trend_thresh_selected)
                             
         ),
         shinydashboard::box(width = 9, title = "Trends & Thresholds", status = "primary",
                             htmlOutput("trend_table")
         ),
               
         #
         # Previously, the legend was in a separate tab inside a tabPanel().
         # I moved the legend into an absolutePanel() and put the 'trend_table'
         # into a shinydashboard::box(), see above.
         # (DELETE THE FOLLOWING CODE, IF NOTHING CHANGES.)
         #
         # shinydashboard::tabBox(title = "Trends & Thresholds", width = 9, 
         #                        
         #                        
         #                        
         #                        tabPanel("Indicator Table", 
         #                                 
         #                                 htmlOutput("trend_table")  #,
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
         # ),
         # 
         # tabPanel("Legend", 
         #          
         #         # plotOutput("plot_legend_traffic") 
         #         htmlOutput("trend_legend")
         #         
         #          
         # )
         #),
         absolutePanel(id = "timecontrol_tt", class = "panel panel-default", 
                       fixed = TRUE, draggable = TRUE, top = "auto", 
                       left = "auto", right = 10, bottom = 10,
                       width = 350, height = 110, style = "opacity: 0.90",
                       
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
         ), # end absolutePanel()
         absolutePanel(id = "timecontrol_tt2", class = "panel panel-default", 
                       fixed = TRUE, draggable = TRUE, top = "auto", 
                       left = "auto", right = 10, bottom = 130, style = "opacity: 0.90",
                       width = 350, height = 420,
                       htmlOutput("trend_legend")
         ) # end absolutePanel()

  ) # end fluidRow()
}
  