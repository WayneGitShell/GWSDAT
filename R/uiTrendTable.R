

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
       
         absolutePanel(id = "timecontrol_tt", class = "panel panel-default", 
                       fixed = TRUE, draggable = TRUE, top = "auto", 
                       left = "auto", right = 10, bottom = 10,
                       width = 350, height = 130, style = "opacity: 0.90",
                       
                       div(style = "margin-left: 15px; margin-top: 5px;",
                           h4(textOutput("timepoint_tt_idx_label")),
                           sliderInput("timepoint_tt_idx",
                                       label="",
                                       min = 1,
                                       max = length(csite$ui_attr$timepoints),
                                       step = 1,
                                       value = csite$ui_attr$timepoint_tt_idx,
                                       animate = animationOptions(loop = TRUE, interval = 1500)
                           ) 
                       )
         ), # end absolutePanel()
       
        absolutePanel(id = "trendtable_legend", class = "panel panel-default", 
                      fixed = TRUE, draggable = TRUE, top = "auto", 
                      left = "auto", right = 10, bottom = 130, style = "opacity: 0.90",
                      width = 350, height = 420,
                      htmlOutput("trend_legend")
         )              
         #
         # Collapse absolutePanel(). This works, but after moving the panel, the lower
         # edge will stick to the 'bottom' parameter and the panel's height changes.
         # Changing the height to a fixed value, collapses the include HTML legend table
         # but not the panel. 
         #
         # absolutePanel(id = "trendtable_legend", class = "panel panel-default", 
         #               fixed = TRUE, draggable = TRUE, top = "auto", 
         #               left = "auto", right = 10, bottom = 130, style = "opacity: 0.90",
         #               width = 350, height = "auto",#420,
         #               HTML('<button data-toggle="collapse" data-target="#demo">Collapsible</button>'),
         #               tags$div(id = 'demo',  class = "collapse in",
         #               htmlOutput("trend_legend"))
         # ) # end absolutePanel()

  ) # end fluidRow()
}
  
