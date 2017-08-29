

#
# Define the Shiny dashboard header
#
dbHeader <- shinydashboard::dashboardHeader(title = "GWSDAT",
#                            tags$li(a(href = 'http://shinyapps.company.com',
#                                      icon("power-off"),
#                                      title = "Back to Apps Home"),
#                                    class = "dropdown"))#,
                            tags$li(class = "dropdown", 
                                    tags$a(href = 'http://www.api.org/oil-and-natural-gas/environment/clean-water/ground-water/gwsdat',
                                      target = '_blank',
                                      tags$img(src = system.file("extdata", "gwsdat_logo.png", package = "GWSDAT"), 
                                          title = "GWSDAT Homepage", height = "40px"),
                                      style = "padding-top:5px; padding-bottom:5px;")
                                    ))




 




uiFull <- shinydashboard::dashboardPage(skin = "black",
  
  dbHeader, 
  shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(id = "sidebar_menu",
    shinydashboard::menuItem("Manage Data", tabName = "input_data", icon = icon("archive")),
    shinydashboard::menuItem("Analyse", tabName = "analysis", icon = icon("bar-chart"))
  )),
  
  shinydashboard::dashboardBody(shinyjs::useShinyjs(), 

    shinydashboard::tabItems(
     shinydashboard::tabItem(tabName = "input_data", 
      
        div(id = "data_manager", uiOutput("uiDataManager")),
        shinyjs::hidden( div(id = "data_add_csv", uiOutput("uiDataAddCSV"))),
        shinyjs::hidden( div(id = "data_add_new", uiOutput("uiDataAddNew"))),
        shinyjs::hidden( div(id = "data_add_excel", uiOutput("uiDataAddExcel")))
        
      ),
      
      shinydashboard::tabItem(tabName = "analysis", 
              div(id = "data_select_page",
                  uiOutput("data_overview")
              ),
              shinyjs::hidden(div(id = "analyse_page",
                     uiOutput("rndAnalyse"))
              )
      )
      
    ) # end tabItems
 ) # end dashboardBody 
) # end ui


uiSimple <- shinydashboard::dashboardPage(skin = "black",
  
  dbHeader, 
  shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Analyse", tabName = "analysis", icon = icon("bar-chart")),
    shinydashboard::menuItem("Save Session", tabName = "session", icon = icon("save"))
    )
  ),
  
  shinydashboard::dashboardBody(shinyjs::useShinyjs(),
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "analysis", uiOutput("rndAnalyse"))# , 
      #shinydashboard::tabItem(tabName = "session",  uiSession())         
    ) # end tabItems
  ) # end dashboardBody 
) # end ui

