

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
                                      tags$img(src = "extdata/gwsdat_logo.png", 
                                               title = "GWSDAT Homepage", height = "40px"),
                                               style = "padding-top:5px; padding-bottom:5px;")
                                    ))




uiFull <- shinydashboard::dashboardPage(skin = "black",
  
  dbHeader, 
  shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(id = "sidebar_menu",
      shinydashboard::menuItem("Manage Data", tabName = "input_data", icon = icon("archive")),
      shinydashboard::menuItem("Analyse", tabName = "analysis", icon = icon("bar-chart")),
      shinydashboard::menuItem("Debug", tabName = "debug_it")
    )
  ),
  
  shinydashboard::dashboardBody(
                      shinyjs::useShinyjs(), 
                      #shinyjs::extendShinyjs(text = jsCode),
                      #path_to_extdata <- system.file("extdata", package = "GWSDAT"),
                      #cat("* adding ", path_to_extdata, " as resource\n"),
                      #shiny::singleton(tags$head(tags$script(src = system.file("www", "trafficlight.js", package = "GWSDAT")))),
                      #shiny::singleton(tags$head(tags$link(href = "extdata/trafficlight.css", rel = "stylesheet"))),
                      #includeScript("extdata/trafficlight.js"),
                      #includeHTML("inst/www/trafficlight.js"),
                      #shiny::singleton(tags$head(tags$script(src = "inst/extdata/trafficlight.js"))),
                      #includeScript("www/trafficlight.js"),
                      tags$head(HTML(sprintf(
                        "<script>
                        function jumpToPlot(i, j) {
                        $('.tabbable .nav.nav-tabs li a:first').click();
                        }
                        </script>"))),

    shinydashboard::tabItems(
     shinydashboard::tabItem(tabName = "input_data", 
      
        uiOutput("uiDataManager"),                     
        shinyjs::hidden( uiOutput("uiDataAddNew")),
        shinyjs::hidden( uiOutput("uiDataAddCSV")),
        shinyjs::hidden( uiOutput("uiDataAddExcel"))
        
      ),
      
      shinydashboard::tabItem(tabName = "analysis", 
              div(id = "data_select_page",
                  uiOutput("data_overview")
              ),
              shinyjs::hidden(div(id = "analyse_page",
                     uiOutput("rndAnalyse"))
              )
              ),
     shinydashboard::tabItem(tabName = "debug_it",
                             verbatimTextOutput("debug")
                             )
      
    ) # end tabItems
    ) # end dashboardBody
) # end ui


uiSimple <- shinydashboard::dashboardPage(skin = "black",

  dbHeader, 
  shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Analyse", tabName = "analysis", icon = icon("bar-chart"))
    ),
    collapsed = TRUE
  ),
  
  shinydashboard::dashboardBody(
                      shinyjs::useShinyjs(),
                      shiny::singleton(tags$head(tags$script(src = system.file("www", "trafficlight.js", package = "GWSDAT")))),
                      shiny::singleton(tags$head(tags$link(href = system.file("www", "trafficlight.css", package = "GWSDAT"), rel = "stylesheet"))),
                      shinydashboard::tabItems( shinydashboard::tabItem(tabName = "analysis", uiOutput("rndAnalyse")))
  ) # end dashboardBody 
) # end ui

