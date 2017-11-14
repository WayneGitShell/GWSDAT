

#
# Define the Shiny dashboard header
#
dbHeader <- shinydashboard::dashboardHeader(title = "GWSDAT.beta",
                            tags$li(a(href = 'http://www.api.org/oil-and-natural-gas/environment/clean-water/ground-water/gwsdat',
                                      icon("home"),
                                      title = "GWSDAT Homepage"),
                                    class = "dropdown"))
                  
                            # Not using the image because can't get the resource to load:
                            # tags$li(class = "dropdown", 
                            #         tags$a(href = 'http://www.api.org/oil-and-natural-gas/environment/clean-water/ground-water/gwsdat',
                            #           target = '_blank',
                            #           tags$img(src = "extdata/gwsdat_logo.png", 
                            #                    title = "GWSDAT Homepage", height = "40px"),
                            #                    style = "padding-top:5px; padding-bottom:5px;")
                            #         ))




uiFull <- shinydashboard::dashboardPage(skin = "black",
  
  dbHeader, 
  shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(id = "sidebar_menu",
      shinydashboard::menuItem("Manage Data", tabName = "menu_data_manager", icon = icon("archive")),
      shinydashboard::menuItem("Analyse", tabName = "menu_analyse", icon = icon("bar-chart")),
      shinydashboard::menuItem("Version", tabName = "menu_version")
    )
  ),
  
  shinydashboard::dashboardBody(
                      shinyjs::useShinyjs(), 
                      
                      #tags$head(includeScript("inst/www/google-analytics.js")),
                      
                      tags$head(HTML(sprintf(
                      "<!-- Global site tag (gtag.js) - Google Analytics -->
                        <script async src='https://www.googletagmanager.com/gtag/js?id=UA-109683161-1'></script>
                        <script>
                        window.dataLayer = window.dataLayer || [];
                      function gtag(){dataLayer.push(arguments);}
                      gtag('js', new Date());
                      gtag('config', 'UA-109683161-1');
                      </script>"))),
                        
                      #shinyjs::extendShinyjs(text = jsCode),
                      #path_to_extdata <- system.file("extdata", package = "GWSDAT"),
                      #cat("* adding ", path_to_extdata, " as resource\n"),
                      #shiny::singleton(tags$head(tags$script(src = system.file("www", "trafficlight.js", package = "GWSDAT")))),
                      #shiny::singleton(tags$head(tags$link(href = "extdata/trafficlight.css", rel = "stylesheet"))),
                      #includeScript("extdata/trafficlight.js"),
                      #includeHTML("inst/www/trafficlight.js"),
                      #shiny::singleton(tags$head(tags$script(src = "inst/extdata/trafficlight.js"))),
                      #includeScript("www/trafficlight.js"),
                      
                      #tags$head(HTML(sprintf(
                      #  "<script>
                      #  function jumpToPlot(i, j) {
                      #  $('.tabbable .nav.nav-tabs li a:first').click();
                      #  }
                      #  </script>"))),
                 
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "menu_data_manager", 
      
        uiOutput("uiDataManager"),                
        shinyjs::hidden( uiOutput("uiDataAddSession")),
        shinyjs::hidden( uiOutput("uiDataAddNew")),
        shinyjs::hidden( uiOutput("uiDataAddCSV")),
        shinyjs::hidden( uiOutput("uiDataAddExcel")),
        shinyjs::hidden( uiOutput("uiDataEdit"))
        
      ),
      
      shinydashboard::tabItem(tabName = "menu_analyse", 
              div(id = "data_select_page",
                  uiOutput("uiAnalyseDataList")
              ),
              shinyjs::hidden(div(id = "analyse_page",
                     uiOutput("rndAnalyse"))
              )
      ),
     
      shinydashboard::tabItem(tabName = "menu_version",
                             verbatimTextOutput("version_info")
      )
      
    ) # end tabItems
    ) # end dashboardBody
) # end ui


uiSimple <- shinydashboard::dashboardPage(skin = "black",

  dbHeader, 
  shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Analyse", tabName = "analysis", icon = icon("bar-chart")),
    shinydashboard::menuItem("Version", tabName = "menu_version")
    ),
    collapsed = TRUE
  ),
  
  shinydashboard::dashboardBody(
                      shinyjs::useShinyjs(),
                      shiny::singleton(tags$head(tags$script(src = system.file("www", "trafficlight.js", package = "GWSDAT")))),
                      shiny::singleton(tags$head(tags$link(href = system.file("www", "trafficlight.css", package = "GWSDAT"), rel = "stylesheet"))),
                      shinydashboard::tabItems( 
                        shinydashboard::tabItem(tabName = "analysis", uiOutput("rndAnalyse")),
                        shinydashboard::tabItem(tabName = "menu_version", verbatimTextOutput("version_info"))
                      )
                      
  ) # end dashboardBody 
) # end ui

