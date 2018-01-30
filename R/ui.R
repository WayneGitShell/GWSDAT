# This is the main UI file that defines the two types of interfaces: 
#  1. uiFull : running on a server with multiple data sets.
#  2. uiSimple : running  locally on a single data set (a.k.a. 'ExcelMode') 
#



dbHeaderFull <- shinydashboard::dashboardHeader(title = "GWSDAT Beta", 
                                                shinydashboard::dropdownMenuOutput("welcomeMsg"), 
                                                shinydashboard::dropdownMenuOutput("logAction"),
                                                shinydashboard::dropdownMenuOutput("signupAction"))


uiFull <- shinydashboard::dashboardPage(skin = "black",
  
  dbHeaderFull, 
  shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(id = "sidebar_menu",
      shinydashboard::menuItem("Manage Data", tabName = "menu_data_manager", icon = icon("archive")),
      shinydashboard::menuItem("Analyse", tabName = "menu_analyse", icon = icon("bar-chart")),
      shinydashboard::menuItem("Log and Jobs", tabName = "logs_jobs", icon = icon("wpforms"))
    )
  ),
  
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(), 
    
    # Not using includeScript because it wraps <script> tags
    # around which doesn't work with the GA js directives.
    tags$head(includeHTML("inst/www/google-analytics.js")),
    
    # Load .js Code that jumps from trend table to time-series table.
    tags$head(includeScript("inst/www/jump_to_tsplot.js")),
    
    # Makes the sidebar minimize to icons only.
    tags$script(HTML("$('body').addClass('sidebar-mini');")),
    
    
    #path_to_extdata <- system.file("extdata", package = "GWSDAT"),
    #cat("* adding ", path_to_extdata, " as resource\n"),
    #shiny::singleton(tags$head(tags$script(src = system.file("www", "trafficlight.js", package = "GWSDAT")))),
    #shiny::singleton(tags$head(tags$link(href = "extdata/trafficlight.css", rel = "stylesheet"))),
    
                 
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
     
      shinydashboard::tabItem(tabName = "logs_jobs",
                              uiOutput("uiLogsJobs")
      )
      
    ) # end tabItems
    ) # end dashboardBody
) # end ui



# Define the Shiny dashboard header
dbHeaderSimple <- shinydashboard::dashboardHeader(title = "GWSDAT.beta",
    tags$li(a(href = 'http://www.api.org/oil-and-natural-gas/environment/clean-water/ground-water/gwsdat',
              icon("home"), title = "GWSDAT Homepage"), class = "dropdown")
)



uiSimple <- shinydashboard::dashboardPage(skin = "black",

  dbHeaderSimple, 
  shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Analyse", tabName = "analysis", icon = icon("bar-chart")),
    shinydashboard::menuItem("Logs and Jobs", tabName = "logs_jobs", icon = icon("wpforms"))
    ),
    collapsed = TRUE
  ),
  
  shinydashboard::dashboardBody(
                      shinyjs::useShinyjs(),
                      shiny::singleton(tags$head(tags$script(src = system.file("www", "trafficlight.js", package = "GWSDAT")))),
                      shiny::singleton(tags$head(tags$link(href = system.file("www", "trafficlight.css", package = "GWSDAT"), rel = "stylesheet"))),
                      shinydashboard::tabItems( 
                        shinydashboard::tabItem(tabName = "analysis", uiOutput("rndAnalyse")),
                        shinydashboard::tabItem(tabName = "logs_jobs", uiOutput("uiLogsJobs"))
                      )
                      
  ) # end dashboardBody 
) # end ui

