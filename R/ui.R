#' @importFrom utils globalVariables
globalVariables("APP_CUSTOM_COMPONENT")

# This is the main UI file that defines the two types of interfaces: 
#  1. uiFull : running on a server with multiple data sets.
#  2. uiSimple : running  locally on a single data set (a.k.a. 'ExcelMode') 
#


createLogoCSS <- function() {
    tags$head(tags$style(HTML('
      .main-header .logo {
        background-image: url("www/logo.png");
        background-repeat: no-repeat;
        background-size: contain;
        background-position: left;
      }
    ')))
}


dbHeaderFull <- function() shinydashboard::dashboardHeader(title = "GWSDAT", 
                                                shinydashboard::dropdownMenuOutput("welcomeMsg"), 
                                                shinydashboard::dropdownMenuOutput("logAction"),
                                                shinydashboard::dropdownMenuOutput("signupAction"),tags$li(a(href = 'http://gwsdat.net',target="_blank",
                                                                                                             icon("home"), title = "GWSDAT Homepage and User Manual"), class = "dropdown"))


uiFull <- function() shinydashboard::dashboardPage(skin = "black",
  
  dbHeaderFull(), 
  shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(id = "sidebar_menu",
      shinydashboard::menuItem("Manage Data", tabName = "menu_data_manager", icon = icon("archive")),
      shinydashboard::menuItem("Analyse", tabName = "menu_analyse", icon = icon("bar-chart")),
      shinydashboard::menuItem("Log and Jobs", tabName = "logs_jobs", icon = icon("wpforms"))
    )
  ),
  
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(), 
    createLogoCSS(),
    # if (exists("APP_CUSTOM_COMPONENT", envir = .GlobalEnv)) 
    #     APP_CUSTOM_COMPONENT(),
    
    # Not using includeScript because it wraps <script> tags
    # around which doesn't work with the GA js directives.
    #tags$head(includeHTML("inst/www/google-analytics.js")),
    tags$head(includeHTML(system.file("www/google-analytics.js", package="GWSDAT"))),
    
    # Load .js Code that jumps from trend table to time-series table.
    tags$head(includeScript(system.file("www/jump_to_tsplot.js", package="GWSDAT"))),
    
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
dbHeaderSimple <- function() shinydashboard::dashboardHeader(title = "GWSDAT",
    tags$li(a(href = 'http://gwsdat.net',target="_blank",
              icon("home"), title = "GWSDAT Homepage and User Manual"), class = "dropdown")
)



uiSimple <- function() shinydashboard::dashboardPage(skin = "black",

  dbHeaderSimple(), 
  shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Analyse", tabName = "analysis", icon = icon("bar-chart")),
    shinydashboard::menuItem("Logs and Jobs", tabName = "logs_jobs", icon = icon("wpforms"))
    ),
    collapsed = TRUE
  ),
  
  shinydashboard::dashboardBody(
                      shinyjs::useShinyjs(),
                      createLogoCSS(),
                      shiny::singleton(tags$head(tags$script(src = system.file("www", "trafficlight.js", package = "GWSDAT")))),
                      shiny::singleton(tags$head(tags$link(href = system.file("www", "trafficlight.css", package = "GWSDAT"), rel = "stylesheet"))),
                      shinydashboard::tabItems( 
                        shinydashboard::tabItem(tabName = "analysis", uiOutput("rndAnalyse")),
                        shinydashboard::tabItem(tabName = "logs_jobs", uiOutput("uiLogsJobs"))
                      )
                      
  ) # end dashboardBody 
) # end ui



#
# Note: The following two functions (uiLoginModal and uiLoginSignup) where originally placed in the file uiLogin.R. There were moved here to bypass the following CRAN Note (using R CMD check --as-cran <GWSDAT_X.X.X.tar.gz>):
#
# * checking R code for possible problems ... NOTE
# server: no visible global function definition for ‘uiLoginModal’
# server: no visible global function definition for ‘uiLoginSignup’
# Undefined global functions or variables:
#  uiLoginModal uiLoginSignup
#
# As the message says, they are called from server() but they can't be seen.
# Other ui functions are handled in the same way but can be seen in their respective ui*.R files. It is not clear why this happens. Please move these functions to an appropriate script file when this is resolved.
#



uiLoginModal <- function() {
  return(shiny::modalDialog(
    
    h3('Login'),
    
    textInput("login_email", "Email:"),    
    passwordInput("login_password", "Password:"),
    div(style = 'color: red; margin-bottom: 5px', textOutput('wrongPasswordMsg1')),  
    actionButton("doLogin", "Login", icon = icon("sign-in")),
   
    footer = tagList(
      actionButton("cancelLogin", "Cancel")
    )
  ))
}

uiSignupModal <- function() {
    return(modalDialog(

    h3('Sign-up'),
  
    div(style = "margin-top: 25px; margin-bottom: 25px", 'If not already registered, sign-up by specifying an e-mail and password.'),
    textInput("signup_email", "Email:"),    
    passwordInput("signup_password", "Password:"),
    passwordInput("signup_password2", "Repeat password:"),
    div(style = 'color: red; margin-bottom: 5px', textOutput('wrongPasswordMsg2')),  
    actionButton("doSignup","Sign up", icon = icon("user-plus")),
    
    
    footer = tagList(
      actionButton("cancelSignup", "Cancel")
    )
  ))
}

