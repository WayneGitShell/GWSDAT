

# uiDataManager <- function() {
# 
#   fluidPage(
#     fluidRow(
#       
#       h3("Data Manager"),
#       hr(),
#       "No data is present.",
#       a(id = "toggleDataImport", "Add", href = "#"),
#       " new data set.",
#       hr(),
#       #p(HTML("No data is present. <a href='#import_data_page'>Add</a> new data set.")),
#       #a(id = "toggleDataImport", "Add", href = "#")#,
#       actionButton("add_new_data", label = " Import Data", icon = icon("plus"), 
#                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
#     )
#   )
# }



uiDataImport <- function() {
  
  
  fluidPage(

    div(style = "margin-bottom: 10px", a(id = "toggleDataManager", "<- Go back.", href = "#")),
    
    box(width = 3, solidHeader = TRUE, status = "primary", 
           
           
           h3("Import New CSV Data"),
           "Select the contaminant data and well coordinate files in .csv format. The tables on the right allow you to edit individual values.",
           hr(),
           
           textInput("new_data_name", label = "Data Name", value = "Area 1"),
           fileInput('well_data_file', 'Well Data File (CSV)',
                     accept = c('text/csv', 
                                'text/comma-separated-values,text/plain', 
                                '.csv')),
           
           fileInput('well_coord_file', 'Well Coordinates File (CSV)',
                     accept = c('text/csv', 
                                'text/comma-separated-values,text/plain', 
                                '.csv')),
           
           hr(),
           
           checkboxInput('header', 'Header', TRUE),
           checkboxInput('excel_date', 'Transform Excel Date', TRUE),
           radioButtons('sep', 'Separator',
                        c(Comma = ',',
                          Semicolon = ';',
                          Tab = '\t'),
                        ','),
           radioButtons('quote', 'Quote',
                        c(None = '',
                          'Double Quote' = '"',
                          'Single Quote' = "'"),
                        '"'),
           hr(),
           actionButton("reset_button", label = "Reset"),
           actionButton("import_button", label = "Import Data", icon("arrow-down"), 
                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
           )
           
           
    ), # end box
    
    tabBox("Data", width = 9, 
           tabPanel(title = "Contaminant data",
                    withSpinner(rHandsontableOutput("table_conc_data"))
#                    tableOutput('table_well_data')
           ), 
           tabPanel(title = "Well Coordinates",
                    rHandsontableOutput("table_well_coord")
           )
    )
    
  ) # end fluidPage
}


uiDataImport_prev <- function() {


  fluidPage(
    
    column(3,
           
           a(id = "toggleDataManager", "<- Go back.", href = "#"),
           
           h3("Import New CSV Data"),
           "Select the well data file including the solute values for each well, and the well coordinates file in .csv file format.",
           
           hr(),
           fileInput('well_data_file', 'Well Data File (CSV)',
                     accept = c('text/csv', 
                                'text/comma-separated-values,text/plain', 
                                '.csv')),
           
           fileInput('well_coord_file', 'Well Coordinates File (CSV)',
                     accept = c('text/csv', 
                                'text/comma-separated-values,text/plain', 
                                '.csv')),
           
           hr(),
           
           checkboxInput('header', 'Header', TRUE),
           checkboxInput('excel_date', 'Transform Excel Date', FALSE),
           radioButtons('sep', 'Separator',
                        c(Comma = ',',
                          Semicolon = ';',
                          Tab = '\t'),
                        ','),
           radioButtons('quote', 'Quote',
                        c(None = '',
                          'Double Quote' = '"',
                          'Single Quote' = "'"),
                        '"'),
           hr(),
           actionButton("reset_button", label = "Reset"),
           #tags$head(
           #   tags$style(HTML('#run{background-color:orange}'))
           # ),
           actionButton("import_button", label = "Import Data", icon("arrow-down"), 
                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
           )
           
           
    ), # end column
    
    tabBox("Data", width = 8, 
      tabPanel(title = "Concentration Measures",
        tableOutput('table_well_data')
      ), 
      tabPanel(title = "Well Coordinates",
           tableOutput('table_well_coord')
      )
    )
    
  ) # end fluidPage
}
