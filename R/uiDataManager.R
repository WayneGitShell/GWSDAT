

uiDataManager <- function() {

  fluidPage(
    fluidRow(
      
      h3("Data Manager"),
      hr(),
      "No data is present.",
      a(id = "toggleDataImport", "Add", href = "#"),
      " new data set.",
      hr(),
      #p(HTML("No data is present. <a href='#import_data_page'>Add</a> new data set.")),
      #a(id = "toggleDataImport", "Add", href = "#")#,
      actionButton("add_new_data", label = " Import Data", icon = icon("plus"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  )

}

uiDataImport <- function() {


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
           
           tags$hr(),
           
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
    
    column(4,
           #h4("Well and Solute data"),
           tableOutput('table_well_data')
    ), # end column
    column(4,
           #h4("Well and Solute data"),
           tableOutput('table_well_coord')
    )
    
  ) # end fluidPage
}
