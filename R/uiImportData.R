


uiImportExcelData <- function(csite_list) {
  
  fluidPage(
    div(style = "margin-bottom: 10px", actionButton("gotoDataManager_b", label = "", icon = icon("arrow-left"))),
    
    shinydashboard::box(width = 3, solidHeader = TRUE, status = "primary", 
                        
                        
                        h3("Import Excel Data"),
                        "Select the Excel file containing the GWSDAT data.",
                        hr(),
                        textInput("new_data_name", label = "Data Name", value = getValidDataName(csite_list)),
                        fileInput('excel_import_file', 'Excel File', accept = c('.xls', '.xlsx')),
                        actionButton("reset_xls_import", label = "Reset"),
                        actionButton("import_button_xls", label = "Import Data", icon("arrow-down"), 
                                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        
    ),
    
    shinydashboard::tabBox(title = "Imported Tables", width = 9, 
                           tabPanel("Contaminant Data", rhandsontable::rHandsontableOutput("tbl_conc_xls")
                           ), 
                           tabPanel("Well Coordinates", rhandsontable::rHandsontableOutput("tbl_well_xls")
                           )#,
                           #tabPanel("Shape Files", rhandsontable::rHandsontableOutput("tbl_shape_xls"))
                           #tabPanel("Shape Files", "Shape files must be uploaded to the server.")
                           #rhandsontable::rHandsontableOutput("tbl_shape_xls")
                           #fileInput('shapefile_import', 'Shape File (*.shp)', accept = c('.shp'))
                           
    ))
  
}

#
# Previously, I had a single call to output$uiDataAddExcel (was inside server()). 
# It reacted to changes in the variables input$add_excel_data and input$reset_xls_import (buttons). 
# This worked fine, however, renderUI() was executed twice, ones for each reactive variable
# although only one changed. This might be because the function generates one of the 
# reactive variables: input$reset_xls_import and it changes its value. Thus, right after 
# it is created, renderUI() is triggered again, because it reacts to input$reset_xls_import.
# 
# To counter this, I call renderUI() directly from an 
#   observeEvent(input$reset_xls_import, ..)  and 
#   observeEvent(input$add_excel_data, ..)  and 
#
# Btw: The same scenario is happening with the uiDataManager. 
#
# output$uiDataAddExcel <- renderUI({
#   cat("* in uiDataAddExcel()\n")
#   
#   # React to changes in these:
#   input$add_excel_data
#   input$reset_xls_import
#   
#   import_tables$DF_well <<- NULL
#   import_tables$DF_conc <<- NULL
#   
#   fluidPage(
#     div(style = "margin-bottom: 10px", actionButton("gotoDataManager_b", label = "", icon = icon("arrow-left"))),
#     
#     shinydashboard::box(width = 3, solidHeader = TRUE, status = "primary", 
#                         
#                         
#                         h3("Import Excel Data"),
#                         "Select the Excel file containing the GWSDAT data.",
#                         hr(),
#                         textInput("new_data_name", label = "Data Name", value = getValidDataName(csite_list)),
#                         fileInput('excel_import_file', 'Excel File', accept = c('.xls', '.xlsx')),
#                         actionButton("reset_xls_import", label = "Reset"),
#                         actionButton("import_button_xls", label = "Import Data", icon("arrow-down"), 
#                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
#                         
#     ),
#     
#     shinydashboard::tabBox(title = "Imported Tables", width = 9, 
#                            tabPanel("Contaminant Data", rhandsontable::rHandsontableOutput("tbl_conc_xls")
#                            ), 
#                            tabPanel("Well Coordinates", rhandsontable::rHandsontableOutput("tbl_well_xls")
#                            )#,
#                            #tabPanel("Shape Files", rhandsontable::rHandsontableOutput("tbl_shape_xls"))
#                            #tabPanel("Shape Files", "Shape files must be uploaded to the server.")
#                            #rhandsontable::rHandsontableOutput("tbl_shape_xls")
#                            #fileInput('shapefile_import', 'Shape File (*.shp)', accept = c('.shp'))
#                            
#     ))
# })



uiImportCSVData <- function(valid_data_name) {
  
  fluidPage(
    div(style = "margin-bottom: 10px", actionButton("gotoDataManager_c", label = "", icon = icon("arrow-left"))),
    
    shinydashboard::box(width = 3, solidHeader = TRUE, status = "primary", 
                        
                        
                        h3("Import .csv Data"),
                        "Select the contaminant data and well coordinate files in .csv format. The tables on the right allow you to edit individual values.",
                        hr(),
                        
                        textInput("new_data_name", label = "Data Name", value = valid_data_name),
                        fileInput('well_data_file', 'Contaminant Data File',
                                  accept = c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                        
                        fileInput('well_coord_file', 'Well Coordinates File',
                                  accept = c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                        "Add multiple shape files by using Shift- or Ctrl- inside the Open Dialog.",
                        fileInput('shape_files_csv', 'Add Shape Files', accept = c('*.shx', '*.dbf', '*.sbn', '*.sbx', '*.prj', '*.shp'),
                                  multiple = TRUE),
                        
                        hr(),
                        
                        checkboxInput('header', 'Header', TRUE),
                        #checkboxInput('excel_date', 'Transform Excel Date', TRUE),
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
                        actionButton("reset_csv_import", label = "Reset"),
                        actionButton("import_button_csv", label = "Import Data", icon("arrow-down"), 
                                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                        )
                        
                        
    ), # end box
    
    shinydashboard::tabBox(title = "Imported Tables", width = 9, id = "tabbox_csv_import",
                           tabPanel("Contaminant Data", rhandsontable::rHandsontableOutput("tbl_conc_csv")
                           ), 
                           tabPanel("Well Coordinates", rhandsontable::rHandsontableOutput("tbl_well_csv")
                           ),
                           #tabPanel("Shape Files", tableOutput("tbl_shape_csv"))
                           tabPanel("Shape Files", {
                             shiny::tagList(rhandsontable::rHandsontableOutput("tbl_shape_csv"),
                              shinyjs::hidden(div(id = "testingaa", style = "margin-top: 5px", actionButton("remove_shapefiles_csv", label = "Remove All Files"))))
                             })
    )
    
  ) # end fluidPage
}
