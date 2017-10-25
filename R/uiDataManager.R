
uiDataManagerList <- function(csite_list) {

  # Will contain the button information needed to create observers later.
  del_btns <- list()
  
  html_out <- tagList(
    #shinydashboard::box(width = 3, 
    div(style = "float : right; margin-bottom: 5px",
        actionButton("add_session_data", label = "Load Data", icon = icon("plus"), 
                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        actionButton("add_new_data", label = "Add New Data", icon = icon("plus"), 
                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        actionButton("add_csv_data", label = "Import .csv Data", icon = icon("arrow-down"), 
                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        actionButton("add_excel_data", label = "Import Excel File", icon = icon("arrow-down"), 
                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    h2("Data Manager")
  )

  
  if (length(csite_list) == 0) {
    # No data exists.
    html_out <- tagList(html_out,
                        shinydashboard::box(width = 7, title = "No Data Present", 
                                            status = "warning", "Import and or add to analyse."))
  } else {
    
    data_sets <- getDataInfo(csite_list)
    
    btn_idx <- 1
    
    for (set_name in names(data_sets)) {
      
      # If data set can be deleted, create button in side the box.
      if (!data_sets[[set_name]]$do_not_del) {
        
        # Create the name of the button and a safe to list with associated data name.
        # This will be passed back to create an observer in the calling function.
        btn_name <- paste0("del_data_btn", btn_idx)
        del_btns[[length(del_btns) + 1]] <- list(btn_name = btn_name, 
                                                 csite_name = set_name)  
        
        btn_idx <- btn_idx + 1
      }
      
      html_out <- tagList(html_out, fluidRow(
        shinydashboard::box(width = 7, status = "primary", collapsible = TRUE,
                            title = set_name, 
                            div(style = "float: left", 
                                HTML(paste("<b>Contaminants</b>: ", pasteLimit(data_sets[[set_name]]$contaminants, limit = 4), "<br />")),
                                HTML(paste("<b>Wells</b>: ", pasteLimit(data_sets[[set_name]]$wells, limit = 4), "<br />")),
                                HTML(paste("<b>Aquifer</b>: ", paste(data_sets[[set_name]]$Aquifer, collapse = ", ")))
                                ),
                            
                            if (!data_sets[[set_name]]$do_not_del) { div(style = "display: inline-block; float : right", 
                                actionButton(btn_name, "Delete")
                            ) }
        )))
    }
  } # end of else
  
  return(list(html_out = html_out, del_btns = del_btns))
  
}




uiImportSessionData <- function(valid_data_name) {
  
  cat("* in uiImportSessionData\n")
  fluidPage(
    div(style = "margin-bottom: 10px", actionButton("gotoDataManager_d", label = "", icon = icon("arrow-left"))),
    
    shinydashboard::box(width = 3, solidHeader = TRUE, status = "primary", 
                        
                        
                        h3("Load Session Data"),
                        "Load a session file that was previously saved inside GWSDAT. The file has to be a valid .rds file in GWSDAT format.",
                        hr(),
                        
                        textInput("dname_sess", label = "Data Name", value = valid_data_name),
                        fileInput('data_session_file', 'Load .rds File', accept = c('.rds', '.RDS')),
                        actionButton("reset_sess_import", label = "Reset"),
                        actionButton("import_button_sess", label = "Add Data", icon("arrow-up"), 
                                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    
    shinydashboard::tabBox(title = "Data Preview", width = 9, id = "tabbox_nd_import",
                           tabPanel("Contaminant Data", rhandsontable::rHandsontableOutput("tbl_conc_sess")), 
                           tabPanel("Well Coordinates", rhandsontable::rHandsontableOutput("tbl_well_sess"))
    )
  )
}


uiImportNewData <- function(valid_data_name) {

  fluidPage(
    div(style = "margin-bottom: 10px", actionButton("gotoDataManager_a", label = "", icon = icon("arrow-left"))),
    
    shinydashboard::box(width = 3, solidHeader = TRUE, status = "primary", 
                        
                        
                        h3("Add New Data"),
                        "Enter the data directly or copy/paste into the tables.",
                        hr(),
                        
                        textInput("dname_nd", label = "Data Name", value = valid_data_name),
                        "Add multiple shape files by using Shift- or Ctrl- inside the Open Dialog.",
                        fileInput('shape_files_nd', 'Add Shape Files', accept = c('.shx', '.dbf', '.sbn', '.sbx', '.prj', '.shp'),
                                  multiple = TRUE),
                        actionButton("reset_nd_import", label = "Reset"),
                        actionButton("import_button_nd", label = "Add Data", icon("arrow-up"), 
                                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    
    shinydashboard::tabBox(title = "New Tables", width = 9, id = "tabbox_nd_import",
                           tabPanel("Contaminant Data", shiny::tagList(
                             div(style = "display: inline-block; float:left", actionButton("clear_tbl_conc_nd", "Clear Table")),
                             div(style = "float:left; margin-left: 5px; margin-bottom: 5px", actionButton("addrow_tbl_conc_nd", "Add Row", icon = icon("plus") )),
                             rhandsontable::rHandsontableOutput("tbl_conc_nd"),
                             HTML("<b>Hints</b>: Right click into table to remove rows. Select Well Names that already exist inside the Well Coordinates Tab.")
                           )), 
                           
                           tabPanel("Well Coordinates", shiny::tagList(
                             div(style = "display: inline-block; float:left", actionButton("clear_tbl_well_nd", "Clear Table")),
                             div(style = "float:left; margin-left: 5px; margin-bottom: 5px", actionButton("addrow_tbl_well_nd", "Add Row", icon = icon("plus") )),
                             rhandsontable::rHandsontableOutput("tbl_well_nd"),
                             HTML("<b>Hints</b>: Right click into table to remove rows.")
                           )), 
                           
                           tabPanel("Shape Files", {
                             shiny::tagList(
                                            rhandsontable::rHandsontableOutput("tbl_shape_nd"),
                                            shinyjs::hidden(div(id = "removeshp_nd", style = "margin-top: 5px", actionButton("remove_shapefiles_nd", label = "Remove All Files"))))
                           })
    )
  )
}



uiImportCSVData <- function(valid_data_name) {
  
  fluidPage(
    div(style = "margin-bottom: 10px", actionButton("gotoDataManager_c", label = "", icon = icon("arrow-left"))),
    
    shinydashboard::box(width = 3, solidHeader = TRUE, status = "primary", 
                        
                        
                        h3("Import CSV Data"),
                        "Select the contaminant data and well coordinate files in CSV format (see setting below).",
                        hr(),
                        
                        textInput("dname_csv", label = "Data Name", value = valid_data_name),
                        fileInput('well_data_csv', 'Contaminant Data File',
                                  accept = c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                        
                        fileInput('well_coord_csv', 'Well Coordinates File',
                                  accept = c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                        "Add multiple shape files by using Shift- or Ctrl- inside the Open Dialog.",
                        fileInput('shape_files_csv', 'Add Shape Files', accept = c('*.shx', '*.dbf', '*.sbn', '*.sbx', '*.prj', '*.shp'),
                                  multiple = TRUE),
                        
                        hr(),
                        "Change the format of the CSV file in case it can not be read properly.",
                        #checkboxInput('header', 'Header is Present', TRUE),
                        #checkboxInput('excel_date', 'Transform Excel Date', TRUE),
                        radioButtons('sep', 'Column Separator',
                                     c(Comma = ',',
                                       Semicolon = ';',
                                       Tab = '\t'),
                                     ','),
                        radioButtons('quote', 'Quote for Character Strings',
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
    
    shinydashboard::tabBox(title = "Data Preview", width = 9, id = "tabbox_csv_import",
                           tabPanel("Contaminant Data", 
                                    rhandsontable::rHandsontableOutput("tbl_conc_csv")
                           ), 
                           tabPanel("Well Coordinates", 
                                    rhandsontable::rHandsontableOutput("tbl_well_csv")
                           ),
                           #tabPanel("Shape Files", tableOutput("tbl_shape_csv"))
                           tabPanel("Shape Files", {
                             shiny::tagList(rhandsontable::rHandsontableOutput("tbl_shape_csv"),
                                            shinyjs::hidden(div(id = "removeshp_csv", style = "margin-top: 5px", 
                                                                actionButton("remove_shapefiles_csv", label = "Remove All Files"))))
                           })
    )
    
  ) # end fluidPage
}



uiImportExcelData <- function(csite_list) {
  
  fluidPage(
    div(style = "margin-bottom: 10px", actionButton("gotoDataManager_b", label = "", icon = icon("arrow-left"))),
    
    shinydashboard::box(width = 3, solidHeader = TRUE, status = "primary", 
                        
                        
                        h3("Import Excel Data"),
                        "Select the Excel file containing the GWSDAT data.",
                        hr(),
                        textInput("dname_xls", label = "Data Name", value = getValidDataName(csite_list)),
                        fileInput('excel_import_file', 'Excel File', accept = c(".xls", ".xlsx")),
                        # MIME type .xlsx: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet
                        "Add multiple shape files by using Shift- or Ctrl- inside the Open Dialog.",
                        fileInput('shape_files_xls', 'Add Shape Files', accept = c('.shx', '.dbf', '.sbn', '.sbx', '.prj', '.shp'),
                                  multiple = TRUE),
                        actionButton("reset_xls_import", label = "Reset"),
                        actionButton("import_button_xls", label = "Import Data", icon("arrow-down"), 
                                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        
    ),
    
    shinydashboard::tabBox(title = "Data Preview", width = 9, id = "tabbox_xls_import",
                           tabPanel("Contaminant Data", rhandsontable::rHandsontableOutput("tbl_conc_xls")
                           ), 
                           tabPanel("Well Coordinates", rhandsontable::rHandsontableOutput("tbl_well_xls")
                           ),
                           tabPanel("Shape Files", {
                             shiny::tagList(
                               HTML("Note: Shape files specified in the Excel file will not be automatically uploaded. Please use the <b>Add Shape Files</b> control in the left panel to upload files."),
                               rhandsontable::rHandsontableOutput("tbl_shape_xls"),
                                            shinyjs::hidden(div(id = "removeshp_xls", style = "margin-top: 5px", 
                                                                actionButton("remove_shapefiles_xls", label = "Remove All Files"))))
                           })
                             
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


