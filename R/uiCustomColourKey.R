
#' @import shiny
uiCustomColourKey <- function(csite) {

  fluidPage(
    
    shinydashboard::box(width = 5, title = "Customise the Spatial Plot Colour Key", solidHeader = TRUE, status = "primary",
        
      "Specify the solute concentration threshold values in ug/L",
      
      #div(style = "margin-top:10px", "Customise the Spatial Plot Colour Key"),
      
      div(style = "margin-top: 10px",
          rHandsontableOutput("ColourKeyRHandsontable")
      ),
      
      div(style = "margin-top:10px", "(Double-click on cells to edit)"),
      div(style = "margin-top:10px", "(Right-click on cells to add or delete rows)"),
      
      div(style = "float: right",
          actionButton("save_Colour_Key", label = "Save", icon = icon("save"))
      ),
      
      shinyjs::hidden( 
        div(id = "options_save_msg_Colour_Key",
            column(2, textOutput("options_saved_Colour_Key"), style = "float: right; color: Green;")))
                
    )
    
  )
  
}
