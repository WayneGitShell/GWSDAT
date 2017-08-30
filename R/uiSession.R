
#' @import shiny
uiSession <- function() {
  
  fluidPage(
    
    shinydashboard::box(width = 5, title = "Save Session", solidHeader = TRUE, status = "primary",
        
      "Save and download the current Analysis session to a .RData file.",
      
      div(style = "margin-top: 10px",
            textInput("session_filename", "Filename", 
                      value = paste("GWSDAT_", gsub(":", "_", gsub(" ", "_", Sys.time())), ".RData", sep = ""))
      ),
      
      div(style = "float: right",
          downloadButton("save_session_btn", label = "Download", icon = icon("save"))
      )
                
    )
    
  )
  
}
