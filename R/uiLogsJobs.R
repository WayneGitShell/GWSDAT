

uiLogsJobs <- function() {
  
  fluidRow(
      
    shinydashboard::tabBox(title = "", width = 12, id = "logs_panel",
                           tabPanel("Logs", verbatimTextOutput("logs_view")
                                    ), 
                           tabPanel("Job Queue", 
                                    h3("Job Queue"),
                                    tableOutput("job_queue_table"),
                                    h3("Running Jobs"),
                                    tableOutput("job_run_table"),
                                    h3("Done"),
                                    tableOutput("job_done_table")
                           )
    )
  )
}