
#
# This code is from 
#   https://github.com/rstudio/shiny-examples/blob/master/036-custom-input-control/chooser.R
#

chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE) {

  # Make 'inst/chooserinput' directory available so chooser-input.js can be sourced (see below).
  shiny::addResourcePath(prefix='chooserinput', directoryPath = system.file('chooserinput', package = 'GWSDAT'))
    
  leftChoices <- lapply(leftChoices, shiny::tags$option)
  rightChoices <- lapply(rightChoices, shiny::tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
      singleton(shiny::tags$head(
      
      shiny::tags$script(src = "chooserinput/chooser-binding.js"),
      shiny::tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
        div(class="chooser-container chooser-left-container",
            shiny::tags$select(class="left", size=size, multiple=multiple, leftChoices)
        ),
        div(class="chooser-container chooser-center-container",
            icon("arrow-circle-o-right", "right-arrow fa-3x"),
            shiny::tags$br(),
            icon("arrow-circle-o-left", "left-arrow fa-3x")
        ),
        div(class="chooser-container chooser-right-container",
            shiny::tags$select(class="right", size=size, multiple=multiple, rightChoices)
        )
    )
  )
}

shiny::registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if (is.null(data))
    NULL
  else
    list(left=as.character(data$left), right=as.character(data$right))
}, force = TRUE)

