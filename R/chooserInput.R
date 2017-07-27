
#
# This code is from 
#   https://github.com/rstudio/shiny-examples/blob/master/036-custom-input-control/chooser.R
#
#
# Packaging note (from the github Readme.md):
#
# This example is set up for easy reading of the code and easy running via shiny::runGitHub().
#
# If instead we wanted to make this line chart component easily distributable to other Shiny 
# users, we would set it up as a package. chooser.R would go into the R subdirectory. The 
# contents of www would be moved to inst, and chooserInput would call shiny::addResourcePath 
# to make them available at a URL prefix like "chooser". See shiny-incubator for one example.
#

chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
      tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
        div(class="chooser-container chooser-left-container",
            tags$select(class="left", size=size, multiple=multiple, leftChoices)
        ),
        div(class="chooser-container chooser-center-container",
            icon("arrow-circle-o-right", "right-arrow fa-3x"),
            tags$br(),
            icon("arrow-circle-o-left", "left-arrow fa-3x")
        ),
        div(class="chooser-container chooser-right-container",
            tags$select(class="right", size=size, multiple=multiple, rightChoices)
        )
    )
  )
}

registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if (is.null(data))
    NULL
  else
    list(left=as.character(data$left), right=as.character(data$right))
}, force = TRUE)

