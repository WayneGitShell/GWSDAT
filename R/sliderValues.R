
#
# [Not used] Attempt on updating the sliderValues input, but it will work only
#  for the 'label' attribute. See updateSliderInput() for a reference. 
#  Needs more insight I guess.
#
updateSliderValues <- function(session, inputId, label = NULL, from = NULL) 
{
  # vals <- shiny:::dropNulls(list(value, min, max))
  # type <- unique(lapply(vals, function(x) {
  #   if (inherits(x, "Date")) 
  #     "date"
  #   else if (inherits(x, "POSIXt")) 
  #     "datetime"
  #   else "number"
  # }))
  # if (length(type) > 1) {
  #   stop("Type mismatch for value, min, and max")
  # }
  # if ((length(type) == 1) && (type == "date" || type == "datetime")) {
  #   to_ms <- function(x) 1000 * as.numeric(as.POSIXct(x))
  #   if (!is.null(min)) 
  #     min <- to_ms(min)
  #   if (!is.null(max)) 
  #     max <- to_ms(max)
  #   if (!is.null(value)) 
  #     value <- to_ms(value)
  # }

  message <- shiny:::dropNulls(list(label = label, from = shiny:::formatNoSci(from)))
  session$sendInputMessage(inputId, message)
}


# Debounce slider inputs: The input of sliderValues() is not delayed.
#  To debounce input$timepoint by 'ms' milliseconds, define the following line 
#  and use timepoint_d()' as reactive to respond to delayed changes of input$timepoint. 
#
#timepoint_d <- debounce( reactive({input$timepoint  }), ms)



#
# [Not used] This modified slider input allows the passing of a vector of values
#  instead of just limits. The original sliderInput doesn't do this. 
#  The Problem: Can't update the attributes so far, and the grid looks too crowded
#  if 'values' has lots of content. 
#
sliderValues <- function(inputId,
                          label,
                          values,
                          from,
                          to = NULL,
                          grid = TRUE,
                          width = NULL,
                          postfix = NULL,
                          prefix = NULL,
                          dragRange = TRUE,
                          disable = FALSE,
                          animate = FALSE) {
  validate_fromto <-
    function(fromto = NULL,
             values = NULL,
             default = 0) {
      if (!is.null(fromto)) {
        if (is.character(values) & is.numeric(fromto)) {
          fromto <- fromto - 1
        } else {
          fromto <- which(values == fromto) - 1
        }
      } else {
        fromto <- default
      }
      return(fromto)
    }
  
  sliderProps <- shiny:::dropNulls(
    list(
      class = "js-range-slider",
      id = inputId,
      `data-type` = if (!is.null(to))
        "double"
      else
        "single",
      `data-from` = validate_fromto(fromto = from, values = values),
      `data-to`   = validate_fromto(fromto = to, values = values, default = length(values)),
      `data-grid` = grid,
      `data-prefix` = if (is.null(prefix)) {
        "null"
      } else {
        shQuote(prefix, "sh")
      },
      `data-postfix` = if (is.null(postfix)) {
        "null"
      } else {
        shQuote(postfix, "sh")
      },
      `data-drag-interval` = dragRange,
      `data-disable` = disable,
      `data-values` = if (is.numeric(values)) {
        paste(values, collapse = ", ")
      } else {
        paste(shQuote(values, type = "sh"), collapse = ", ")
      }
    )
  )
  
  sliderProps <- lapply(
    X = sliderProps,
    FUN = function(x) {
      if (identical(x, TRUE))
        "true"
      else if (identical(x, FALSE))
        "false"
      else
        x
    }
  )
  
  sliderTag <- tags$div(
    class = "form-group shiny-input-container",
    style = if (!is.null(width))
      paste0("width: ", htmltools::validateCssUnit(width), ";"),
    if (!is.null(label))
      shiny:::controlLabel(inputId, label),
    do.call(
      tags$input,
      list(
        type = if (is.numeric(values) &
                   is.null(to)) {
          "number"
        } else {
          "text"
        },
        #class = "js-range-slider",
        id = inputId,
        name = inputId,
        value = ""
      )
    ),
    tags$style(
      whisker::whisker.render(
        template =
          "input[id='{{id}}'] {
        -moz-appearance:textfield;
}
input[id='{{id}}']::-webkit-outer-spin-button,
input[id='{{id}}']::-webkit-inner-spin-button {
-webkit-appearance: none;
margin: 0;
}", data = list(id = inputId))
    ),
    tags$script(
      HTML(
        whisker::whisker.render(
          template = '$("#{{id}}").ionRangeSlider({
          type: "{{data-type}}",
          from: {{data-from}},
          to: {{data-to}},
          grid: {{data-grid}},
          keyboard: true,
          keyboard_step: 1,
          postfix: {{data-postfix}},
          prefix: {{data-prefix}},
          drag_interval: {{data-drag-interval}},
          values: [{{data-values}}],
          disable: {{data-disable}}
          });',
          data = sliderProps
      )
      ))
      )
  if (identical(animate, TRUE)) 
    animate <- animationOptions()
  if (!is.null(animate) && !identical(animate, FALSE)) {
    if (is.null(animate$playButton)) 
      animate$playButton <- icon("play", lib = "glyphicon")
    if (is.null(animate$pauseButton)) 
      animate$pauseButton <- icon("pause", lib = "glyphicon")
    sliderTag <- htmltools::tagAppendChild(
      sliderTag,
      tags$div(class = "slider-animate-container", 
               tags$a(href = "#", class = "slider-animate-button", 
                      `data-target-id` = inputId, `data-interval` = animate$interval, 
                      `data-loop` = animate$loop, span(class = "play", 
                                                       animate$playButton), 
                      span(class = "pause", 
                           animate$pauseButton)))
    )
  }
  dep <- htmltools::htmlDependency(
    "ionrangeslider",
    "2.1.12",
    c(href = "shared/ionrangeslider"),
    script = "js/ion.rangeSlider.min.js",
    stylesheet = c(
      "css/ion.rangeSlider.css",
      "css/ion.rangeSlider.skinShiny.css"
    )
  )
  htmltools::attachDependencies(sliderTag, dep)
}


# This works for the 'label' attributes, but not for the others.
# Would be great to get it to work for 'from', 'to', 'values' 
updateLabelSliderValues <- function(session, inputID, label) {
  
  message <- shiny:::dropNulls(list(label = as.character(label)))
  session$sendInputMessage(inputID, message)

}




#
# [Not used] A slim version of sliderValues(). Does not directly provide with 
#  'inputId' the value in 'values', but an index value into 'values'. Kept it 
#  to here for later testing of an updateSliderValues_slim() method.
#
sliderValues_slim <- function (inputId, label, values, from, to = NULL, width = NULL) {
  sliderProps <- shiny:::dropNulls(list(class = "js-range-slider", 
                                        id = inputId,  
                                        `data-type` = if (!is.null(to)) "double",
                                        `data-from` = which(values == from) - 1,
                                        `data-to` = if (!is.null(to)) which(values == to) - 1,
                                        `data-grid` = TRUE,
                                        `data-values` = paste(values, collapse = ", ")
  ))
  sliderProps <- lapply(sliderProps, function(x) {
    if (identical(x, TRUE)) 
      "true"
    else if (identical(x, FALSE)) 
      "false"
    else x
  })
  sliderTag <- div(class = "form-group shiny-input-container", 
                   style = if (!is.null(width)) 
                     paste0("width: ", validateCssUnit(width), ";"), 
                   if (!is.null(label)) 
                     shiny:::controlLabel(inputId, label), do.call(tags$input, 
                                                                   sliderProps))
  dep <- list(htmltools::htmlDependency("ionrangeslider", "2.0.12", c(href = "shared/ionrangeslider"), 
                                        script = "js/ion.rangeSlider.min.js",
                                        stylesheet = c("css/ion.rangeSlider.css",
                                                       "css/ion.rangeSlider.skinShiny.css")))
  htmltools::attachDependencies(sliderTag, dep)
}
