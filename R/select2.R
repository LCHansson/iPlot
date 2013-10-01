#' Select2 Input
#' 
#' ...
#' 
#' @param inputId input id
#' @param ... see selectInput()
#' @param options select 2 options
#' @export
select2input <- function(inputId, ..., options = list(width = "200px")) {
  
  suppressMessages(singleton(addResourcePath("js", system.file("js", package = "iPlot"))))
  
  tagList(
    singleton(
      tags$head(
        tags$script(
          src = "js/select2/select2.js",
          type= "text/javascript"
        )
      )
    ),
    singleton(
      tags$head(
        tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "js/select2/select2.css"
        )
      )
    ),
    selectInput(
      inputId = inputId,
      ...
    ),
    tags$script(
      sprintf(
        "$(document).ready(function() { $('#%s').select2(%s); });",
        inputId,
        toJSON(options)
      )
    )
  )
}