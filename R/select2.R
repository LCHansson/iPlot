#' Select2 Input
#' 
#' ...
#' 
#' @param inputId input id
#' @param ... see selectInput()
#' @param options select 2 options
#' @export
select2input <- function(inputId, ..., options = NULL) {
  tagList(
    singleton(tagList(
      includeCSS(system.file("js/select2/select2.css", package="iPlot")),
      includeScript(system.file("js/select2/select2.js", package="iPlot"))
    )
    ),
    selectInput(inputId = inputId, ...),
    tags$script(sprintf("$(document).ready(function() { $('#%s').select2(%s); });", inputId, toJSON(options)))
  )
}