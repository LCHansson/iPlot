#' Bootstrap Select
#' 
#' ...
#' 
#' @param inputId input id
#' @export
bootstrapSelect <- function(inputId, options = NULL) {
  
  suppressMessages(singleton(addResourcePath("js", system.file("js", package = "iPlot"))))
  
  tagList(
    singleton(
      tags$head(
        tags$script(
          src = "js/bootstrap-select/bootstrap-select.min.js",
          type= "text/javascript"
        )
      )
    ),
    singleton(
      tags$head(
        tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "js/bootstrap-select/bootstrap-select.min.css"
        )
      )
    ),
    HTML(sprintf("<select id=\"%s\" class=\"selectpicker\" multiple>\n  <option value=\"1\" selected=\"selected\">1</option>\n  <option value=\"2\">2</option>\n  <option value=\"3\">3</option>\n</select>", inputId)),
    tags$script(
      sprintf(
        "$('.selectpicker').selectpicker(%s);",
        inputId,
        toJSON(options)
      )
    )
  )
}