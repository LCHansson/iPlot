

textInput2 <- function (inputId, label, value = "", class = "") 
{
  tagList(
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value, class=class)
  )
}
