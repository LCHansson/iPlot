#' actionButton2
#' 
#' an extension of actionButton
#' 
#' @param inputId Specifies the input slot that will be used to access the value.
#' @param label The contents of the button–usually a text label, but you could also use any other HTML, like an image.
#' @param class the class parameters of the \code{actionButton}
#' @export

actionButton2 <- function(inputId, label, class="btn action-button") {
  tags$button(id = inputId, type = "button", class = class,
              label)
}
