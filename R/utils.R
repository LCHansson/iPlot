#' setInput
#' 
#' ...
#' 
#' @param a variable to set
#' @param b new input
#' @param limit plot y limit
setInput <- function(a, b, limit) {
  if(!is.null(b$x)) {
    if(b$y > limit){
      a[[2]] <- b$x
    } else {
      a[[1]] <- b$x
    }
  }
  return(a)
}
