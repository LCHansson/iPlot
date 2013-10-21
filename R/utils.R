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

#' NA or false
#' 
#' ...
#' 
#' @param x vector
#' @param na NA or false
na_or_false <- function(x, na) if (na) is.na(x) else rep(F, length(x))
