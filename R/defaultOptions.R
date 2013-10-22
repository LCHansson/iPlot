#' Default options for iPlot
#' 
#' Add default options to an options list or create a new options list
#' 
#' @param options The iPlot options list to pass to the function

defaultOptions <- function(options = list()) {
  
  defaultOptionsList <- list(
    graph = TRUE,
    table = TRUE,
    na_handler = TRUE,
    facets = TRUE
  )
  
  lapply(names(defaultOptionsList), function(i) {
    if(!is.null(options[[i]])) return()
    
    options[[i]] <<- defaultOptionsList[[i]]
  })
  
  return(options)
}