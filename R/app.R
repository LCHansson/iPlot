#' Interactive Plot
#' 
#' ...
#' 
#' @param data dataset
#' @param height height (app)
#' @param width width (app)
#' @param geom main plot geom function
#' @param ... see runApp()
#' 
#' @examples 
#' \dontrun{
#' iPlot(MASS::survey)
#' iPlot(mtcars, geom = geom_bar())
#' iPlot(as.data.table(ggplot2::diamonds), geom = geom_density(aes(position="stack")))
#' }
#' @export
iPlot <- function(
  data = ggplot2::diamonds,
  height = 600,
  width = 800,
  geom = geom_density(alpha = .3),
  liveSearchLimit = 7,
  iPoptions = list(),
  ...
){
  iPoptions <<- defaultOptions(iPoptions)
  
  if(class(data) != "iData") {
    static <<- iData(data) 
  } else {
    static <<- copy(data)
  }
  
#   graphTypes <<- c("density", "scatter")
  
  # Run app
  runApp("inst")
}
