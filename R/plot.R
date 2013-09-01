#' Mini plot
#' 
#' ...
#' 
#' @param title plot title
#' @param vec data vector
#' @param sel user input; vector of length two (c(min, max))
mini_plot <- function(title, vec, sel) {
    plot(table(vec), main = title, sub = paste("min:", sel[[1]], "\nmax:", sel[[2]]), xlab = "", ylab = "")
    abline(v = sel)
    usr <- par('usr')
    rect(usr[1], max(table(vec))/2, usr[2], usr[4])
    rect(usr[1], max(table(vec))/2, usr[2], usr[3])
    if(!is.null(sel[[1]])) {
      if(!is.null(sel[[2]])){
        rect(sel[[1]], usr[3], sel[[2]], usr[4], col=rgb(0, 0, 0,0.5))
      }
  }
}