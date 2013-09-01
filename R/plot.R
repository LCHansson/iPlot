#' Mini plot
#' 
#' ...
#' 
#' @param title plot title
#' @param vec data vector
#' @param sel user input; vector of length two (c(min, max))
mini_plot <- function(title, vec, sel) {
    plot(table(vec), main = title, sub = paste("min:", min(sel), "\nmax:", max(sel)), xlab = "", ylab = "")
    abline(v = sel)
    usr <- par('usr')
    rect(usr[1], max(table(vec))/2, usr[2], usr[4])
    rect(usr[1], max(table(vec))/2, usr[2], usr[3])
    if(!is.null(min(sel))) {
      if(!is.null(max(sel))){
        rect(min(sel), usr[3], max(sel), usr[4], col=rgb(0, 0, 0,0.5))
      }
  }
}