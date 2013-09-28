#' Mini plot
#' 
#' ...
#' 
#' @param title plot title
#' @param vec data vector
#' @param sel user input; vector of length two (c(min, max))
mini_plot <- function(title, vec, sel) {
    op <- par(mar = c(2, 0, 1, 0))
    plot(table(vec), xlab = "", ylab = "", bty="n", yaxt="n", xaxt="n", cex.axis = 0.8)
    abline(v = sel)
    middle <- max(table(vec))/2
    segments(sel[1], middle, sel[2], middle, lty = "dashed")
    axis(1, mgp=c(0, 0, 0), tick = F, cex.axis = 0.8)
    usr <- par('usr')
    if(!is.null(min(sel))) {
      if(!is.null(max(sel))){
        rect(min(sel), usr[3], max(sel), usr[4])
      }
    }
    mtext(title, side=3, line=0, adj=0, cex=0.9, col="black")
    par(op)
}