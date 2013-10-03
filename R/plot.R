#' Mini plot
#' 
#' ...
#' 
#' @param title plot title
#' @param vec data vector
#' @param sel user input; vector of length two (c(min, max))
mini_plot <- function(title, subtitle = "", vec, sel) {
   op <- par(mar = c(2, 0, 1, 0))
   plot(density(vec), xlab = "", ylab = "", axes=F, xaxt="n", yaxt="n", main=" ", cex.axis=0.8)
   abline(h = max(density(vec)$y)/2, lty="dashed")
   usr <- par('usr')
   if(!is.null(min(sel))) {
      if(!is.null(max(sel))){
         rect(min(sel), usr[3], max(sel), usr[4], col=rgb(0, 1, 1,0.2), border = NA)
      }
   }
   mtext(title, side=3, line=0, adj=0, cex=1, col="black")
   mtext(min(sel), side=1, line=0, adj=0, cex=1, col="grey")
   mtext(max(sel), side=1, line=0, adj=1, cex=1, col="grey")
   par(op)
}