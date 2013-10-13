#' Mini plot
#' 
#' ...
#' 
#' @param title plot title
#' @param vec data vector, e.g. show density given all data
#' @param sel user input; vector of length two (c(min, max))
#' @param vec2 secondary data vector, e.g. show density given specific selection
mini_plot <- function(title, subtitle = "", vec, sel, vec2 = NULL) {
  op <- par(mar = c(2, 0, 1, 0))
  plot(density(vec), xlab = "", ylab = "", lwd=2, axes=F, xaxt="n", yaxt="n", main=" ", cex.axis=0.8)
  if(!is.null(vec2) & length(vec2) >= 2){
    lines(density(vec2), col="blue")
  }
  abline(h = max(density(vec)$y)/2, lty="dashed")
  usr <- par('usr')
  if(!is.null(min(sel))) {
    if(!is.null(max(sel))){
       rect(min(sel), usr[3], max(sel), usr[4], col=rgb(0, 0, 0, 0.2), border = NA)
    }
  }
  mtext(title, side=3, line=0, adj=0, cex=1, col="black")
  mtext(num_format(min(sel)), side=1, line=0, adj=0, cex=0.8, col="black")
  mtext(num_format(max(sel)), side=1, line=0, adj=1, cex=0.8, col="black")
  par(op)
}

#' Number format
num_format <- function(x) prettyNum(x, drop0trailing = TRUE, digits = 3)