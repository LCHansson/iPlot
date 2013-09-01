# http://stackoverflow.com/questions/15875786
# Special thanks to Rahul Savani!

library(data.table)

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

data <- na.omit(as.data.table(MASS::survey))
variables = c("Pulse", "Age")

shinyServer(function(input, output) {
  main_data <- reactive({
    #interval <- c(min(left_input$values), max(left_input$values))
    interval <- NULL
    if(length(interval) == 2){
      d <- data[Pulse %between% interval]
    } else {
      d <- data
    }
    return(d)
  })
  
  output$left_filter <- renderUI({
    plot_output_list <- lapply(variables, function(i) {
      tagList(
        plotOutput(paste0("plot", i), height = 250, width = 350, clickId = paste0("click", i)),
        textOutput(paste0("text", i))
      )
    })
    do.call(tagList, plot_output_list)
  })

  output$main_plot <- renderPlot({
    plot(main_data()$Age, main_data()$Pulse)
  })
  
  rv <- reactiveValues()
  for (var in variables) {

    local({
      i <- var
      rv[[i]] <- c(min(data[[i]], na.rm = T), max(data[[i]], na.rm = T))
        
      observe({
        rv[[i]] <- setInput(rv[[i]], input[[paste0("click", i)]], max(table(data[[i]]))/2)
      })
    })
    
    local({
      i <- var
      
      output[[paste0("plot", i)]] <- renderPlot({
        mini_plot(i, data[[i]], rv[[i]])
      })
    })
  }
})
