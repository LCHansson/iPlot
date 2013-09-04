#' Interactive Plot
#' 
#' ...
#' 
#' @param x variable
#' @paran y color variable
#' @param data dataset
#' @param vars filter variables
#' @param height height (app)
#' @param width width (app)
#' @param ... see runApp()
#' 
#' @examples 
#' \dontrun{
#' iPlot("Pulse", "Age", MASS::survey, c("Pulse", "Age", "Height", "NW.Hnd", "Wr.Hnd"))
#' }
#' @export
iPlot <- function(x, y, data, vars, height = 100, width = 250, ...) {
  
  # Subset data
  data <- data[ , unique(c(x, y, vars))]
  
  # Remove NA's
  pre_nrow <- nrow(data)
  data <- na.omit(as.data.table(data))
  diff <- pre_nrow - nrow(data)
  if (diff > 0) {
    warning(paste(diff, "NA rows has been removed"))
  }
  
  # Run app
  runApp(
    list(
      ui = bootstrapPage(
        HTML("<table><tr><td>"),
        uiOutput("filters"),
        HTML("</td><td>"),
        plotOutput("main_plot", height = height, width = width*0.8),
        HTML("</td></tr></table>")
      ),
      server = function(input, output, session) {
        main_data <- reactive({
          conditions <- lapply(vars, function(i) {
            data[[i]] <= max(rv[[i]]) & data[[i]] >= min(rv[[i]])
          })
          data[Reduce("&", conditions),]
        })
        
        output$filters <- renderUI({
          plot_output_list <- lapply(vars, function(i) {
            tagList(
              plotOutput(
                paste0("plot", i),
                height = ifelse(height/length(vars) > 100, 100, height/length(vars)), 
                width = width*0.2, clickId = paste0("click", i)
              ),
              textOutput(paste0("text", i))
            )
          })
          do.call(tagList, plot_output_list)
        })
        
        output$main_plot <- renderPlot({
          require(ggplot2)
          require(ggthemes)
          p <- ggplot(main_data(), aes_string(x = x, fill = y)) + geom_density(alpha=.3) + theme_tufte()
          print(p)
        })
        
        rv <- reactiveValues()
        for (var in vars) {
      
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
      }
    )
  , ...)
}