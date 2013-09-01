#' Interactive Plot
#' 
#' ...
#' 
#' @param data dataset
#' @param vars filter variables
#' @param ... see runApp()
#' 
#' @examples 
#' iplot("Pulse", "Age", MASS::survey, c("Pulse", "Age"))
#' 
#' @export
iplot <- function(x, y, data, vars, ...) {
  
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
      ui = pageWithSidebar(
        headerPanel(""),
        sidebarPanel(
          uiOutput("filters")
        ),
        mainPanel(
          plotOutput("main_plot", height = 600, width = 800)
        )
      ),
      server = function(input, output, session) {
        main_data <- reactive({
          conditions <- lapply(vars, function(i) {
            data[[i]] <= max(rv[[i]]) & data[[i]] >= min(rv[[i]])
          })
          data[do.call("&", conditions),]
        })
        
        output$filters <- renderUI({
          plot_output_list <- lapply(vars, function(i) {
            tagList(
              plotOutput(paste0("plot", i), height = 250, width = 350, clickId = paste0("click", i)),
              textOutput(paste0("text", i))
            )
          })
          do.call(tagList, plot_output_list)
        })
      
        output$main_plot <- renderPlot({
          plot(x = main_data()[[x]], y = main_data()[[y]], xlab = x, ylab = y)
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