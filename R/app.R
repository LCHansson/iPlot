# data <- na.omit(as.data.table(MASS::survey))
# vars = c("Pulse", "Age")

#' Interactive Plot
#' 
#' ...
#' 
#' @param data dataset
#' @param vars filter variables
#' @param ... see runApp()
#' 
#' @export
iplot <- function(data, vars, ...) {
  
  data <- na.omit(data)
  
  runApp(
    list(
      ui = pageWithSidebar(
        headerPanel(""),
        sidebarPanel(
          uiOutput("left_filter")
        ),
        mainPanel(
          plotOutput("main_plot", height = 600, width = 500)
        )
      ),
      server = function(input, output, session) {
        main_data <- reactive({
          data # TODO
        })
        
        output$left_filter <- renderUI({
          plot_output_list <- lapply(vars, function(i) {
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