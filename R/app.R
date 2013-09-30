#' Interactive Plot
#' 
#' ...
#' 
#' @param x variable
#' @paran fill fill variable
#' @param data dataset
#' @param vars filter variables
#' @param height height (app)
#' @param width width (app)
#' @param geom main plot geom function
#' @param ... see runApp()
#' 
#' @examples 
#' \dontrun{
#' iPlot(x = "Pulse", fill = "Exer", data = MASS::survey)
#' iPlot(x = "Pulse", fill = "Smoke", data = MASS::survey, vars = c("Height", "NW.Hnd", "Wr.Hnd"))
#' iPlot(x = "Height", fill = "Exer", data = MASS::survey, geom = geom_bar())
#' }
#' @export
iPlot <- function(
  x = names(data)[1],
  fill = NULL,
  data,
  height = 600,
  width = 800,
  geom = geom_density(alpha = .3),
  ...
){
  
  static <- iData(data)
  vars = static$numerics
  
  # Run app
  runApp(
    list(
      ui = bootstrapPage(
        HTML("<table><tr><td colspan=2>"),
        uiOutput("count"),
        HTML("</td></tr><tr><td>"),
        uiOutput("plot_filters"),
        HTML("</td><td>"),
        plotOutput("main_plot", height = height, width = width*0.8),
        HTML("</td></tr></table>")
      ),
      server = function(input, output, session) {
        main_data <- reactive({
          conditions <- lapply(vars, function(i) {
            static$data[[i]] <= max(rv[[i]]) & static$data[[i]] >= min(rv[[i]])
          })
          static$data[Reduce("&", conditions),]
        })
        
        output$plot_filters <- renderUI({
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
        
        output$count <- renderText({
          sprintf("Selected %s out of %s, whereas %s deleted because of missing values.",
            nrow(main_data()),
            nrow(static$data),
            static$removed_na
          )
        })
        
        output$main_plot <- renderPlot({
          p <- ggplot(main_data(), aes_string(x = x, fill = fill)) + geom + theme_bw()
          print(p)
        })
        
        rv <- reactiveValues()
        for (var in vars) {
      
          local({
            i <- var
            rv[[i]] <- c(min(static$data[[i]], na.rm = T), max(static$data[[i]], na.rm = T))
              
            observe({
              rv[[i]] <- setInput(
                rv[[i]],
                input[[paste0("click", i)]],
                max(density(static$data[[i]])$y)/2
              )
            })
          })
          
          local({
            i <- var
            
            output[[paste0("plot", i)]] <- renderPlot({
              mini_plot(i, static$data[[i]], rv[[i]])
            })
          })
        }
      }
    )
  , ...)
}