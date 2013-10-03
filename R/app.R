#' Interactive Plot
#' 
#' ...
#' 
#' @param data dataset
#' @param height height (app)
#' @param width width (app)
#' @param geom main plot geom function
#' @param ... see runApp()
#' 
#' @examples 
#' \dontrun{
#' iPlot(MASS::survey)
#' iPlot(mtcars, geom = geom_bar())
#' iPlot(as.data.table(ggplot2::diamonds), geom = geom_density(aes(position="stack")))
#' }
#' @export
iPlot <- function(
  data,
  height = 600,
  width = 800,
  geom = geom_density(alpha = .3),
  ...
){
  
  static <- iData(data)
  
  # Run app
  runApp(
    list(
      ui = bootstrapPage(
        includeCSS(system.file("css/custom.css", package="iPlot")),
        div(class="row",
          div(class="span2",
            uiOutput("num_filter")
          ),
          div(class="span8",
            chartOutput("main_plot", "highcharts"),
            uiOutput("count")
          ),
          div(class="span2",
            uiOutput("select_fill"),
            uiOutput("select_density"),
            uiOutput("cat_filter")
          )
        )
      ),
      server = function(input, output, session) {
        observe({
          print(input$test)
          
        })
        main_data <- reactive({
          
          num_conditions <- lapply(static$numerics, function(i) {
            static$data[[i]] <= max(rv[[i]]) & static$data[[i]] >= min(rv[[i]])
          })
          
          cat_conditions <- lapply(static$categories, function(i) {
              if(length(input[[paste0("menu", i)]]) > 0) {
                 static$data[[i]] %in% input[[paste0("menu", i)]]
              } else {
                 TRUE
              }
           })
          static$data[Reduce("&", c(num_conditions, cat_conditions)), ]
        })
        
        output$select_fill <- renderUI({
          bootstrapSelectInput(
            "fill",
            label = "Select fill variable:",
            choices = static$categories,
            liveSearch = T,
            subtext = rep("categorical", length(static$categories)),
            style = "btn-info"
          )
        })
        
         output$select_density <- renderUI({
           bootstrapSelectInput(
             "density",
             label = "Select density variable:",
             choices = static$numerics,
             liveSearch = T,
             subtext = rep("numerical", length(static$numerics)),
             style = "btn-info"
            )
         })
        
        output$num_filter <- renderUI({
          plot_output_list <- lapply(static$numerics, function(i) {
            tagList(
              plotOutput(
                paste0("plot", i),
                height = ifelse(height/length(static$numerics) > 100, 100, height/length(static$numerics)), 
                width = width*0.2, clickId = paste0("click", i)
              ),
              textOutput(paste0("text", i))
            )
          })
          
          do.call(tagList, plot_output_list)
        })
        
        output$cat_filter <- renderUI({
           selector_menu_list <- lapply(static$categories, function(i) {
              tbl <- table(static$data[[i]])
              tagList(
                 bootstrapSelectInput(
                   paste0("menu", i),
                   label = i,
                   choices = names(tbl),
                   selected = names(tbl),
                   multiple = T,
                   liveSearch = T,
                   subtext = tbl,
                   selectedTextFormat = "count"
                 )
              )
           })
           do.call(tagList, selector_menu_list)
        })
        
        output$count <- renderText({
          sprintf("Selected %s out of %s, whereas %s deleted because of missing values.",
            nrow(main_data()),
            nrow(static$data),
            static$removed_na
          )
        })
        
        output$main_plot <- renderChart({
          require(data.table)
          data <- main_data()
          data <- rbindlist(lapply(unique(data[[input$fill]]), 
            function(i) {
              d <- data[data[[input$fill]] == i, ][[input$density]]
              if (length(d) > 1) {
                data.table(x = density(d)$x, y = density(d)$y, i = i)
              } else NULL
            }
          ))
          p <- hPlot(x = "x", y = "y", data = data, type = "line", group = "i", radius = 1)
          p$xAxis(title = list(enabled = F), lineWidth = 0, minorTickLength = 0, tickLength = 0)
          p$yAxis(title = list(enabled = F), labels = list(enabled = F), lineWidth = 0, gridLineWidth = 0, minorTickLength = 0, tickLength = 0)
          p$legend(borderWidth = 0, verticalAlign = "top")
          p$addParams(dom = 'main_plot')
          return(p)
        })
        
        rv <- reactiveValues()
        for (var in static$numerics) {
      
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
              mini_plot(
                i,
                paste(format(rv[[i]], digits = 3), collapse = " - "),
                static$data[[i]],
                rv[[i]]
              )
            })
          })
        }
      }
    )
  , ...)
}
