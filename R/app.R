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
  require(rCharts)
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
            uiOutput("select_fill"),
            uiOutput("select_density"),
            chartOutput("main_plot", "highcharts"), #, height = height, width = width*0.8)
            uiOutput("count")
          ),
          div(class="span2",
            uiOutput("cat_filter")
          )
        )
      ),
      server = function(input, output, session) {
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
          cats <- get_vars(static$categories, "categorical")
          nums <- get_vars(static$numerics, "numerical")
          vars <- c(cats, nums)
          select2input("fill", label = "Select fill variable:", choices = vars)
        })
        
         output$select_density <- renderUI({
           select2input(
             "density",
             label = "Select density variable:",
             choices = get_vars(static$numerics, "numerical")
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
              choice_lst = names(tbl)
              names(choice_lst) <- sprintf("%s (%s)", choice_lst, tbl)
              tagList(
                 select2input(paste0("menu",i), label = i, choices = choice_lst, multiple = TRUE, options = list(placeholder = "select ..."))
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
          data <- rbindlist(lapply(unique(data[[input$fill]]), 
            function(i) {
              d <- data[data[[input$fill]] == i, ][[input$density]]
              if (length(d) > 1) {
                data.table(x = density(d)$x, y = density(d)$y, i = i)
              } else NULL
            }
          ))
          p <- hPlot(x = "x", y = "y", data = data, type = "line", group = "i", radius = 1)
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
                paste(format(rv[[i]], digits = 3), collapse = "-"),
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

get_vars <- function(vars, type) {
  names(vars) <- sprintf("%s (%s)", vars, type)
  return(vars)
}
          