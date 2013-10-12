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
  data = ggplot2::diamonds,
  height = 600,
  width = 800,
  geom = geom_density(alpha = .3),
  liveSearchLimit = 7,
  options = list(),
  ...
){
  options <- defaultOptions(options)
  
  if(class(data) != "iData") {
    static <- iData(data) 
  } else {
    static <- copy(data)
  }
  
  graphTypes = c("density", "scatter")
  
  # Run app
  runApp(
    list(
      
      ## UI --------------------------------------------------------------------
      ui = bootstrapPage(
#         includeCSS(system.file("css/custom.css", package="iPlot")),
        includeCSS("inst/css/custom.css"),
        div(
          class="row",
          
          #### Filters focus area ####
          div(
            class="span2",
            div(
              class="row",
              uiOutput("select_filters")
            ),
            div(
              class="row",
              uiOutput("filters"),
              tags$hr(),
              uiOutput("cat_filter")
            )
          ),
          
          #### Graph focus area ####
          div(
            class="span8",
            div(
              class="row",
              uiOutput("select_method")
            ),
            div(
              class="row",
              plotOutput("main_plot"),
              tags$hr()
            ),
            
            #### Table/numeric focus area ####
            div(
              class="row",
              uiOutput("select_analysis")
            ),
            div(
              class="row",
              uiOutput("analysis")
            )
          ),
          
          #### Right column focus area ####
          div(
            class="span2",
            div(
              class="row",
              div(class="span2",uiOutput("buttons"))
            )
          )
        )
      ),
      
      ## SERVER ----------------------------------------------------------------
      server = function(input, output, session) {

        observe({
          print(input$test)
        })
        
        
        #### Reactive internals ####
        
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
        
        ## Quit button
        observe({
          if(is.null(input$quit)) return()
          if(input$quit == 0) return()
          input$quit
          
          stopApp()
        })
        
        
        #### FILTERS focus area ####

        output$select_filters <- renderUI({
          multiselectInput(
            "filter_sel",
            label = "Choose filters:",
            choices = c(static$numerics,static$categories),
            selected = c(static$numerics[1:2],static$categories[1:2]),
            multiple = T,
            options = list(
              buttonClass = "btn btn-link btn-core",
              includeSelectAllOption = T,
              enableFiltering = T
            )
          )
        })
        
        output$cat_filter <- renderUI({
          cat_vars <- static$categories[static$categories %in% input$filter_sel]
          
          selector_menu_list <- lapply(cat_vars, function(i) {
            tbl <- table(static$data[[i]])
            tagList(
              multiselectInput(
                paste0("menu", i),
                label = i,
                choices = names(tbl),
                selected = names(tbl),
                multiple = T,
                options = list(
                  buttonClass = "btn btn-link",
                  includeSelectAllOption = T,
                  enableFiltering = T
                )
              )
            )
          })
          do.call(tagList, selector_menu_list)
        })
        
        output$filters <- renderUI({
          plot_vars <- static$numerics[static$numerics %in% input$filter_sel]
          
          plot_output_list <- lapply(plot_vars, function(i) {
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
        
        
        #### GRAPH focus area ####
        
        output$select_method <- renderUI({
          if(options$graph == FALSE) return()
          
          tagList(
            div(
              class="span2",
              multiselectInput(
                "method",
                label = "Analysis method:",
                choices = c(
                  Composition = "comp",
                  Regression = "regr"
                ),
                options = list(
                  buttonClass = "btn btn-link btn-core",
                  includeSelectAllOption = F,
                  enableFiltering = F
                )
              )
            ),
            conditionalPanel(
              "input.method == 'comp' | input.method == 'regr'",
              div(
                class="span2",
                multiselectInput(
                  "fill",
                  label = "Select fill variable:",
                  choices = c("None",static$categories),
                  options = list(
                    buttonClass = "btn btn-link",
                    includeSelectAllOption = T,
                    enableFiltering = T
                  )
                )
              )
            ),
            conditionalPanel(
              "input.method == 'comp'",
              div(
                class="span2",
                multiselectInput(
                  "density",
                  label = "Select density variable:",
                  choices = static$numerics,
                  options = list(
                    buttonClass = "btn btn-link",
                    includeSelectAllOption = T,
                    enableFiltering = T
                  )
                )
              )
            ),
            conditionalPanel(
              "input.method == 'regr'",
              div(
                class="span2",
                multiselectInput(
                  "indepvar",
                  label = "X axis (independent)",
                  choices = c(static$numerics,static$categories),
                  selected = c(static$numerics,static$categories)[1],
                  options = list(
                    buttonClass = "btn btn-link",
                    includeSelectAllOption = T,
                    enableFiltering = T
                  )
                )
              )
            ),
            conditionalPanel(
              "input.method == 'regr'",
              div(
                class="span2",
                multiselectInput(
                  "depvar",
                  label = "Y axis (dependent)",
                  choices = c(static$numerics,static$categories),
                  selected = c(static$numerics,static$categories)[2],
                  options = list(
                    buttonClass = "btn btn-link",
                    includeSelectAllOption = T,
                    enableFiltering = T
                  )
                )
              )
            )
          )
        })

        output$main_plot <- renderPlot({
          if(options$graph == FALSE) return()
          
          data <- main_data()
          
          # Do nothing if the UI components have not yet been defined
          if(is.null(input$method)) return()
          
          if(input$fill != "None") {
            data[[input$fill]] <- as.factor(data[[input$fill]])
          }
          
          if(input$method == "comp") {
            p <- ggplot(data, aes_string(x = input$density, fill = ifelse(input$fill != "None", input$fill, 1))) + 
              geom + 
              ggthemes::theme_tufte()
            
            print(p)
          }
          
          if(input$method == "regr") {
            p <- ggplot(data,aes_string(x = input$indepvar, y = input$depvar, color = ifelse(input$fill != "None", input$fill, 1))) +
              geom_point(alpha=.3) +
              ggthemes::theme_tufte()
            p <- p + geom_smooth(method = "lm", se=FALSE, linetype = 2, size = 1, color = "#5bc0de")
            
            print(p)
          }
          
        })
        
        
        #### TABLE focus area ####
        
        output$select_analysis <- renderUI({
          if(options$table == FALSE) return()
          
          tagList(
            div(
              class="span2",
              multiselectInput(
                "text_sel",
                label = "Tables and measures:",
                choices = c(
                  Variables = "data_view",
                  Regression = "regr_table"
                ),
                selected = "Variables",
                options = list(
                  buttonClass = "btn btn-link btn-core",
                  includeSelectAllOption = F,
                  enableFiltering = F
                )
              )
            ),
            conditionalPanel(
              "input.text_sel == 'data_view'",
              div(
                class="span2",
                multiselectInput(
                  "view_vars",
                  label = "List variables",
                  choices = c(static$numerics,static$categories),
                  selected = c(static$numerics,static$categories)[1:2],
                  multiple = T,
                  options = list(
                    buttonClass = "btn btn-link",
                    includeSelectAllOption = T,
                    enableFiltering = T
                  )
                )
              )
            )
          )
        })

        output$analysis <- renderUI({
          if(options$table == FALSE) return()
          
          tagList(
            div(class="span8",uiOutput("count")),
            uiOutput(outputId = input$text_sel)
          )
        })

        output$data_view <- renderUI({
          data <- main_data()
          
          list_conditions <- lapply(input$view_vars, function(i) {
            tags$ul(
              class="list-inline",
              tags$li(i),
              tags$li(format(mean(data[[i]]),digits=2)),
              tags$li(format(sd(data[[i]]),digits=2)),
              tags$li(format(quantile(data[[i]],0.05),digits=2)),
              tags$li(format(quantile(data[[i]],0.25),digits=2)),
              tags$li(format(quantile(data[[i]],0.75),digits=2)),
              tags$li(format(quantile(data[[i]],0.95),digits=2))
            )
          })
          
         do.call(tagList, list_conditions)
        })
        
        output$count <- renderText({
          sprintf("Selected %s out of %s, whereas %s deleted because of missing values.",
                  nrow(main_data()),
                  nrow(static$data),
                  static$removed_na
          )
        })
        
        #### Regression model functions ####
        make_model <- function(model_type, formula, ...) {
          # The code for this function is recycled from the following Shiny Showcase example:
          # https://gist.github.com/wch/4034323
          # http://glimmer.rstudio.com/winston/heightweight/
          
          # Get the subset of the data limited by the specified range
          hw_sub <- limit_data_range()
          if (is.null(hw_sub))
            return()
          
          # In order to get the output to print the formula in a nice way, we'll
          # use do.call here with some quoting.
          do.call(model_type, args = list(formula = formula, data = quote(hw_sub), ...))
        }
        
        ## Thomas: PLEASE add inline documentation of the following code!
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
        
        #### RIGHT COLUMN focus area ####
        output$buttons <- renderUI({
          tagList(
            downloadButton("dlData","Download data", "btn-primary btn-small btn-block btn-rmenu"),
            downloadButton("dlGraph","Save graph", "btn-primary btn-small btn-block btn-rmenu"),
            actionButton2("options", "Advanced settings","btn action-button btn-primary btn-small btn-block btn-rmenu"),
            actionButton2("quit","Quit iPlot","btn action-button btn-primary btn-small btn-block btn-rmenu")
          )
        })
        
        output$dlData <- downloadHandler(
          filename = function() {
            if(require(XLConnect)) {
              "test.xlsx" 
            } else {
              "test.csv"
            }
          },
          content = function(con) {
            temp_file <- paste(tempfile(), "test.xlsx", sep = "_")
            on.exit(unlink(temp_file))
            xlfun <- function(input, output) {
              nrows <- nrow(main_data())
              limit <- 10000
              print(nrows)
              
              if(nrows > limit) stop(sprintf("Too many rows in data for memory to handle! Your data contains %s rows and the limit is set to %s", nrows, limit))
              
              if(!require(XLConnect)) {
                warning("Could not find package 'XLConnect'. Exporting data to CSV instead of XLS. Please run install.packages('XLConnect') to enable export to XLS.")
                
                write.csv(main_data(),file=output)
              } else {
                wb <- loadWorkbook(output, create = TRUE)
                createSheet(wb, name = "output")
                writeWorksheet(wb, input, sheet = "output")
                saveWorkbook(wb)
              }
            }
            xlfun(main_data(), temp_file)
            bytes <- readBin(temp_file, "raw", file.info(temp_file)$size)
            writeBin(bytes, con)

          }
        )
        
        output$dlGraph <- downloadHandler(
          filename = function() "test.pdf",
          content = function(con) {
            
            stop("Graph export not implemented yet!")
            
#             temp_file <- paste(tempfile(), "test.pdf", sep = "_")
#             on.exit(unlink(temp_file))
#             pngfun <- function(input, output) {
#               pdf(output)
#               generateThePlot()
#               dev.off()
#             }
#             pngfun(last_plot(), temp_file)
#             bytes <- readBin(temp_file, "raw", file.info(temp_file)$size)
#             writeBin(bytes, con)
          }
        )

      }
    )
    , ...)
}
