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
  geom = NULL,
  liveSearchLimit = 7,
  options = list(),
  ...
){
  
  if(!is.null(geom)) warning("Parameter 'geom' is deprecated. Please stop using it.")
  
  options <- defaultOptions(options)
  
  if(class(data) != "iData") {
    static <- iData(data) 
  } else {
    static <- copy(data)
  }
  
  ## FIX: THIS SHOULD BE AN INPUT PARAMETER
  graphTypes = c("density", "scatter")
  
  ## FIX: THESE NAMES SHOULD BE STORED IN A MODULE OR GLOBALS.R OR SOMETHING SIMILAR
  stat_names <- c(
    Mean = "mean(x,na.rm=T)",
    Median = "median(x,na.rm=T)",
    Stddev = "sd(x,na.rm=T)",
    Q05 = "quantile(x,0.05,na.rm=T)",
    Q10 = "quantile(x,0.10,na.rm=T)",
    Q25 = "quantile(x,0.25,na.rm=T)",
    Q75 = "quantile(x,0.75,na.rm=T)",
    Q90 = "quantile(x,0.90,na.rm=T)",
    Q95 = "quantile(x,0.95,na.rm=T)"
  )
  
  # Run app
  runApp(
    list(
      
      ## UI --------------------------------------------------------------------
      ui = bootstrapPage(
        includeCSS(system.file("css/custom.css", package="iPlot")),
#         includeCSS("inst/css/custom.css"),
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
              plotOutput("graph"),
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
          
          num_conditions <- lapply(reactive_nums(), function(i) {
            static$data[[i]] <= max(rv[[i]]) & static$data[[i]] >= min(rv[[i]])
          })
          
          cat_conditions <- lapply(reactive_cats(), function(i) {
            if(length(input[[paste0("menu", i)]]) > 0) {
              static$data[[i]] %in% input[[paste0("menu", i)]]
            } else {
              TRUE
            }
          })
          static$data[Reduce("&", c(num_conditions, cat_conditions)), ]
        })
        
        main_plot <- reactive({
          data <- main_data()
          
          if(input$fill != "None") {
            data[[input$fill]] <- as.factor(data[[input$fill]])
          }
          
          if(input$method == "comp") {
            p <- ggplot(data, aes_string(x = input$density, fill = ifelse(input$fill != "None", input$fill, FALSE))) + 
              geom_density(alpha = ifelse(require(pmreports),0.7,0.3)) + 
              ggthemes::theme_tufte()
            
#             browser()
            if(input$line_coords != "") {
              x <- as.numeric(input$line_coords)
              p <- p + 
                geom_vline(xintercept=x, size=1, linetype=5, alpha=0.7) + 
                annotate("text",x=x, y=0, label=x, size=5, angle=90, vjust=-0.2, hjust=0, color="gray10", alpha=0.8)
            }
          }
          
          if(input$method == "regr") {
            p <- ggplot(data,aes_string(x = input$indepvar, y = input$depvar, color = ifelse(input$fill != "None", input$fill, FALSE))) +
              geom_point(alpha=ifelse(require(pmreports),0.7,0.3)) +
              ggthemes::theme_tufte()
            p <- p + geom_smooth(method = "lm", se=FALSE, linetype = 2, size = 1, color = "#5bc0de")
          }
          
          # Add pmreports styling if it is installed
          if(require(pmreports)) {
            p <- style_plot(p, colors=pm_colors(), base_size=16) + aes(alpha=0.7)
          }
          
          # If no fill variable is selected, hide the legend
          if(input$fill == "None") {
            p <- p + theme(legend.position="none")
          }
          
          return(p)
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
          choices <- c(static$numerics,static$categories)
          multiselectInput(
            "filter_sel",
            label = "",
            choices = choices,
            selected = c(static$numerics[1:2],static$categories[1:2]),
            multiple = T,
            options = list(
              buttonClass = "btn btn-link btn-core",
              includeSelectAllOption = T,
              enableFiltering = T,
              buttonText = sprintf("#! function(options, select) {return 'Variables (' + options.length + '/%s)'}!#", length(choices))
            )
          )
        })
        
        output$cat_filter <- renderUI({
          selector_menu_list <- lapply(reactive_cats(), function(i) {
            tbl <- table(static$data[[i]])
            tagList(
              multiselectInput(
                paste0("menu", i),
                label = "",
                choices = names(tbl),
                selected = names(tbl),
                multiple = T,
                options = list(
                  buttonClass = "btn btn-link",
                  includeSelectAllOption = T,
                  enableFiltering = T,
                  buttonText = sprintf("#! function(options, select) {return '%s (' + options.length + '/%s)'}!#", i, length(tbl))
                )
              )
            )
          })
          do.call(tagList, selector_menu_list)
        })
        
        output$filters <- renderUI({
          plot_output_list <- lapply(reactive_nums(), function(i) {
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
                label = "",
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
            
            ## Shared graph menus
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
            
            ## Composition graph menus
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
              "input.method == 'comp'",
              div(
                class="span2",
                textInput2("line_coords", "Draw a line at","",class="input-small")
              )
            ),
            
            ## Regression graph menus
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

        output$graph <- renderPlot({
          if(options$graph == FALSE) return()
          
          # Do nothing if the UI components have not yet been defined
          if(is.null(input$method)) return()
          
          p <- main_plot()
          
          print(p)
          
        })
        
        
        #### TABLE focus area ####
        
        output$select_analysis <- renderUI({
          if(options$table == FALSE) return()
          
          tagList(
            div(
              class="span2",
              multiselectInput(
                "text_sel",
                label = "",
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
            ),
            conditionalPanel(
              "input.text_sel == 'data_view'",
              div(
                class="span2",
                multiselectInput(
                  "stat_properties",
                  label = "Statistical properties",
                  choices = stat_names,
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
          div(
            class="span8",
            tableOutput("var_list")
          )
        })
        
        output$var_list <- renderTable({
          require(data.table)
          if(!require(xtable)) return()
          if(is.null(input$stat_properties)) return()
          if(is.null(input$view_vars)) return()

          
          data <- data.table(main_data())
          
          
          ## ERROR: When only one variable is selected, the data.table below
          ## does not behave normally and returns a vecor instead of a one-
          ## column data.table. This distorts the algorithm!
          comp_table <- data.table(sapply(input$stat_properties, function(i) {
            sapply(data[,input$view_vars,with=F], function(x,i) {
              if(i == "multiselect-all") return(0)
              eval(parse(text=i))
            }, i)
          }))

#           browser()
          
          # Remove the multiselect-all artifact and rename columns for output
          if(length(input$stat_properties) > 1) {
            comp_table <- comp_table[,names(comp_table) != "multiselect-all",with=F]
          }
          setnames(comp_table,names(comp_table), names(stat_names[stat_names %in% input$stat_properties]))
          row.names(comp_table) <- input$view_vars
          
#           browser()
          
          # Print the table
          return(comp_table)
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
        
        # Using reactive values to store the click coordinates from
        # the filter plots
        rv <- reactiveValues()
        
        # Temp fix, clear reactive values
        # Still buggy! Plot keeps coordinates somehow?
        observe({
          non_sel <- static$numerics[!static$numerics %in% input$filter_sel]
          lapply(non_sel, function(i) {
            rv[[i]] <- c(min(static$data[[i]]), max(static$data[[i]]))
          })
        })
        
        # Reactive function that returns the selected numerical variables
        reactive_nums <- reactive({
          static$numerics[static$numerics %in% input$filter_sel]
        })
        
        # Reactive function that returns the selected categorical variables
        reactive_cats <- reactive({
          static$categories[static$categories %in% input$filter_sel]
        })
        
        observe({
          # Create a small filter plot for each selected numerical variable
          for (var in reactive_nums()) {
            
            # Need to use local, see http://stackoverflow.com/questions/15875786
            # Thanks Rahul Savani!
            local({
              i <- var
              rv[[i]] <- c(min(static$data[[i]], na.rm = T), max(static$data[[i]],
                na.rm = T))
              
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
                  rv[[i]],
                  main_data()[[i]]
                )
              })
            })
          }
        })
        
        #### RIGHT COLUMN focus area ####
        output$buttons <- renderUI({
          tagList(
            downloadButton("dlData","Download data", "btn-primary btn-small btn-block btn-rmenu"),
            downloadButton("dlGraph","Save graph", "btn-primary btn-small btn-block btn-rmenu"),
#             actionButton2("options", "Advanced settings","btn action-button btn-primary btn-small btn-block btn-rmenu"),
            actionButton2("quit","Quit iPlot","btn action-button btn-primary btn-small btn-block btn-rmenu")
          )
        })
        
        output$dlData <- downloadHandler(
          filename = function() {
            if("XLConnect" %in% rownames(installed.packages())) {
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
                        
            temp_file <- paste(tempfile(), "test.pdf", sep = "_")
            on.exit(unlink(temp_file))
            pngfun <- function(input, output) {
              # Make a PDF of size A4
              pdf(output,width=11.7,height=8.3)
              p <- main_plot()
              print(p)
              dev.off()
            }
            pngfun(last_plot(), temp_file)
            bytes <- readBin(temp_file, "raw", file.info(temp_file)$size)
            writeBin(bytes, con)
          }
        )
      }
    )
    , ...)
}
