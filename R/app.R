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
        
        tags$script(type = "text/javascript", "
          $(function() { // Run when DOM ready
            $(window).bind('beforeunload', function(e) {
              Shiny.onInputChange('quit', true); // sets input$quit to true
            });
          });
        "),
        
        uiOutput("mainUI")
      ),
      
      ## SERVER ----------------------------------------------------------------
      server = function(input, output, session) {

        #### What is this? ping @reinholdsson ####
        observe({
          print(input$test)
        })
        
        
        #### Main UI grid ####
        
        output$mainUI <- renderUI({
          #### Define what components to include ####
          # (Actually, this should also include the "filters" and "buttons"
          # components. We'll implement that later...)
          components <- c("graph", "table")
          components <- components[components %in% names(options[options==TRUE])]
          
          #### Filters focus area ####
          filterUI <- div(
            class="span2",
            uiOutput("select_filters"),
            tags$hr(),
            uiOutput("filters"),
            uiOutput("cat_filter")
          )
          
          #### Graph and table focus areas #### 
          mainUI <- lapply(components, function(i) {
            tagList(
              div(
                class="row",
                uiOutput(paste0("select_",i))
              ),
              div(
                class="row",
                plotOutput(i),
                tags$hr()
              )
            )
          })
          mainUI <- div(
            class="span8",
            div(
              class="row",
              uiOutput("select_graph")
            ),
            div(
              class="row",
              plotOutput("graph"),
              tags$hr()
            ),
#             div(
#               class="row",
#               uiOutput("select_table")
#             ),
            div(
              class="row",
#               uiOutput("table"),
              div(class="span8",
                  dataTableOutput("var_list")
              )
            )
          )
          
          #### Right column buttons focus area ####
          buttonUI <- div(
            class="span2",
            div(
              class="row",
              div(class="span2",uiOutput("buttons"))
            )
          )
          
          div(class="row", 
              filterUI,
#               div(class="span8",mainUI),
              mainUI,
              buttonUI
          )
        })
        
        #### Reactive internals ####
        
        ## Main data
        main_data <- reactive({
          
          num_conditions <- lapply(reactive_nums(), function(i) {
            static$data[[i]] <= max(rv[[i]], na.rm = T) &
            static$data[[i]] >= min(rv[[i]], na.rm = T) |
            na_or_false(static$data[[i]], input[[paste0("na", i)]])
          })
          
          cat_conditions <- lapply(reactive_cats(), function(i) {
            if(length(input[[paste0("menu", i)]]) > 0) {
              static$data[[i]] %in% input[[paste0("menu", i)]] |
              na_or_false(static$data[[i]], input[[paste0("na", i)]])
            } else {
              TRUE
            }
          })
          data <- static$data[Reduce("&", c(num_conditions, cat_conditions)), ]

          if(is.null(input$sampleButton)) return(data)
          
          if(input$sampleButton %% 2 == 1) {
            if(nrow(data) > 50000) {
              return(data[sort(sample(nrow(data), 10000)),])
            } else {
              return(data[sort(sample(nrow(data), nrow(data) %/% 5)),])
            }
          }
          
          return(data)
          
        })
        
        ## Main plot
        main_plot <- reactive({
          
          # Get data
          data <- main_data()
          
          # Fill variable?
          if(input$fill != "None") {
            data[[input$fill]] <- as.factor(data[[input$fill]])
          }
          
          # Component analysis module
          if(input$method == "comp") {
            vars <- unique(c(input$density, input$fill))
            vars <- vars[vars != "None"]
            data <- na.omit(subset(data, select = vars))
            p <- ggplot(data, aes_string(x = input$density, fill = ifelse(input$fill != "None", input$fill, FALSE))) + 
              geom_density(alpha = ifelse(require(pmreports),0.7,0.3)) + 
              ggthemes::theme_tufte() +
              theme(axis.text.y=element_blank())
            
            if(input$line_coords != "") {
              x <- as.numeric(input$line_coords)
              p <- p + 
                geom_vline(xintercept=x, size=1, linetype=5, alpha=0.7) + 
                annotate(
                  "text",x=x, y=0, label=x,
                  size=5, angle=90, vjust=-0.2, hjust=0, color="gray10", alpha=0.8
                )
            }
          }
          
          if(input$method == "scatter") {
            vars <- unique(c(input$fill, input$indepvar, input$depvar))
            vars <- vars[vars != "None"]
            data <- na.omit(subset(data, select = vars))
            p <- ggplot(
              data, aes_string(
                x = input$indepvar,
                y = input$depvar,
                color = ifelse(input$fill != "None", input$fill, FALSE))
              ) +
              geom_point(alpha=ifelse(require(pmreports),0.7,0.3)) +
              ggthemes::theme_tufte()
            p <- p + geom_smooth(
              method = "lm", se=FALSE, linetype = 2, size = 1, color = "#5bc0de"
            )
          }
          
          if(input$method == 'facets') {
            vars <- unique(c(input$density, input$fill, input$xfacet, input$yfacet))
            vars <- vars[vars != "None"]
            data <- na.omit(subset(data, select = vars))
            p <- ggplot(data, aes_string(x = input$density, fill = ifelse(input$fill != "None", input$fill, FALSE))) + 
              facet_grid(paste(
                ifelse(input$yfacet != "None", input$yfacet, "."),
                "~",
                ifelse(input$xfacet != "None", input$xfacet, ".")
                )) +
              geom_density(alpha = ifelse(require(pmreports),0.7,0.3)) + 
              ggthemes::theme_tufte()
            
            if(input$line_coords != "") {
              x <- as.numeric(input$line_coords)
              p <- p + 
                geom_vline(xintercept=x, size=1, linetype=5, alpha=0.7) + 
                annotate(
                  "text",x=x, y=0, label=x,
                  size=5, angle=90, vjust=-0.2, hjust=0, color="gray10", alpha=0.8
                )
            }
          }
          
          # Add pmreports styling if it is installed
          if(require(pmreports)) {
            p <- style_plot(p, colors=pm_colors(), base_size=16) + aes(alpha=0.7)
          }
          
          # If no fill variable is selected, hide the legend
          if(input$fill == "None") {
            p <- p + theme(legend.position="none")
          }
          
          # Remove Y axis text if it is a density plot
          if(input$method %in% c("comp","facets")) {
            p <- p + theme(axis.text.y=element_blank())
          }
          
          # Write to console
          message(nrow(data), " observations used in plot.")
          
          return(p)
        })
        
        ## Quit function
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
            label = HTML("<i class=\"icon-filter\"></i> variables"),
            choices = choices,
            selected = c(static$numerics[1:2],static$categories[1:2]),
            multiple = T,
            options = list(
              buttonClass = "btn btn-link",
              includeSelectAllOption = T,
              enableFiltering = T,
              buttonText = sprintf("#! function(options, select) {return options.length + '/%s'}!#", length(choices))
            )
          )
        })
        
        output$cat_filter <- renderUI({
          selector_menu_list <- lapply(reactive_cats(), function(i) {
            tbl <- table(static$data[[i]])
            tagList(
              div(class = "var",
                div(class = "var-text",
                  multiselectInput(
                    paste0("menu", i),
                    #label = HTML("<i class=\"icon-filter\"></i>"),
                    label = "",
                    choices = names(tbl),
                    selected = names(tbl),
                    multiple = T,
                    options = list(
                      buttonClass = "btn btn-link",
                      includeSelectAllOption = T,
                      enableFiltering = T,
                      buttonText = sprintf("#! function(options, select) {return '%s ' + options.length + '/%s'}!#", i, length(tbl))
                    )
                )),
                bootstrapCheckbox(paste0("na", i), "", value = T, options = list(
                  buttonStyle = "btn-link btn-small",
                  checkedClass = "icon-ok",
                  uncheckedClass = "icon-remove",
                  checked = T
              )))
            )
          })
          do.call(tagList, selector_menu_list)
        })
        
        output$filters <- renderUI({
          plot_output_list <- lapply(reactive_nums(), function(i) {
            tagList(div(class = "var",
              div(class = "var-text", HTML(i)),
              bootstrapCheckbox(paste0("na", i), "", value = T, options = list(
                buttonStyle = "btn-link btn-small",
                checkedClass = "icon-ok",
                uncheckedClass = "icon-remove",
                defaultState = T
              ))),
              plotOutput(
                paste0("plot", i),
                height = ifelse(height/length(static$numerics) > 100, 100, height/length(static$numerics))/2, 
                width = width*0.2, clickId = paste0("click", i)
              )
            )
          })
          
          do.call(tagList, plot_output_list)
        })
        
        
        #### GRAPH focus area ####
        
        output$select_graph <- renderUI({
          if(options$graph == FALSE) return()
          
          tagList(
            div(
              class="span2",
              multiselectInput(
                "method",
                label = HTML("<i class=\"icon-signal\"></i> graph"),
                choices = c(
                  Composition = "comp",
                  Scatter = "scatter",
                  Facets = "facets"
                ),
                options = list(
                  buttonClass = "btn btn-link",
                  includeSelectAllOption = F,
                  enableFiltering = F
                )
              )
            ),
            
            ## Shared graph menus
            conditionalPanel(
              "input.method == 'comp' | input.method == 'scatter' | input.method == 'facets'",
              div(
                class="span2",
                multiselectInput(
                  "fill",
                  label = HTML("<i class=\"icon-tint\"></i> fill"),
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
              "input.method == 'comp' | input.method == 'facets'",
              div(
                class="span2",
                multiselectInput(
                  "density",
                  label = HTML("<i class=\"icon-certificate\"></i> density"),
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
              "input.method == 'comp' | input.method == 'facets'",
              div(
                class="span2",
                textInput2("line_coords", HTML("<i class=\"icon-indent-right\"></i> line"),"",class="input-small")
              )
            ),
            
            ## Regression graph menus
            conditionalPanel(
              "input.method == 'scatter'",
              div(
                class="span2",
                multiselectInput(
                  "indepvar",
                  label = HTML("<i class=\"icon-resize-horizontal\"></i> x"),
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
              "input.method == 'scatter'",
              div(
                class="span2",
                multiselectInput(
                  "depvar",
                  label = HTML("<i class=\"icon-resize-vertical\"></i> y"),
                  choices = c(static$numerics,static$categories),
                  selected = c(static$numerics,static$categories)[2],
                  options = list(
                    buttonClass = "btn btn-link",
                    includeSelectAllOption = T,
                    enableFiltering = T
                  )
                )
              )
            ),
            conditionalPanel(
              "input.method == 'facets'",
              div(
                class="span2",
                multiselectInput(
                  "xfacet",
                  label = HTML("<i class=\"icon-th\"></i> x"),
                  choices = c("None",static$categories),
                  selected = static$categories[1],
                  options = list(
                    buttonClass = "btn btn-link",
                    includeSelectAllOption = T,
                    enableFiltering = T
                  )
                )
              )
            ),
            conditionalPanel(
              "input.method == 'facets'",
              div(
                class="span2",
                multiselectInput(
                  "yfacet",
                  label = HTML("<i class=\"icon-th\"></i> y"),
                  choices = c("None",static$categories),
                  selected = static$categories[2],
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

        ## Plot output
        output$graph <- renderPlot({
          # Don't draw graph if the graph option is set to FALSE
          if(options$graph == FALSE) return()
          
          # Do nothing if the UI components have not yet been defined
          if(is.null(input$method)) return()
          
          p <- main_plot()
          
          print(p)
          
        })
        
        
        #### TABLE focus area ####
        
        output$table <- renderUI({
#           uiOutput("data_view")
          dataTableOutput("var_list")
        })

        output$data_view <- renderUI({
          div(
            class="span8",
            dataTableOutput("var_list")
          )
        })
        
        output$var_list <- renderDataTable({
          
          data <- data.table(main_data())
          data <- data[, names(data)[names(data) %in% input$filter_sel], with=F]
          
          ## Create a table with analytical measures (as defined in stat_names above)
          comp_table <- data.table(sapply(stat_names, function(i) {
            sapply(data[, names(data)[names(data) %in% static$numerics], with=F], function(x,i) {
              format(
                eval(parse(text=i)),
                digits=4
              )
            }, i)
          }))

          ## Add "Name" column to the front of the table
          comp_table$Name <- names(data)[names(data) %in% static$numerics]
          comp_table <- comp_table[,c(ncol(comp_table),1:ncol(comp_table)-1)]
          
          # Print the table
          return(comp_table)
        }, options = list(aLengthMenu = c(5, 10, 25, 100), iDisplayLength = 5, bLengthChange = FALSE))
        
        
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
        
        # Still buggy! Plot keeps coordinates somehow?
        observe({
          non_sel <- static$numerics[!static$numerics %in% input$filter_sel]
          lapply(non_sel, function(i) {
            rv[[i]] <- c(min(static$data[[i]], na.rm = T), max(static$data[[i]]), na.rm = T)
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
                  max(density(static$data[[i]], na.rm = T)$y)/2
                )
              })
            })
            
            local({
              i <- var
              
              output[[paste0("plot", i)]] <- renderPlot({
                mini_plot(
                  i,
                  paste(format(rv[[i]], digits = 3), collapse = " - "),
                  na.omit(static$data[[i]]),
                  rv[[i]],
                  na.omit(main_data()[[i]])
                )
              })
            })
          }
        })
        
        #### RIGHT COLUMN focus area ####
        output$buttons <- renderUI({
          tagList(
            downloadButton("dlData", HTML("<i class=\"icon-download\"></i>"), "btn btn-link"), br(),
            downloadButton("dlGraph", HTML("<i class=\"icon-eye-open\"></i>"), "btn btn-link"), br(),
            bootstrapCheckbox("sampleButton", "", options = list(checkedClass = "icon-ok-sign", uncheckedClass = "icon-fast-forward")),
            actionButton2("quit", HTML("<i class=\"icon-off\"></i>"), "btn action-button btn-link")
          )
        })
        
        ## Download data button
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
        
        ## Graph download button
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
