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
  ...
){
  
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
          
          #### Numeric (graphic) filters ####
          div(
            class="span2",
            div(
              class="row",
              uiOutput("select_filters")
            ),
            div(
              class="row",
              uiOutput("filters"),
              uiOutput("cat_filter")
            )
          ),
          
          #### Graph window ####
          div(
            class="span8",
            div(
              class="row",
              uiOutput("select_method")
            ),
            div(
              class="row",
              plotOutput("main_plot")
            ),
            
            #### Table/numeric window ####
            div(
              class="row",
              uiOutput("select_analysis"),
              uiOutput("count")
            ),
            div(
              class="row",
              htmlOutput("analysis")
            )
          ),
          
          #### Discrete (text) filters ####
          div(
            class="span2",
            uiOutput("select_fill"),
            uiOutput("select_density")
#             uiOutput("cat_filter")
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
        
        
        #### Reactive UI components (in order or appearance in the UI code) ####

        output$select_filters <- renderUI({
          multiselectInput(
            "filter_sel",
            label = "Choose filters:",
            choices = c(static$numerics,static$categories),
            selected = c(static$numerics[1],static$categories[1]),
            multiple = T,
            options = list(
              buttonClass = "btn btn-link btn-core",
              includeSelectAllOption = T,
              enableFiltering = T
            )
          )
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
        
        
        # Top graph menu (this is where a lot of the conditional magic happens)
        
        output$select_method <- renderUI({
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
                  choices = static$categories,
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
        
        
        #### UI-static reactive components ####
        
        output$count <- renderText({
          sprintf("Selected %s out of %s, whereas %s deleted because of missing values.",
                  nrow(main_data()),
                  nrow(static$data),
                  static$removed_na
          )
        })
        
        output$main_plot <- renderPlot({
          data <- main_data()
          data[[input$fill]] <- as.factor(data[[input$fill]])
          
          
          if(input$method == "comp") {
            p <- ggplot(data, aes_string(x = input$density, fill = input$fill)) + geom + ggthemes::theme_tufte()
            print(p)
          }
          
          if(input$method == "regr") {
            p <- ggplot(data,aes_string(x = input$indepvar, y = input$depvar, color = input$fill)) + geom_point(alpha=.3) + ggthemes::theme_tufte()
            p <- p + geom_smooth(method = "lm", se=FALSE, linetype = 2, size = 1, color = "#5bc0de")
            print(p)
          }
          
        })
        
        output$select_analysis <- renderUI({
          tagList(
            div(
              class="span2",
              multiselectInput(
                "table",
                label = "Tables and measures:",
                choices = c(
                  Variables = "comp_table",
                  Regression = "regr_table"
                ),
                options = list(
                  buttonClass = "btn btn-link btn-core",
                  includeSelectAllOption = F,
                  enableFiltering = F
                )
              )
            )
          )
        })
        
        output$analysis <- renderUI({
          NULL
        })
        
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
      }
    )
    , ...)
}
