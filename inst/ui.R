shinyUI(bootstrapPage(
  ## CSS
  includeCSS(system.file("css/custom.css", package="iPlot")),

  ## UI
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
        plotOutput("main_plot")
      ),
      
      tags$hr(),
      
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
))