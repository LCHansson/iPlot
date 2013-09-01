shinyUI(pageWithSidebar(
  headerPanel(""),
  sidebarPanel(
    uiOutput("left_filter")
  ),
  mainPanel(
    plotOutput("main_plot", height = 600, width = 500)
  )
))
