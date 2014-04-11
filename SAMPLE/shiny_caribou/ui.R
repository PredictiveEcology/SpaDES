library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("SAMPLE ABM model of caribou on a landscape"),
  
  sidebarPanel(
    # number of agents
    sliderInput("N", "Number of caribou:", min=1, max=1000, value=100, step=1),

    # maximum simulation time
    sliderInput("tmax", "Maximum simulation time:", min=1, max=100, value=10, step=0.1)
  ),
  
  mainPanel(
#    textOutput("debug"),
#    plotOutput("mapPlot"),
     plotOutput("simPlot")
  )
))
