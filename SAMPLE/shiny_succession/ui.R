shinyUI(fluidPage(
  titlePanel("A simple forest succession model."),

  sidebarLayout(
    sidebarPanel(
                 sliderInput('stopTime', 'Year to predict',
                             min=2006, max=2040, value=2008, step=1, round=FALSE,
                             format = "###0"),

                 br(),

                 h3("Simulation modules"),
                 checkboxInput('fireModule', 'Forest fires'),

                 sliderInput('drought', 'Drought intensity',
                            min=0.8, max=1.2, value=1, step=0.05, round=FALSE)
    ),

    mainPanel(
      plotOutput('mapsInit'),
      plotOutput('maps'),
      plotOutput('fireHist', xlab="Area burned (ha)")
    )
  )
))
