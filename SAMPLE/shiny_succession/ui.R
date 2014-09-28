shinyUI(fluidPage(
  titlePanel("A simple forest succession model."),

  sidebarLayout(
    sidebarPanel(
                 sliderInput('stopTime', 'Year to predict',
                             min=2006, max=2050, value=2008, step=1, round=FALSE,
                             format = "###0"),

                 br(),

                 h3("Simulation modules"),
                 checkboxInput('successionModule', 'Forest succession', value=TRUE),
                 checkboxInput('fireModule', 'Forest fires', value=TRUE),
                 checkboxInput('caribouModule', 'Caribou', value=TRUE),

                 checkboxInput('carbonModule', 'Carbon', value=FALSE),

                 sliderInput('drought', 'Drought intensity',
                            min=0.8, max=1.2, value=1, step=0.05, round=FALSE)
    ),

    mainPanel(
      plotOutput('mapsInit'),
      plotOutput('maps'),
      plotOutput('initHists', height="200px"),
      plotOutput('endHists'),
      plotOutput('caribouMaps')
    )
  )
))
