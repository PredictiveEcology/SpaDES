shinyUI(fluidPage(
  titlePanel("A simple forest succession model."),

  sidebarLayout(
    sidebarPanel("Parameters",
                 sliderInput('time', 'Number of years',
                             min=0, max=250, value=20, step=10, round=0),

                 br(),

                 checkboxInput('fires', 'Forest fires'),

                 sliderInput('time', 'Number of years',
                             min=0, max=250, value=20, step=10, round=0),

                 sliderInput('time', 'Number of years',
                             min=0, max=250, value=20, step=10, round=0)
    ),

    mainPanel("main panel")
  )
))
