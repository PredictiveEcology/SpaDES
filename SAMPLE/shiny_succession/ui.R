shinyUI(fluidPage(
  titlePanel("A simple forest succession model."),

  sidebarLayout(
    sidebarPanel(
      plotOutput('plot'),

      hr(),

      fluidRow(
        column(3,
               h4("Parameters"),

               sliderInput('time', 'Number of years',
                           min=0, max=250, value=20, step=10, round=0),

               br(),

               checkboxInput('fires', 'Forest fires')
        ),
        column(4, offset=1,
               sliderInput('time', 'Number of years',
                           min=0, max=250, value=20, step=10, round=0),

               sliderInput('time', 'Number of years',
                           min=0, max=250, value=20, step=10, round=0)
        )
      )
    )
  )
))
