shinyUI(fluidPage(
  titlePanel("ISA integration platform, SpaDES"),

  sidebarLayout(
    sidebarPanel(

      sliderInput('stopTime', 'Year to predict',
                  min=2006, max=2050, value=2008, step=1, round=FALSE,
                  format="###0"),
      br(),

      h4("Model integration"),
      checkboxInput('successionModule', 'Forest succession', value=TRUE),
      checkboxInput('fireModule', 'Forest fires', value=TRUE),
      checkboxInput('caribouModule', 'Caribou', value=TRUE),

      checkboxInput('carbonModule', 'Carbon', value=FALSE),
      checkboxInput('CanIBIS', 'CanIBIS', value=FALSE),
      checkboxInput('CANFIRE', 'CANFIRE', value=FALSE),

      sliderInput('drought', 'Drought intensity',
                  min=0.8, max=1.2, value=1, step=0.05, round=FALSE),
      br(),
      h4("Data integration"),
      checkboxGroupInput("landCoverDataSources", "Land cover data",
                         list("NFI","KNN","LCC05"),
                         list("LCC05")),
      checkboxGroupInput("climateDataSources", "Climate data",
                         list("McKenney et al.", "ClimateWNA")),
      br(),
      fileInput("dataUpload", "Datasets to upload")

    ),

    mainPanel(

      h4("Land Cover Classification, 2005, 250m"),
      plotOutput('lcc05'),
      h4(textOutput('numPixels')),
      hr(),
      h4("2005"),
      plotOutput('mapsInit'),
      h4(textOutput('endYear')),
      plotOutput('maps'),
      plotOutput('initHists', height="200px"),
      plotOutput('endHists', height="200px"),
      plotOutput('fireHist', height="200px"),
      plotOutput('Fire', height="400px", width = "400px"),
      plotOutput('caribou', height="400px", width = "400px")
    )
  )
))
