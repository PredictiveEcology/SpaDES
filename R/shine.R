require(SpaDES)

times <- list(start = 0.0, end = 20)
parameters <- list(
  .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
  .progress = list(NA),
  randomLandscapes = list(nx = 100L, ny = 100L, inRAM = TRUE),
  fireSpread = list(nFires = 10L, spreadprob = 0.225, its = 1e6, persistprob = 0,
                    returnInterval = 10, startTime = 0,
                    .plotInitialTime = 0, .plotInterval = 10),
  caribouMovement = list(N = 100L, moveInterval = 1,
                         .plotInitialTime = 1, .plotInterval = 1)
)
modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
path <- list(modulePath = system.file("sampleModules", package = "SpaDES"),
             outputPath = file.path(tempdir(), "simOutputs"))

mySim <- simInit(times = times, params = parameters, modules = modules, path = path)

#if (interactive()) dev()
#spades(mySim)

#####
library(shiny)
#library(ggplot2)
#library(dplyr)
sessionEnv <- new.env()
i = 1
ui <- fluidPage(
  titlePanel("SpaDES App"),
  sidebarLayout(
     sidebarPanel(
       actionButton("goButton", "Run model"),
       sliderInput(paste0("param",1), names(params(mySim)[[6]])[3], 10, 1000, 100),
       sliderInput(paste0("param",2), paste0("Param",2), 0, 100, c(25,50)),
       sliderInput(paste0("param",3), paste0("Param",3), 0, 100, c(25,50)),
       sliderInput(paste0("param",4), paste0("Param",4), 0, 100, c(25,50))

       #sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
#       radioButtons("typeInput", "Product type",
#                    choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
#                    selected = "WINE"),
#       uiOutput("countryOutput")
     ),
    mainPanel(
      plotOutput("coolplot")#,
#      br(), br(),
#      tableOutput("results")
    )
  )
)

server <- function(input, output, session) {
#   output$countryOutput <- renderUI({
#     selectInput("countryInput", "Country",
#                 sort(unique(bcl$PRODUCT_COUNTRY_ORIGIN_NAME)),
#                 selected = "CANADA")
#   })

  for(i in 1:length(params(mySim))) {
    output[[paste0("param",i)]] <- renderUI({
      selectInput(paste0("param",i), paste0("Param",i))
    })
  }

#  output$coolplot <- renderPlot({
#    clearPlot()
    #invalidateLater(0, session)
#  })

  spadesCall <- eventReactive(input$goButton, {
    clearPlot()
    params(mySim)$caribouMovement$N <- input[[paste0("param",1)]]
    mySim <- spades(mySim)
  })

  output$coolplot <- renderPlot({
    spadesCall()
  })
#   output$results <- renderTable({
#     filtered()
#   })
}

shinyApp(ui = ui, server = server)



