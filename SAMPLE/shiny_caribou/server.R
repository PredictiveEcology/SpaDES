library(shiny)

# Define server
shinyServer(function(input, output) {
    
  sim <- reactive({
        dosim(maxsimtime=as.numeric(input$tmax), modules=list("habitat", "caribou"),
                  params=list(Ncaribou=as.numeric(input$N)), path="..")
      })
  
#   output$mapPlot <- renderPlot({
#      map <- readRDS("../data/habitat.rds")
#      plot(map)
#      
#      caribou <- readRDS("../data/caribou_0.rds")
#      points(caribou, pch=19, cex=0.1)
#      
#      for (i in 1:input$tmax) {
#          caribou <- readRDS(paste("../data/caribou_", i, ".rds", sep=""))
#          points(caribou[[1]], pch=19, cex=0.1)
#          points(caribou[[2]]$x, caribou[[2]]$y, col=caribou[[2]]$ids, pch=19, cex=0.1)
#      }
#   })
  
  output$simPlot <- renderPlot({ sim() })
  
  # debug only
  output$debug <- renderText({ sim() })
})
