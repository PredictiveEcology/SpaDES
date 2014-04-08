library(shiny)

# Define server
shinyServer(function(input, output) {
    
  p <- reactive({ 
      dosim(maxsimtime=as.numeric(input$tmax), modules=list("habitat", "caribou"),
                                  params=list(Ncaribou=as.numeric(input$N)), path="..")
      })

  output$mapPlot <- renderPlot({
    par(mar=c(5,5,1,1))
    plot(hab)
    points(caribou, pch=19, cex=0.1)
  })
  
  # debug only
  output$debug <- renderText({ p() })
})
