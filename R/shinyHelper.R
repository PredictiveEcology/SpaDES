if(FALSE) {
  outputs = expand.grid(objectName=c("caribou", "landscape"), saveTime=0:10)
  mySim <- simInit(times, params, modules, objects = list(), paths, outputs = outputs)
  out <- spades(mySim, .plotInitialTime = NA)
  #server.R
  library(leaflet)
  # download and load data
  maps5 <- lapply(list.files(outputPath(out), full.names=TRUE, pattern = "landscape"), function(x) {
    y <- readRDS(x)
    #browser()
    crs(y) <- sp::CRS("+init=epsg:3857")
    y[[5]]})
  rasStack1 <- stack(maps5)

  maps4 <- lapply(list.files(outputPath(out), full.names=TRUE, pattern = "landscape"), function(x) {
    y <- readRDS(x)
    #browser()
    crs(y) <- sp::CRS("+init=epsg:3857")
    y[[4]]})
  rasStack2 <- stack(maps4)

  map1 = leaflet() %>% addTiles() %>%#'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
                               #attribution = paste(
                              #   '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors'
                              # ))

    fitBounds(xmin(rasStack1), ymin(rasStack1), xmax(rasStack1), ymax(rasStack1)) %>%
    setView(0,0,zoom=18)

  map2 = leaflet() %>% #addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
    #             attribution = paste(
    #               '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors'
    #             ))
    fitBounds(xmin(rasStack2), ymin(rasStack2), xmax(rasStack2), ymax(rasStack2)) %>%
    setView(0,0,zoom=18)
}
runApp(list(
  server =

    function(input, output){
      #shinyServer(function(input, output){

      v <- reactiveValues()
      for(i in 1:2) {
        output[[paste0("raster_map",i)]] = renderLeaflet({
          get(paste0("map", i))})
        #v[[paste0("reactiveRaster",i)]] <- reactive({get(paste0("rasStack",i))[[input$simTimes+1]]})
      }
#    output$raster_map2 = renderLeaflet({
#      map2})
    #browser()
    #v$reactiveRaster1 <- reactive({rasStack1[[input$simTimes+1]]})
    #v$reactiveRaster2 <- reactive({rasStack2[[input$simTimes+1]]})

    observeEvent(input$simTimes, {
      for(i in 1:2) {
        v[[paste0("reactiveRaster",i)]] <- get(paste0("rasStack",i))[[input$simTimes+1]]
      }
    }
                 )
    observe({
      for(i in 1:2) {
        leafletProxy(paste0("raster_map",i)) %>%
        #clearImages() %>%
          addRasterImage(v[[paste0("reactiveRaster",i)]], project = FALSE)
      }
    })
    #observe({
    #  leafletProxy("raster_map2") %>%
    #    #clearImages() %>%
    #    addRasterImage(v$reactiveRaster2, project = FALSE)
    #})

}


,
#UI.R

ui=fluidPage(
  #shinyUI(fluidPage(
  sliderInput("simTimes", "Time period",
              min=1, max=10, value=1, ticks=F, animate = TRUE),

  mainPanel(fluidRow(
    leafletOutput('raster_map1', width=200, height=200),
    leafletOutput('raster_map2', width=200, height=200))
    )
)

))
