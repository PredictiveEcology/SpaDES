#' Display a simple, interactive shiny app of the simList
#'
#' Currently, this is quite simple. It creates a side bar with the simulation times,
#' plus a set of tabs, one for each module, with numeric sliders. Currently, this
#' does not treat NAs correctly. Also, it is slow (shiny is not built to be fast
#' out of the box). Currently, it does not show plotting updates; it only shows
#' the final output of a spades call. There are two buttons, one to run the entire
#' spades call, the other to do just one time step at a time. It can be repeatedly
#' pressed.
#'
#' @param sim a simInit object
#' @param title character string. The title of the shiny page.
#' @param ... additional arguments. Currently not used
#' @export
#' @importFrom shiny fluidPage titlePanel sidebarPanel sidebarLayout actionButton sliderInput uiOutput
#' @importFrom shiny mainPanel plotOutput renderUI tabPanel tabsetPanel
#' @importFrom shiny eventReactive renderPlot runApp downloadButton downloadHandler h3
#' @examples
#' \dontrun{
#' times <- list(start = 0.0, end = 20.0)
#' parameters <- list(
#'   .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'   .progress = list(NA),
#'   randomLandscapes = list(nx = 100L, ny = 100L, inRAM = TRUE),
#'   fireSpread = list(nFires = 10L, spreadprob = 0.225, its = 1e6, persistprob = 0,
#'                     returnInterval = 10, startTime = 0,
#'                     .plotInitialTime = 0.0, .plotInterval = 10),
#'   caribouMovement = list(N = 100L, moveInterval = 1,
#'                          .plotInitialTime = 1, .plotInterval = 1)
#' )
#' modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
#' path <- list(modulePath = system.file("sampleModules", package = "SpaDES"),
#'              outputPath = file.path(tempdir(), "simOutputs"))
#'
#' mySim <- simInit(times = times, params = parameters, modules = modules, path = path)
#'
#' shine(mySim)
#' }
#'
setGeneric("shine", function(sim, title="SpaDES App", ...) {
  standardGeneric("shine")
})

setMethod(
  "shine",
  signature(sim = "simList"),
  definition = function(sim, title, ...) {

  simOrig_ <- as(sim, "simList_")
  simOrig <- sim

  i = 1
  ui <- fluidPage(
    titlePanel(title),
    sidebarLayout(
      sidebarPanel(
        actionButton("fullSpaDESButton", "Run model"),
        actionButton("oneTimestepSpaDESButton", "Run model 1 timestep"),
        actionButton("resetSimInit", "Reset"),
        downloadButton('downloadData', 'Download'),
        sliderInput("simTimes", paste0("Simuated ",timeunit(sim)), sep="",
                    start(sim) , end(sim), c(start(sim), end(sim))),
        h3("Modules"),
        uiOutput("moduleTabs")
      ),
      mainPanel(
        plotOutput("spadesPlot", height = "800px"),
        plotOutput("spadesPlotFull", height = "800px"),
        plotOutput("spadesReset", height = "800px")
      )
    )
  )

  server <- function(input, output) {

    output$moduleTabs = renderUI({
      mods <- unlist(modules(sim))[-(1:4)]
      nTabs = length(mods)
      myTabs = lapply(mods, function(x) tabPanel(x,
                                                 h4("Parameters"),
                                                 uiOutput(outputId = x)))
      do.call(tabsetPanel, myTabs)
    })

    for(k in unlist(modules(sim))[-(1:4)]) {
      local({ # local is needed because it must force evaluation, avoid lazy evaluation
        kLocal <- k
        output[[kLocal]] <- renderUI({
          whSliders <- params(sim)[[kLocal]]
          whSliders <- names(whSliders[sapply(whSliders, is.numeric)]) # only numeric parameters
          lapply(whSliders, function(i) {
            sliderInput(
              inputId = paste0(kLocal,"$",i),
              label = i,
              min = params(sim)[[kLocal]][[i]]*0.5,
              max = params(sim)[[kLocal]][[i]]*2,
              value = params(sim)[[kLocal]][[i]],
              step =(params(sim)[[kLocal]][[i]]*2 - params(sim)[[kLocal]][[i]]*0.5)/10,
              sep="")
          })
        })
      })
    }

    spadesCallFull <- eventReactive(input$fullSpaDESButton, {
      # Update simInit with values obtained from UI
      clearPlot() # Don't want to use this, but it seems that renderPlot will not allow overplotting
      start(sim) <- input$simTimes[1]
      end(sim) <- input$simTimes[2]

      mods <- unlist(modules(sim))[-(1:4)]
      for(m in mods) {
       for(i in names(params(sim)[[m]][sapply(params(sim)[[m]], is.numeric)])) {
         if(!is.null(input[[paste0(m,"$",i)]])) # only if it is not null
           params(sim)[[m]][[i]] <- input[[paste0(m,"$",i)]]
       }
      }

      sim <<- spades(sim)
    })

    simReset <- eventReactive(input$resetSimInit, {
      # Update simInit with values obtained from UI
      clearPlot() # Don't want to use this, but it seems that renderPlot will not allow overplotting
      rm(list=ls(sim), envir=envir(sim))
      sim <<- simOrig
      for(i in names(simOrig_@.list)) {
        sim[[i]]  <<- simOrig_@.list[[i]]
      }

    })

    spadesCall <- eventReactive(input$oneTimestepSpaDESButton, {
      # Update simInit with values obtained from UI
      clearPlot() # Don't want to use this, but it seems that renderPlot will not allow overplotting
      mods <- unlist(modules(sim))[-(1:4)]
      for(m in mods) {
        for(i in names(params(sim)[[m]][sapply(params(sim)[[m]], is.numeric)])) {
          if(!is.null(input[[paste0(m,"$",i)]])) # only if it is not null
            params(sim)[[m]][[i]] <- input[[paste0(m,"$",i)]]
        }
      }

      end(sim) <- time(sim) + 1
      sim <<- spades(sim)
    })

    output$spadesPlot <- renderPlot({
      spadesCall()
    })
    output$spadesPlotFull <- renderPlot({
      spadesCallFull()
    })
    output$spadesReset <- renderPlot({
      simReset()
    })

    output$downloadData <- downloadHandler(
      filename = function() { paste("simObj.rds", sep="") },
      content = function(file) {
        saveRDS(sim, file = file)
      }
    )
  }

  runApp(list(ui = ui, server = server),
                launch.browser = getOption("viewer", browseURL),
                quiet = TRUE
  )
})
