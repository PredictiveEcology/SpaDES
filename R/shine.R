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
#' @param debug Logical. If TRUE, then will show spades event debugger in console.
#' @param ... additional arguments. Currently not used
#' @export
#' @importFrom shiny fluidPage titlePanel sidebarPanel sidebarLayout actionButton sliderInput uiOutput
#' @importFrom shiny mainPanel plotOutput renderUI tabPanel tabsetPanel textOutput
#' @importFrom shiny eventReactive renderPlot runApp downloadButton downloadHandler h3
#' @importFrom shiny numericInput
#' @importFrom DiagrammeR DiagrammeROutput renderDiagrammeR
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
setGeneric("shine", function(sim, title="SpaDES App", debug=FALSE, ...) {
  standardGeneric("shine")
})

setMethod(
  "shine",
  signature(sim = "simList"),
  definition = function(sim, title, debug, ...) {

  # Keep a copy of input simList so Reset button works
  simOrig_ <- as(sim, "simList_") # convert objects first
  simOrig <- sim # Not enough because objects are in an environment, so they both change

  #i = 1
  endTime <- end(sim)
  ui <- fluidPage(
    titlePanel(title),
    sidebarLayout(
      sidebarPanel(
        actionButton("fullSpaDESButton", "Run model"),
        actionButton("stopButton", "Stop"),
        actionButton("oneTimestepSpaDESButton", label=textOutput("StepActionButton")),
        numericInput("Steps", "Step size", 1, width="100px"),
        actionButton("resetSimInit", "Reset"),
        downloadButton('downloadData', 'Download'),
        sliderInput("simTimes", paste0("Simuated ",timeunit(sim)), sep="",
                    start(sim) , end(sim), start(sim)),
        h3("Modules"),
        uiOutput("moduleTabs")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Preview", plotOutput("spadesPlot", height = "800px")),
          tabPanel("Module diagram", plotOutput("moduleDiagram", height = "400px")),
          tabPanel("Object diagram",
                   DiagrammeROutput("objectDiagram",
                                    height = "400px")),#textOutput("objectDiagramTextHeight"))),
          tabPanel("Event diagram", DiagrammeROutput("eventDiagram", height = "400px"))
        )

      )
    )
  )

  server <- function(input, output, session) {
    curDev <- dev.cur()
    alreadyPlotted <- grepl(ls(.spadesEnv), pattern=paste0("spadesPlot",curDev))
    if(any(alreadyPlotted)) {
      clearPlot() # Don't want to use this, but it seems that renderPlot will not allow overplotting
    }

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

    spadesCallFull <- function() {#eventReactive(input$fullSpaDESButton, {
      # Update simInit with values obtained from UI
      mods <- unlist(modules(sim))[-(1:4)]
      for(m in mods) {
        for(i in names(params(sim)[[m]][sapply(params(sim)[[m]], is.numeric)])) {
          if(!is.null(input[[paste0(m,"$",i)]])) # only if it is not null
            params(sim)[[m]][[i]] <- input[[paste0(m,"$",i)]]
        }
      }

      end(sim) <- pmin(endTime, time(sim) + 1)

      if(is.null(v$stop)) {v$stop="go"}
      if((time(sim) < endTime) & (v$stop!="stop")) invalidateLater(0)
      sim <<- spades(sim, debug=debug)
#      v$time <- time(sim)

    }

    spadesCall <- eventReactive(input$oneTimestepSpaDESButton, {
      # Update simInit with values obtained from UI
      mods <- unlist(modules(sim))[-(1:4)]
      for(m in mods) {
        for(i in names(params(sim)[[m]][sapply(params(sim)[[m]], is.numeric)])) {
          if(!is.null(input[[paste0(m,"$",i)]])) # only if it is not null
            params(sim)[[m]][[i]] <- input[[paste0(m,"$",i)]]
        }
      }

      end(sim) <- time(sim) + input$Steps
      sim <<- spades(sim, debug=debug)
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

    v <- reactiveValues(data = NULL, time=time(sim), end=end(sim), sliderUsed=FALSE)

    observeEvent(input$oneTimestepSpaDESButton, {
      v$data <- "oneTime"
      v$stop <- "go"
    })

    observeEvent(input$stopButton, {
      v$stop <- "stop"
    })

    observeEvent(input$resetSimInit, {
      v$data <- "reset"
      v$time <- start(sim)
      v$end <- endTime
    })

    observeEvent(input$fullSpaDESButton, {
      v$data <- "full"
      v$stop <- "go"
    })

    output$spadesPlot <- renderPlot({
      curDev <- dev.cur()
      alreadyPlotted <- grepl(ls(.spadesEnv), pattern=paste0("spadesPlot",curDev))
      if(any(alreadyPlotted)) {
        rePlot()
      } else {
        clearPlot() # Don't want to use this, but it seems that renderPlot will not allow overplotting
      }
      if (is.null(v$data)) return()
      if(v$data=="oneTime") {
        spadesCall()
      } else if (v$data=="full") {
        spadesCallFull()
      } else if (v$data=="reset") {
        simReset()
      }
      v$time <- time(sim)
      if(time(sim)>=endTime) {
        v$end <- end(sim)
      }
      v$sliderUsed=FALSE
    })

    output$moduleDiagram <- renderPlot({
      moduleDiagram(sim)
    })

    output$objectDiagram <-
      renderDiagrammeR({
          if (v$time<=start(sim)) {
            return()
          } else {
            objectDiagram(sim)
          }
      })

    output$objectDiagramTextHeight <- renderText({
      #browser()
      print("400px")
      #length(completed(sim))
    })

    output$eventDiagram <- renderDiagrammeR({
      if (v$time<=start(sim)) {
        return()
      } else {
        eventDiagram(sim, startDate="0000-01-01")
      }
    })

    observeEvent(input$simTimes, {
      time(sim) <<- input$simTimes
    })

    observe({
#      browser()
      updateSliderInput(session, "simTimes", value=v$time, max=v$end)
#      v$sliderUsed <- "No"
    })

    output$StepActionButton <- renderPrint({
#      browser()
#      if(v$sliderUsed=="Yes") {
#        cat("Step to time ", input$simTimes, sep="")
#      } else {
        cat("Step ", input$Steps, " timestep", "s"[input$Steps!=1], sep="")
#      }
#      v$sliderUsed <- "Yes"
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
