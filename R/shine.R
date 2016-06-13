#' Display a simple, interactive shiny app of the simList
#'
#' Currently, this is quite simple. It creates a side bar with the simulation
#' times, plus a set of tabs, one for each module, with numeric sliders.
#' Currently, this does not treat NAs correctly. Also, it is slow (shiny is not
#' built to be fast out of the box). Currently, it does not show plotting
#' updates; it only shows the final output of a spades call. There are two
#' buttons, one to run the entire spades call, the other to do just one time
#' step at a time. It can be repeatedly pressed.
#'
#' @note Many module parameters are only accessed by modules at the start of a
#'   model run. So, even if the user changes them mid run, there won't be an
#'   effect on the model runs until \code{Reset} is pressed, and one of the Run
#'   buttons is pressed.
#'
#' @note \code{.plotInterval} changes will only affect plots that are the base
#'   layer of a given plot image. If there are layers on top of a base layer
#'   (e.g., an agent on top of a raster layer), the .plotInterval of the
#'   overlayed layers is ignored.
#'
#' @param sim a simList object
#' @param title character string. The title of the shiny page.
#' @param debug Logical. If TRUE, then will show spades event debugger in
#'   console.
#' @param ... additional arguments. Currently not used
#' @export
#' @importFrom shiny fluidPage titlePanel sidebarPanel sidebarLayout
#'   actionButton sliderInput uiOutput
#' @importFrom shiny mainPanel plotOutput renderUI tabPanel tabsetPanel
#' @importFrom shiny eventReactive renderPlot runApp downloadButton
#' @importFrom shiny numericInput h4 checkboxInput invalidateLater observe
#' @importFrom shiny updateTabsetPanel downloadHandler h3 textOutput
#' @importFrom shiny reactiveValues observeEvent updateSliderInput renderPrint
#' @importFrom DiagrammeR DiagrammeROutput renderDiagrammeR
#' @importFrom DT renderDataTable dataTableOutput
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
#'
#' # if the user wants to see the events go by, which can help with debugging:
#' shine(mySim, debug=TRUE)
#' }
setGeneric("shine", function(sim, title="SpaDES App", debug=FALSE, ...) {
  standardGeneric("shine")
})

#' @export
#' @rdname shine
setMethod(
  "shine",
  signature= signature(sim = "simList"),
  definition = function(sim, title, debug, ...) {

  # Keep a copy of input simList so Reset button works
  simOrig_ <- as(sim, "simList_") # convert objects first
  simOrig <- sim # Not enough because objects are in an environment, so they both change

  #i = 1
  endTime <- end(sim)
  startTime <- start(sim)
  ui <- fluidPage(
    titlePanel(title),
    sidebarLayout(
      sidebarPanel(
        actionButton("fullSpaDESButton", "Run model"),
        actionButton("stopButton", "Stop"),
        actionButton("oneTimestepSpaDESButton", label = textOutput("StepActionButton")),
        numericInput("Steps", "Step size", 1, width = "100px"),
        actionButton("resetSimInit", "Reset"),
        downloadButton('downloadData', 'Download'),
        sliderInput("simTimes", paste0("Simuated ", timeunit(sim)), sep = "",
                    start(sim) , end(sim), start(sim)),
        h3("Modules"),
        uiOutput("moduleTabs")
      ),
      mainPanel(
        tabsetPanel(id="topTabsetPanel",
          tabPanel("Preview", plotOutput("spadesPlot", height = "800px")),
          tabPanel("Module diagram", uiOutput("moduleDiagramUI")),
          tabPanel("Object diagram", uiOutput("objectDiagramUI")),
          tabPanel("Event diagram", uiOutput("eventDiagramUI")),
          tabPanel("Object browser", uiOutput("objectBrowserUI")),
          tabPanel("Inputs loaded", uiOutput("inputObjectsUI"))
        )

      )
    )
  )

  server <- function(input, output, session) {
    # Some cases there may be an error due to a previous plot still existing - this should clear
    curDev <- dev.cur()
    alreadyPlotted <- grepl(ls(.spadesEnv), pattern = paste0("spadesPlot", curDev))
    if (any(alreadyPlotted)) {
      clearPlot() # Don't want to use this, but it seems that renderPlot will not allow overplotting
    }

    # Left side module tabs
    output$moduleTabs <- renderUI({
      mods <- unlist(modules(sim))[-(1:4)]
      nTabs <- length(mods)
      myTabs <- lapply(mods, function(x) {
        tabPanel(x, h4("Parameters"), uiOutput(outputId = x))
      })
      do.call(tabsetPanel, myTabs)
    })

    # Sliders in module tabs
    for (k in unlist(modules(sim))[-(1:4)]) {
      local({ # local is needed because it must force evaluation, avoid lazy evaluation
        kLocal <- k
        output[[kLocal]] <- renderUI({
          Params <- params(sim)[[kLocal]]
          lapply(names(Params), function(i) {
            if (i %in% c(".plotInitialTime", ".saveInitialTime", ".plotInterval", ".saveInterval")) {
              if (!is.na(params(sim)[[kLocal]][[i]])) {
                sliderInput(
                  inputId = paste0(kLocal,"$",i),
                  label = i,
                  min = min(start(sim), params(sim)[[kLocal]][[i]]),
                  max = min(endTime, end(sim)) -
                    ifelse(i %in% c(".plotInterval", ".saveInterval"), start(sim), 0),
                  value = params(sim)[[kLocal]][[i]],
                  step = ((min(endTime, end(sim)) - start(sim))/10) %>% as.numeric(),
                  sep = "")
              }
            } else if (is.numeric(Params[[i]])) {
              sliderInput(
                inputId = paste0(kLocal, "$", i),
                label = i,
                min = params(sim)[[kLocal]][[i]]*0.5,
                max = params(sim)[[kLocal]][[i]]*2,
                value = params(sim)[[kLocal]][[i]],
                step =(params(sim)[[kLocal]][[i]]*2 - params(sim)[[kLocal]][[i]]*0.5)/10,
                sep="")
            } else if (is.logical(Params[[i]])) {
              checkboxInput(
                inputId = paste0(kLocal, "$", i),
                label = i,
                value = params(sim)[[kLocal]][[i]])
            }
            # To do make ones for logical, character, functions, text etc.
          })
        })

      })
    }

    spadesCallFull <- function() {#eventReactive(input$fullSpaDESButton, {
      # Update simInit with values obtained from UI
      mods <- unlist(modules(sim))[-(1:4)]
      for (m in mods) {
        for (i in names(params(sim)[[m]])) {
          if (!is.null(input[[paste0(m, "$", i)]])) # only if it is not null
            params(sim)[[m]][[i]] <- input[[paste0(m, "$", i)]]
        }
      }
      end(sim) <- pmin(endTime, time(sim) + 1)
      if (is.null(v$stop)) {v$stop = "go"}
      if ((time(sim) < endTime) & (v$stop != "stop")) invalidateLater(0)
      sim <<- spades(sim, debug = debug) # Run spades
    }

    # Needs cleaning up - This should just be a subset of above
    spadesCall <- eventReactive(input$oneTimestepSpaDESButton, {
      # Update simInit with values obtained from UI
      mods <- unlist(modules(sim))[-(1:4)]
      for (m in mods) {
        for (i in names(params(sim)[[m]])) {
          if (!is.null(input[[paste0(m, "$", i)]])) {# only if it is not null
            params(sim)[[m]][[i]] <- input[[paste0(m, "$", i)]]
          }
        }
      }
      end(sim) <- time(sim) + input$Steps
      sim <<- spades(sim, debug = debug)
    })

    simReset <- eventReactive(input$resetSimInit, {
      # Update simInit with values obtained from UI
      clearPlot() # Don't want to use this, but it seems that renderPlot will not allow overplotting
      rm(list = ls(sim), envir = envir(sim))
      sim <<- simOrig
      for (i in names(simOrig_@.list)) {
        sim[[i]]  <<- simOrig_@.list[[i]]
      }
    })

    v <- reactiveValues(data = NULL, time = time(sim), end = end(sim), sliderUsed = FALSE)

    # Button clicks
    observeEvent(input$oneTimestepSpaDESButton, {
      v$data <- "oneTime"
      v$stop <- "go"
      updateTabsetPanel(session, "topTabsetPanel", selected = "Preview")
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
      updateTabsetPanel(session, "topTabsetPanel", selected = "Preview")
    })

    # Main plot
    output$spadesPlot <- renderPlot({
      curDev <- dev.cur()
      alreadyPlotted <- grepl(ls(.spadesEnv), pattern = paste0("spadesPlot", curDev))
      if (any(alreadyPlotted)) {
        rePlot()
      } else {
        clearPlot() # Don't want to use this, but it seems that renderPlot will not allow overplotting
      }
      if (is.null(v$data)) return() # catch if no data yet
      if (v$data == "oneTime") {
        spadesCall()
      } else if (v$data == "full") {
        spadesCallFull()
      } else if (v$data == "reset") {
        simReset()
      }
      v$time <- time(sim)
      if (time(sim) >= endTime) {
        v$end <- end(sim)
      }
      v$sliderUsed <- FALSE
    })

    output$moduleDiagram <- renderPlot({
      moduleDiagram(sim)
    })

    output$moduleDiagramUI <- renderUI({
      plotOutput("moduleDiagram",
                 height = max(600, (length(modules(sim)) - 4)*100))
    })

    output$objectDiagram <- renderDiagrammeR({
          if (v$time <= start(sim)) {
            return()
          } else {
            objectDiagram(sim)
          }
      })

    output$objectDiagramUI <- renderUI({
      if (v$time <= start(sim)) {
        return()
      } else {
        DiagrammeROutput("objectDiagram",
                         height = max(600, length(ls(sim))*30))
      }
    })

    output$eventDiagram <- renderDiagrammeR({
      if (v$time <= start(sim)) {
        return()
      } else {
        eventDiagram(sim, startDate = "0000-01-01")
      }
    })

    output$eventDiagramUI <- renderUI({
      if (v$time <= start(sim)) {
        return()
      } else {
        DiagrammeROutput("eventDiagram",
                         height = max(800, NROW(completed(sim))*25))
      }
    })

    output$objectBrowser <- renderDataTable({
      v$time
      dt <- lapply(names(objs(sim)), function(x) {
            data.frame(Name = x, Class = is(objs(sim)[[x]])[1])}) %>%
      do.call(args = ., rbind)
    })

    output$objectBrowserUI <- renderUI({
      v$time
      dataTableOutput("objectBrowser")
    })

    output$inputObjects <- renderDataTable({
      dt <- inputs(sim)
    })

    output$inputObjectsUI <- renderUI({
      dataTableOutput("inputObjects")
    })

    observeEvent(input$simTimes, {
      time(sim) <<- input$simTimes
    })

    # the time slider must update if stepping through with buttons
    observe({
      updateSliderInput(session, "simTimes", value = v$time, max = v$end)
    })

    output$StepActionButton <- renderPrint({
#      if (v$sliderUsed == "Yes") {
#        cat("Step to time ", input$simTimes, sep = "")
#      } else {
        cat("Step ", input$Steps, " timestep", "s"[input$Steps != 1], sep = "")
#      }
#      v$sliderUsed <- "Yes"
    })

    output$downloadData <- downloadHandler(
      filename = function() { paste("simObj.rds", sep = "") },
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
