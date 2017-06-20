#' Display a simple, interactive shiny app of the simList
#'
#' Currently, this is quite simple. It creates a side bar with the simulation
#' times, plus a set of tabs, one for each module, with numeric sliders.
#' Currently, this does not treat NAs correctly. Also, it is slow (shiny is not
#' built to be fast out of the box).
#' There are two buttons, one to run the entire spades call, the other to do
#' just one time step at a time. It can be repeatedly pressed.
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
#' @param filesOnly Logical. If TRUE, then the server.R, ui.R files will be written
#'                  to a temp location, with a message indicating where they are.
#'                  Publishing this to \url{https://shinyapps.io} is currently very buggy,
#'                  and will likely not work as desired.
#' @param ... additional arguments. Currently not used
#'
#' @export
#' @importFrom shiny actionButton checkboxInput downloadButton downloadHandler
#' @importFrom shiny eventReactive fluidPage h3 h4 invalidateLater
#' @importFrom shiny mainPanel numericInput observe observeEvent plotOutput
#' @importFrom shiny reactiveValues renderPlot renderPrint renderUI runApp
#' @importFrom shiny sliderInput sidebarLayout sidebarPanel
#' @importFrom shiny tabPanel tabsetPanel textOutput titlePanel
#' @importFrom shiny uiOutput updateSliderInput updateTabsetPanel
#' @importFrom DiagrammeR DiagrammeROutput renderDiagrammeR
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom grDevices dev.cur
#' @importFrom quickPlot clearPlot rePlot
#' @examples
#' \dontrun{
#'  mySim <- simInit(
#'    times <- list(start = 0.0, end = 20.0),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
#'  )
#'
#' shine(mySim)
#'
#' # To publish to shinyapps.io, need files. This is not reliable yet.
#' shine(mySim, filesOnly = TRUE)
#'
#' # if the user wants to see the events go by, which can help with debugging:
#' shine(mySim, debug = TRUE)
#' }
setGeneric("shine", function(sim, title = "SpaDES App", debug = FALSE, filesOnly = FALSE, ...) {
  standardGeneric("shine")
})

#' @export
#' @rdname shine
setMethod(
  "shine",
  signature = signature(sim = "simList"),
  definition = function(sim, title, debug, filesOnly, ...) {

  # Keep a copy of input simList so Reset button works
  simOrig_ <- as(sim, "simList_") # convert objects first
  simOrig <- sim # Not enough because objects are in an environment, so they both change

  endTime <- end(sim)
  startTime <- start(sim)
  fluidPageArgs <-     list(
    titlePanel(title),
    sidebarLayout(
      sidebarPanel(
        actionButton("fullSpaDESButton", "Run model"),
        actionButton("stopButton", "Stop"),
        actionButton("oneTimestepSpaDESButton", label = textOutput("StepActionButton")),
        numericInput("Steps", "Step size", 1, width = "100px"),
        actionButton("resetSimInit", "Reset"),
        downloadButton("downloadData", "Download"),
        sliderInput("simTimes", paste0("Simulated ", sim@simtimes[["timeunit"]]), sep = "",
                    start(sim), end(sim), start(sim)),
        h3("Modules"),
        uiOutput("moduleTabs")
      ),
      mainPanel(
        tabsetPanel(id = "topTabsetPanel",
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
    if (exists(".spadesEnv"))
      alreadyPlotted <- grepl(ls(.spadesEnv), pattern = paste0("spadesPlot", curDev))
    else
      alreadyPlotted <- FALSE

    if (any(alreadyPlotted)) {
      clearPlot() # Don't want to use this, but it seems that renderPlot will not allow overplotting
    }

    # Left side module tabs
    output$moduleTabs <- renderUI({
      mods <- unlist(modules(sim))
      nTabs <- length(mods)
      myTabs <- lapply(mods, function(x) {
        tabPanel(x, h4("Parameters"), uiOutput(outputId = x))
      })
      do.call(tabsetPanel, myTabs)
    })

    # Sliders in module tabs
    for (k in unlist(modules(sim))) {
      local({
        # local is needed because it must force evaluation, avoid lazy evaluation
        kLocal <- k
        output[[kLocal]] <- renderUI({
          Params <- params(sim)[[kLocal]]
          lapply(names(Params), function(i) {
            moduleParams <- sim@depends@dependencies[[kLocal]]@parameters[sim@depends@dependencies[[kLocal]]@parameters[, "paramName"] == i, ]
            if (i %in% c(".plotInitialTime", ".saveInitialTime", ".plotInterval", ".saveInterval")) {
              if (!is.na(params(sim)[[kLocal]][[i]])) {
                sliderInput(
                  inputId = paste0(kLocal, "$", i),
                  label = i,
                  min = min(start(sim), params(sim)[[kLocal]][[i]]),
                  max = min(endTime, end(sim)) -
                    ifelse(i %in% c(".plotInterval", ".saveInterval"), start(sim), 0),
                  value = params(sim)[[kLocal]][[i]],
                  step = ((min(endTime, end(sim)) - start(sim)) / 10) %>% as.numeric(),
                  sep = "")
              }
            } else if (is.numeric(Params[[i]])) {
              sliderInput(
                inputId = paste0(kLocal, "$", i),
                label = i,
                min = moduleParams[["min"]][[1]],
                max = moduleParams[["max"]][[1]],
                value = params(sim)[[kLocal]][[i]],
                step = (moduleParams[["max"]][[1]] - moduleParams[["min"]][[1]]) / 10,
                sep = "")
            } else if (is.logical(Params[[i]])) {
              checkboxInput(
                inputId = paste0(kLocal, "$", i),
                label = i,
                value = params(sim)[[kLocal]][[i]])
            } else if (is.character(Params[[i]])) {
              selectInput(
                inputId = paste0(kLocal, "$", i),
                label = i,
                multiple = FALSE,
                choices = moduleParams[["min"]][[1]]
              )
            }
            # To do make ones for logical, character, functions, text etc.
          })
        })
      })
    }

    spadesCallFull <- function() {
      # Update simInit with values obtained from UI
      mods <- unlist(modules(sim))
      for (m in mods) {
        for (i in names(params(sim)[[m]])) {
          if (!is.null(input[[paste0(m, "$", i)]])) # only if it is not null
            params(sim)[[m]][[i]] <- input[[paste0(m, "$", i)]]
        }
      }
      end(sim) <- pmin(endTime, time(sim, sim@simtimes[["timeunit"]]) + 1)
      if (is.null(v$stop)) v$stop <- "go"
      if ((time(sim, sim@simtimes[["timeunit"]]) < endTime) & (v$stop != "stop")) invalidateLater(0)
      sim <<- spades(sim, debug = debug) # Run spades
    }

    # Needs cleaning up - This should just be a subset of above
    spadesCall <- eventReactive(input$oneTimestepSpaDESButton, {
      # Update simInit with values obtained from UI
      mods <- unlist(modules(sim))
      for (m in mods) {
        for (i in names(params(sim)[[m]])) {
          if (!is.null(input[[paste0(m, "$", i)]])) {
            params(sim)[[m]][[i]] <- input[[paste0(m, "$", i)]]
          }
        }
      }
      end(sim) <- time(sim, sim@simtimes[["timeunit"]]) + input$Steps
      sim <<- spades(sim, debug = debug)
    })

    simReset <- eventReactive(input$resetSimInit, {
      # Update simInit with values obtained from UI
      clearPlot() # Don't want to use this, but it seems that renderPlot will not allow overplotting
      rm(list = ls(sim), envir = sim@.envir)
      sim <<- simOrig
      for (i in names(simOrig_@.list)) {
        sim[[i]]  <<- simOrig_@.list[[i]]
      }
    })

    v <- reactiveValues(data = NULL, time = time(sim, sim@simtimes[["timeunit"]]),
                        end = end(sim, sim@simtimes[["timeunit"]]), sliderUsed = FALSE)

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
      alreadyPlotted <- if (exists(".spadesEnv")) {
        grepl(ls(.spadesEnv), pattern = paste0("spadesPlot", curDev))
      } else {
        FALSE
      }

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
      v$time <- time(sim, sim@simtimes[["timeunit"]])
      if (time(sim, sim@simtimes[["timeunit"]]) >= endTime) {
        v$end <- end(sim)
      }
      v$sliderUsed <- FALSE
    })

    output$moduleDiagram <- renderPlot({
      moduleDiagram(sim)
    })

    output$moduleDiagramUI <- renderUI({
      plotOutput("moduleDiagram", height = max(600, length(modules(sim)) * 100))
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
        DiagrammeROutput("objectDiagram", height = max(600, length(ls(sim)) * 30))
      }
    })

    output$eventDiagram <- renderDiagrammeR({
      if (v$time <= start(sim)) {
        return()
      } else {
        eventDiagram(sim)
      }
    })

    output$eventDiagramUI <- renderUI({
      if (v$time <= start(sim)) {
        return()
      } else {
        DiagrammeROutput("eventDiagram", height = max(800, NROW(completed(sim)) * 25))
      }
    })

    output$objectBrowser <- renderDataTable({
      v$time
      dt <- lapply(names(objs(sim)), function(x) {
            data.frame(Name = x, Class = is(objs(sim)[[x]])[1])
      }) %>%
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
      filename = function() paste("simObj.rds", sep = ""),
      content = function(file) saveRDS(sim, file = file)
    )
  }

  if (filesOnly) {
    shinyAppDir <- file.path(tempdir(), "shinyApp")
    checkPath(shinyAppDir, create = TRUE)
    globalFile <- file.path(shinyAppDir, "global.R", fsep = "/")
    saveRDS(sim, file = file.path(shinyAppDir, "sim.Rdata"))
    con <- file(globalFile, open = "w+b");
    writeLines(paste("debug <-", debug), con = con)
    writeLines("library(DiagrammeR)", con = con)
    #writeLines("library(igraph)", con = con)
    writeLines("library(DT)", con = con)
    writeLines("library(SpaDES)", con = con)
    pkgs <- unique(unlist(lapply(sim@depends@dependencies,
                         function(x) x@reqdPkgs)))
    writeLines(paste0(paste0("library(", pkgs, ")"), collapse = "\n"),
               con = con)
    writeLines("sim <- readRDS(file = \"sim.Rdata\")", con = con)
    writeLines("simOrig_ <- as(sim, \"simList_\")", con = con) # convert objects first
    writeLines("simOrig <- sim", con = con) # Not enough because objects are in an environment, so they both change

    writeLines("endTime <- end(sim)", con = con)
    writeLines("startTime <- start(sim)", con = con)

    close(con)

    serverFile <- file.path(shinyAppDir, "server.R", fsep = "/")
    con <- file(serverFile, open = "w+b");
    writeLines("shinyServer(", con = con);
    writeLines(deparse(dput(server)), con = con, sep = "\n");
    writeLines(")", con = con);
    close(con)
    serverFile <- gsub(x = serverFile, pattern = "\\\\", "/")

    uiFile <- file.path(shinyAppDir, "ui.R", fsep = "/")
    con <- file(uiFile, open = "w+b");
    writeLines("fluidPage(", con = con);
    writeLines(deparse(dput(fluidPageArgs)), con = con, sep = "\n");
    writeLines(")", con = con);
    close(con)

    message("server.R file is saved. Type: file.edit(\"", serverFile, "\")",
            " to edit the file, or runApp(\"", dirname(serverFile), "\") to run it,",
            " or, rsconnect::deployApp(\"", dirname(serverFile), "\")")
  } else {
    runApp(list(ui = fluidPage(fluidPageArgs), server = server),
           launch.browser = getOption("viewer", browseURL), quiet = TRUE)
  }
})
