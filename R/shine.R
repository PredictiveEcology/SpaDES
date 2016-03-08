#' Display covr results using a shiny app
#'
#' The shiny app is designed to provide local information to coverage
#' information similar to the coveralls.io website.  However it does not and
#' will not track coverage over time.
#' @param sim a simInit object
#' @param title character string. The title of the shiny page.
#' @param ... additional arguments passed to methods
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
#' @export
#' @importFrom shiny fluidPage titlePanel sidebarPanel sidebarLayout actionButton sliderInput uiOutput
#' @importFrom shiny mainPanel plotOutput renderUI tabPanel tabsetPanel
#' @importFrom shiny eventReactive renderPlot runApp
shine <- function(sim, title, ...) UseMethod("shine")

shine.default <- function(sim, title="SpaDES App", ...) {
  stop("shine must be called on a simList object!", call. = FALSE)
}

#' @export
shine.simList <- function(sim, title="SpaDES App", ...) {

  sessionEnv <- new.env()
  i = 1
  ui <- fluidPage(
    titlePanel(title),
    sidebarLayout(
      sidebarPanel(
        actionButton("goButton", "Run model"),

        sliderInput("simTimes", paste0("Simuated ",timeunit(sim)),
                    start(sim) , end(sim), c(start(sim), end(sim))),

        uiOutput("tabPanels")
      ),
      mainPanel(
        plotOutput("spadesPlot")
      )
    )
  )

  server <- function(input, output, session) {

    output$tabPanels = renderUI({
      mods <- unlist(modules(sim))[-(1:4)]
      nTabs = length(mods)
      myTabs = lapply(mods, function(x) tabPanel(x, uiOutput(outputId = x)))
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
              step =(params(sim)[[kLocal]][[i]]*2 - params(sim)[[kLocal]][[i]]*0.5)/10)
          })
        })
      })
    }

    spadesCall <- eventReactive(input$goButton, {
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

      sim <- spades(sim)
    })

    output$spadesPlot <- renderPlot({
      spadesCall()
    })
  }

  runApp(list(ui = ui, server = server),
                launch.browser = getOption("viewer", browseURL),
                quiet = TRUE
  )
}
