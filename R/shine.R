#' Display covr results using a shiny app
#'
#' The shiny app is designed to provide local information to coverage
#' information similar to the coveralls.io website.  However it does not and
#' will not track coverage over time.
#' @param sim a simInit object
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
#' shine(mySim, "CFS")
#' }
#' @export
#' @import shiny #fluidPage titlePanel sidebarLayout actionButton sliderInput
shine <- function(sim, title, ...) UseMethod("shine")

shine.default <- function(sim, title="SpaDES App", ...) {
  stop("shine must be called on a simList object!", call. = FALSE)
}

#' @export
shine.simList <- function(sim, title, ...) {

  #loadNamespace("shiny")

  sessionEnv <- new.env()
  i = 1
  ui <- fluidPage(
    titlePanel(title),
    sidebarLayout(
      sidebarPanel(
        actionButton("goButton", "Run model"),

        sliderInput("simTimes", paste0("Simuated ",timeunit(sim)),
                    start(sim) , end(sim), c(start(sim), end(sim))),

        shiny::tabsetPanel(
          shiny::tabPanel("fire", uiOutput(outputId = "fireSpread")),
          shiny::tabPanel("caribou", uiOutput(outputId = "caribouMovement")),
          shiny::tabPanel("randomLandscapes", uiOutput(outputId = "randomLandscapes"))
        )#,
        #       uiOutput(outputId = "sliders")

        #sliderInput(paste0("param",1), names(params(sim)[[6]])[3], 10, 1000, 100),
        #sliderInput(paste0("param",2), paste0("Param",2), 0, 100, c(25,50)),
        #sliderInput(paste0("param",3), paste0("Param",3), 0, 100, c(25,50)),
        #sliderInput(paste0("param",4), paste0("Param",4), 0, 100, c(25,50))

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

#    for(j in unlist(modules(sim))[-(1:4)]) {
    j = "caribouMovement"
      output[[j]] <- renderUI({
        whSliders <- params(sim)[[j]]
        whSliders <- names(whSliders[sapply(whSliders, is.numeric)])
        lapply(whSliders, function(i) {
          sliderInput(
            inputId = paste0(j,"$",i),
            label = i,
            min = params(sim)[[j]][[i]]*0.5,
            max = params(sim)[[j]][[i]]*2,
            value = params(sim)[[j]][[i]],
            step =(params(sim)[[j]][[i]]*2 - params(sim)[[j]][[i]]*0.5)/10)
        })
      })

      k="fireSpread"
      output[[k]] <- renderUI({
        whSliders <- params(sim)[[k]]
        whSliders <- names(whSliders[sapply(whSliders, is.numeric)])
        lapply(whSliders, function(i) {
          sliderInput(
            inputId = paste0(k,"$",i),
            label = i,
            min = params(sim)[[k]][[i]]*0.5,
            max = params(sim)[[k]][[i]]*2,
            value = params(sim)[[k]][[i]],
            step =(params(sim)[[k]][[i]]*2 - params(sim)[[k]][[i]]*0.5)/10)
        })
      })

      l="randomLandscapes"
      output[[l]] <- renderUI({
        whSliders <- params(sim)[[l]]
        whSliders <- names(whSliders[sapply(whSliders, is.numeric)])
        lapply(whSliders, function(i) {
          sliderInput(
            inputId = paste0(l,"$",i),
            label = i,
            min = params(sim)[[l]][[i]]*0.5,
            max = params(sim)[[l]][[i]]*2,
            value = params(sim)[[l]][[i]],
            step =(params(sim)[[l]][[i]]*2 - params(sim)[[l]][[i]]*0.5)/10)
        })
      })

#      browser()
#    }
    #   for(i in 1:length(params(sim))) {
    #     output[[paste0("param",i)]] <- renderUI({
    #       selectInput(paste0("column",i), paste0("Param",i))
    #     })
    #   }

    #  output$coolplot <- renderPlot({
    #    clearPlot()
    #invalidateLater(0, session)
    #  })

    spadesCall <- eventReactive(input$goButton, {

      clearPlot()
      start(sim) <- input$simTimes[1]
      end(sim) <- input$simTimes[2]

#       for(i in names(params(sim)[[6]])) {
#         params(sim)[[6]][[i]] <- input[[paste0(names(params(sim))[6],"$",i)]]
#       }

      for(i in names(params(sim)[[4]][sapply(params(sim)[[4]], is.numeric)])) {
        if(!is.null(input[[paste0(names(params(sim))[4],"$",i)]]))
          params(sim)[[4]][[i]] <- input[[paste0(names(params(sim))[4],"$",i)]]
      }
      for(j in names(params(sim)[[5]][sapply(params(sim)[[5]], is.numeric)])) {
        if(!is.null(input[[paste0(names(params(sim))[5],"$",j)]]))
          params(sim)[[5]][[j]] <- input[[paste0(names(params(sim))[5],"$",j)]]
      }
      for(k in names(params(sim)[[6]][sapply(params(sim)[[6]], is.numeric)])) {
        if(!is.null(input[[paste0(names(params(sim))[6],"$",k)]]))
          params(sim)[[6]][[k]] <- input[[paste0(names(params(sim))[6],"$",k)]]
      }
      sim <- spades(sim)
    })

    output$coolplot <- renderPlot({
      spadesCall()
    })
    #   output$results <- renderTable({
    #     filtered()
    #   })
  }
#   ui <- shiny::fluidPage(
#     shiny::includeCSS(system.file("www/shiny.css", package = "covr")),
#     shiny::column(2,
#                   shiny::radioButtons("type", label = shiny::h3("Coverage Type"),
#                                       choices = setNames(names(data), to_title(names(data))))
#     ),
#     shiny::column(8,
#                   shiny::tabsetPanel(
#                     shiny::tabPanel("Files", DT::dataTableOutput(outputId = "file_table")),
#                     shiny::tabPanel("Source", addHighlight(shiny::tableOutput("source_table")))
#                   )
#     ),
#     title = paste(attr(sim, "package")$package, "Coverage"))
#
#   server <- function(input, output, session) {
#     output$file_table <- DT::renderDataTable(
#       data[[input$type]]$file_stats,
#       escape = FALSE,
#       options = list(searching = FALSE, dom = "t", paging = FALSE),
#       rownames = FALSE,
#       callback = DT::JS("table.on('click.dt', 'a', function() {
#         Shiny.onInputChange('filename', $(this).text());
#         $('ul.nav a[data-value=Source]').tab('show');
#       });"))
#     shiny::observe({
#       if (!is.null(input$filename)) {
#         output$source_table <- renderSourceTable(data[[input$type]]$full[[input$filename]])
#       }
#     })
#   }

  shiny::runApp(list(ui = ui, server = server),
                launch.browser = getOption("viewer", utils::browseURL),
                quiet = TRUE
  )
}


