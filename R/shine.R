#' Display covr results using a shiny app
#'
#' The shiny app is designed to provide local information to coverage
#' information similar to the coveralls.io website.  However it does not and
#' will not track coverage over time.
#' @param x a simInit object
#' @param ... additional arguments passed to methods
#' @examples
#' \dontrun{
#' x <- package_coverage()
#' shine(x)
#' }
#' @export
shine <- function(x, ...) UseMethod("shine")

shine.default <- function(x, ...) {
  stop("shine must be called on a simInit object!", call. = FALSE)
}

#' @export
shine.simInit <- function(x, ...) {

  loadNamespace("shiny")

  data <- lapply(x, to_shiny_data)

  ui <- shiny::fluidPage(
    shiny::includeCSS(system.file("www/shiny.css", package = "covr")),
    shiny::column(2,
                  shiny::radioButtons("type", label = shiny::h3("Coverage Type"),
                                      choices = setNames(names(data), to_title(names(data))))
    ),
    shiny::column(8,
                  shiny::tabsetPanel(
                    shiny::tabPanel("Files", DT::dataTableOutput(outputId = "file_table")),
                    shiny::tabPanel("Source", addHighlight(shiny::tableOutput("source_table")))
                  )
    ),
    title = paste(attr(x, "package")$package, "Coverage"))

  server <- function(input, output, session) {
    output$file_table <- DT::renderDataTable(
      data[[input$type]]$file_stats,
      escape = FALSE,
      options = list(searching = FALSE, dom = "t", paging = FALSE),
      rownames = FALSE,
      callback = DT::JS("table.on('click.dt', 'a', function() {
        Shiny.onInputChange('filename', $(this).text());
        $('ul.nav a[data-value=Source]').tab('show');
      });"))
    shiny::observe({
      if (!is.null(input$filename)) {
        output$source_table <- renderSourceTable(data[[input$type]]$full[[input$filename]])
      }
    })
  }

  shiny::runApp(list(ui = ui, server = server),
                launch.browser = getOption("viewer", utils::browseURL),
                quiet = TRUE
  )
}


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

       shiny::tabsetPanel(
         shiny::tabPanel("caribou", uiOutput(outputId = "sliders")),
         shiny::tabPanel("randomLandscapes", uiOutput(outputId = "sliders2"))
       )#,
#       uiOutput(outputId = "sliders")

       #sliderInput(paste0("param",1), names(params(mySim)[[6]])[3], 10, 1000, 100),
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

  output$sliders <- renderUI({
    whSliders <- names(params(mySim)[[6]])
    lapply(whSliders, function(i) {
      sliderInput(
        inputId = paste0(names(params(mySim))[6],"$",i),
        label = i,
        min = params(mySim)[[6]][[i]]*0.5,
        max = params(mySim)[[6]][[i]]*2,
        value = params(mySim)[[6]][[i]],
        step =1)
    })
  })

  output$sliders2 <- renderUI({
    whSliders <- params(mySim)[[4]]
    whSliders <- names(whSliders[sapply(whSliders, is.numeric)])
    lapply(whSliders, function(i) {
      sliderInput(
        inputId = paste0(names(params(mySim))[4],"$",i),
        label = i,
        min = params(mySim)[[4]][[i]]*0.5,
        max = params(mySim)[[4]][[i]]*2,
        value = params(mySim)[[4]][[i]],
        step =1)
    })
  })
  #   for(i in 1:length(params(mySim))) {
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
    for(i in names(params(mySim)[[6]])) {
      params(mySim)[[6]][[i]] <- input[[paste0(names(params(mySim))[6],"$",i)]]
    }

    for(i in names(params(mySim)[[4]][sapply(params(mySim)[[4]], is.numeric)])) {
      if(!is.null(input[[paste0(names(params(mySim))[4],"$",i)]]))
        params(mySim)[[4]][[i]] <- input[[paste0(names(params(mySim))[4],"$",i)]]
    }
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



