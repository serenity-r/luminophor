#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(luminophor)
library(magrittr)

ui <- fluidPage(
  titlePanel("LuminophoR Tabsets"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select the random distribution type ----
      radioButtons("dist", "Distribution type:",
                   c("Normal" = "norm",
                     "Uniform" = "unif",
                     "Log-normal" = "lnorm",
                     "Exponential" = "exp")),

      # br() element to introduce extra vertical spacing ----
      br(),

      # Input: Slider for the number of observations to generate ----
      sliderInput("n",
                  "Number of observations:",
                  value = 500,
                  min = 1,
                  max = 1000),

      actionButton("btn","Create widgets")
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      luminophorOutput("lmodock", height="90vh") # Hacky height, but it works
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)

    dist(input$n)
  })

  output$lmodock <- renderLuminophor({
    luminophor()
  })

  observeEvent(input$btn, {
    ns <- NS(input$btn)

    luminophorProxy("lmodock") %>%
      addWidget(id = paste0('widget-', ns('plot')),
                ui = plotOutput(ns('plot')),
                title = "Histogram") %>%
      addWidget(id = paste0('widget-', ns('summary')),
                ui = verbatimTextOutput(ns('summary')),
                title = "Summary") %>%
      addWidget(id = paste0('widget-', ns('table')),
                ui = tableOutput(ns('table')),
                title = "Table")

    # Generate a plot of the data ----
    # Also uses the inputs to build the plot label. Note that the
    # dependencies on the inputs and the data reactive expression are
    # both tracked, and all expressions are called in the sequence
    # implied by the dependency graph.
    output[[ns('plot')]] <- renderPlot({
      dist <- input$dist
      n <- input$n

      hist(d(),
           main = paste("r", dist, "(", n, ")", sep = ""),
           col = "#75AADB", border = "white")
    })

    # Generate a summary of the data ----
    output[[ns('summary')]] <- renderPrint({
      summary(d())
    })

    # Generate an HTML table view of the data ----
    output[[ns('table')]] <- renderTable({
      d()
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
