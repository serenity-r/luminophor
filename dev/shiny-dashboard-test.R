#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
library(phosphorr)

# Define UI for application that draws a histogram
library(shinydashboard)

myWidget <- function(gridstackrProxy,
                     id,
                     ui = HTML("Hello, World!"),
                     title = "Chart Title") {

  content <- tagList(
    tags$div(
      class = "chart-title",
      tags$span(title),
      tags$div(
        class = "action-icons",
        icon("minus", class = "gs-minimize-handle"),
        icon("close", class = "gs-remove-handle")
      )
    ),
    tags$div(class = "chart-stage",
             tags$div(class = "chart-shim"))
  )

  return(addWidget(gridstackrProxy,
                   id = id,
                   content = content,
                   ui = ui,
                   uiWrapperClass = ".chart-shim"))
}

ui <- dashboardPage(
  dashboardHeader(title = "Sample Layout"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Pipeline", tabName = "pipeline", icon = icon("sitemap")),
      menuItem("Code", tabName = "code", icon = icon("code"))
    ),
    actionButton("btn","Create widgets")
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              phosphorrOutput("pjsbox", height="90vh") # Hacky height, but it works
      ),

      # Second tab content
      tabItem(tabName = "pipeline",
              h2("Pipeline tab content")
      ),

      # Third tab content
      tabItem(tabName = "code",
              h2("Code tab content")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)

  output$pjsbox <- renderPhosphorr({
    phosphorr()
  })

  observeEvent(input$btn, {
    ns <- NS(input$btn)

    phosphorrProxy("pjsbox") %>%
      addWidget(id = paste0('widget', ns('slider')),
                ui = sliderInput(ns('slider'), "Number of observations:", 1, 100, 50),
                title = "Slider") %>%
      addWidget(id = paste0('widget-', ns('plot')),
                ui = plotOutput(ns('plot'), height = 250),
                title = "Histogram")

    output[[ns('plot')]] <- renderPlot({
      data <- histdata[seq_len(input[[ns('slider')]])]
      hist(data)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

