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

  observeEvent(input$jsRemove, {
    showModal(modalDialog(
      tags$b("Are you sure you want to close this widget?"),
      footer = tagList(
        modalButton("No"),
        actionButton("remove", "Yes, close widget")
      ),
      size = "s"
    ))
  })

  observeEvent(input$remove, {
    gridstackrProxy(input$jsRemove$gridID) %>%
      removeWidget(id = input$jsRemove$itemID,
                   uiWrapperClass = '.chart-shim')
    removeModal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

