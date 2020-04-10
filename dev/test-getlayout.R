#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(magrittr)
library(luminophor)

# Define UI for application that draws a histogram
library(shinydashboard)

`%||%` <- function(a, b) if (!is.null(a)) a else b

ui <- dashboardPage(
  dashboardHeader(title = "Sample Get Layout"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    ),
    actionButton("btn","Create widgets"),
    actionButton("remove_btn","Remove widgets"),
    actionButton("layout_btn", "Get layout")
  ),
  dashboardBody(
    fluidRow(
      column(9, luminophorOutput("lmobox", height="90vh")),
      column(3, verbatimTextOutput("placeholder"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$lmobox <- renderLuminophor({
    luminophor()
  })

  observeEvent(input$btn, {
    ns <- NS(input$btn)

    luminophorProxy("lmobox") %>%

      addWidget(id = "widget",
                insertmode = "tab-after",
                title = "Widget")

  })

  observeEvent(input$remove_btn, {
    luminophorProxy("lmobox") %>% removeWidgets(.all = TRUE)
  })

  #output$placeholder <- renderText({ input$lmobox_layout })
  #output$placeholder <- renderUI({ input$lmobox_layout })
  output$placeholder <- renderPrint({ input$lmobox_layout })

  observeEvent(input$layout_btn, {
    luminophorProxy("lmobox") %>% getLayout()
    #browser()
  })

}

# Run the application
shinyApp(ui = ui, server = server)

