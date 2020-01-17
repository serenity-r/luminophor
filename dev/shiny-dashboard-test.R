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
  dashboardHeader(title = "Sample Layout"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Pipeline", tabName = "pipeline", icon = icon("sitemap")),
      menuItem("Code", tabName = "code", icon = icon("code"))
    ),
    actionButton("btn","Create widgets"),
    actionButton("remove_btn","Remove tabbed widgets")
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              luminophorOutput("lmobox", height="90vh") # Hacky height, but it works
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

  output$lmobox <- renderLuminophor({
    luminophor()
  })

  observeEvent(input$btn, {
    ns <- NS(input$btn)

    luminophorProxy("lmobox") %>%
      addWidget(id = paste0('widget', ns('slider')),
                ui = sliderInput(ns('slider'), "Number of observations:", 1, 100, 50),
                title = "Slider") %>%
      addWidget(id = paste0('widget-', ns('plot')),
                refwidget = paste0('widget', ns('slider')),
                insertmode = "split-right",
                relsize = 0.6,
                ui = plotOutput(ns('plot')),
                title = "Histogram") %>%
      addWidget(id = "code",
                refwidget = paste0('widget-', ns('plot')),
                insertmode = "split-bottom",
                relsize = 0.25,
                ui = HTML("Code"),
                title = "Code") %>%
      addWidget(id = "vars",
                refwidget = paste0('widget', ns('slider')),
                insertmode = "split-bottom",
                relsize = 0.75,
                ui = HTML("Vars"),
                title = "Variables") %>%
      addWidget(id = "aes",
                refwidget = "vars",
                insertmode = "split-right",
                ui = HTML("Aesthetics"),
                title = "Aesthetics") %>%
      addWidget(id = "messages",
                refwidget = "code",
                insertmode = "tab-after",
                ui = HTML("Messages"),
                title = "Messages") %>%
      addWidget(id = "help",
                refwidget = "messages",
                insertmode = "tab-after",
                ui = HTML("Help"),
                title = "Help")

    output[[ns('plot')]] <- renderPlot({
      mydata <- histdata[seq_len(input[[ns('slider')]])]
      hist(mydata)
    })
  })

  observeEvent(input$remove_btn, {
    luminophorProxy("lmobox") %>% removeWidgets(.all = TRUE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

