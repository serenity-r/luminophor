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

ui <- dashboardPage(
  dashboardHeader(title = "Sample Layout"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Pipeline", tabName = "pipeline", icon = icon("sitemap")),
      menuItem("Code", tabName = "code", icon = icon("code"))
    )
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

  session$onFlushed(function() {
    observe({
      luminophorProxy("lmobox") %>%
        addWidget(id = 'widget-myslider',
                  body = sliderInput('myslider', "Number of observations:", 1, 100, 50),
                  title = "Slider") %>%
        addWidget(id = 'widget-myplot',
                  refwidget = 'widget-myslider',
                  insertmode = "split-right",
                  relsize = 0.6,
                  body = plotOutput('myplot'),
                  title = "Histogram") %>%
        addWidget(id = "code",
                  refwidget = paste0('widget-myplot'),
                  insertmode = "split-bottom",
                  relsize = 0.25,
                  body = HTML("Code"),
                  title = "Code") %>%
        addWidget(id = "vars",
                  refwidget = paste0('widget-myslider'),
                  insertmode = "split-bottom",
                  relsize = 0.75,
                  body = HTML("Vars"),
                  title = "Variables") %>%
        addWidget(id = "aes",
                  refwidget = "vars",
                  insertmode = "split-right",
                  body = HTML("Aesthetics"),
                  title = "Aesthetics") %>%
        addWidget(id = "messages",
                  refwidget = "code",
                  insertmode = "tab-after",
                  body = HTML("Messages"),
                  title = "Messages") %>%
        addWidget(id = "help",
                  refwidget = "messages",
                  insertmode = "tab-after",
                  body = HTML("Help"),
                  title = "Help")
    })
  })

  output$myplot <- renderPlot({
    req(input$myslider)
    mydata <- histdata[seq_len(input$myslider)]
    hist(mydata)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

