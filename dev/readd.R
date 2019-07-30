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
library(phosphorr)

# Define UI for application that draws a histogram
library(shinydashboard)

`%||%` <- function(a, b) if (!is.null(a)) a else b

ui <- dashboardPage(
  dashboardHeader(title = "Sample Layout"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    ),
    actionButton("btn","Create widgets"),
    actionButton("remove_btn","Remove tabbed widgets")
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              phosphorrOutput("pjsbox", height="90vh") # Hacky height, but it works
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
    phosphorrProxy("pjsbox") %>%
      addWidget(id = 'widget-slider',
                ui = sliderInput('slider', "Number of observations:", 1, 100, input$slider %||% 50),
                title = "Slider") %>%
      addWidget(id = 'widget-hist',
                refwidget = 'widget-slider',
                insertmode = "split-right",
                relsize = 0.6,
                ui = plotOutput('plot'),
                title = "Histogram")

    output$plot <- renderPlot({
      mydata <- histdata[seq_len(input$slider)]
      hist(mydata)
    })
  })

  observeEvent(input$remove_btn, {
    phosphorrProxy("pjsbox") %>% removeWidgets(.all = TRUE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

