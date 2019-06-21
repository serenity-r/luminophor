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
    shinyjs::inlineCSS("
    .widget-header i {
      float: right;
      margin-top: 0.5rem;
      margin-right: 4px;
      margin-left: 2px;
    }

    .widget-header .form-group {
      float: right;
      margin-right: 0;
    }

    .pretty {
      margin-right: 0;
      line-height: 1.6rem;
    }

    .pretty.p-toggle .state.p-off .icon {
      color: #000;
    }"),
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
    phosphorr() %>%
      addWidget(id = 'widget-myslider',
                caption = "This is my slider",
                icon = icon("layer-group"),
                ui = uiOutput('widget-myslider'),
                title = "Slider") %>%
      addWidget(id = 'widget-myplot',
                icon = icon("image"),
                refwidget = 'widget-myslider',
                insertmode = "split-right",
                relsize = 0.6,
                ui = plotOutput('myplot'),
                title = "Histogram") %>%
      addWidget(id = "code",
                refwidget = 'widget-myplot',
                insertmode = "split-bottom",
                relsize = 0.25,
                ui = HTML("Code"),
                title = "Code",
                icon = icon("code")) %>%
      addWidget(id = "vars",
                refwidget = 'widget-myslider',
                insertmode = "split-bottom",
                relsize = 0.75,
                ui = HTML("Vars"),
                title = "Variables",
                icon = icon("database")) %>%
      addWidget(id = "aes",
                refwidget = "vars",
                insertmode = "split-right",
                ui = HTML("Aesthetics"),
                title = "Aesthetics",
                icon = icon("paint-brush")) %>%
      addWidget(id = "messages",
                refwidget = "code",
                insertmode = "tab-after",
                ui = HTML("Messages"),
                title = "Messages",
                icon = icon("info")) %>%
      addWidget(id = "help",
                refwidget = "messages",
                insertmode = "tab-after",
                ui = HTML("Help"),
                title = "Help",
                icon = icon("question"))
  })

  output$`widget-myslider` <- renderUI({
    tagList(
      widgetHeader(
        shinyWidgets::prettyToggle(
          inputId = "maximize",
          label_on = "",
          label_off = "",
          status_on = "default",
          status_off = "default",
          outline = TRUE,
          plain = TRUE,
          icon_on = icon("window-minimize"),
          icon_off = icon("window-maximize"),
          inline = TRUE
        )
      ),
      widgetBody(
        sliderInput('myslider', "Number of observations:", 1, 100, 50)
      )
    )
  })

  output$myheader <- renderUI({
  })

  observeEvent(input$maximize, {
    message <- list(
      dockID = "pjsbox",
      widgetID = "widget-myslider"
    )
    if (input$maximize) {
      session$sendCustomMessage("phosphorr:maximizeWidget", message)
    } else {
      session$sendCustomMessage("phosphorr:minimizeWidget", message)
    }
  })

  output$myplot <- renderPlot({
    req(input$myslider)
    mydata <- histdata[seq_len(input$myslider)]
    hist(mydata)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

