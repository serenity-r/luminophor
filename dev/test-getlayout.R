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
library(rmarkdown)

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
      column(3, verbatimTextOutput("placeholder", placeholder = TRUE),
             verbatimTextOutput("layout", placeholder = TRUE))

    )
  )
)

#Server
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

  #output$placeholder <- renderPrint({ input$lmobox_validFlexdash })
  output$placeholder <- renderText({ input$lmobox_validFlexdash })
  output$layout <- renderPrint({ input$lmobox_layout })

  observeEvent(input$layout_btn, {
    luminophorProxy("lmobox") %>% getLayout()
    #browser()
  })

}



  #https://shiny.rstudio.com/articles/generating-reports.html

  # output$download_layout <- downloadHandler(
  #   filename = "layout_template.pdf",
  #   content = function(file) {
  #     tempReport <- file.path(tempdir(), "layout_template.Rmd")
  #     file.copy("layout_template.Rmd", tempReport, overwrite = TRUE)
  #     params <- list(n = input$slider)
  #     rmarkdown::render(tempReport,
  #                       output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = golbalenv())
       # )

  #https://shiny.rstudio.com/gallery/download-knitr-reports.html

    #  src <- normalizePath('layout.Rmd')
    #  owd <- setwd(tempdir())
     # on.exit(setwd(owd))
     # file.copy(src, 'layout.Rmd', overwrite = TRUE)

     # library(rmarkdown)
     # out <- render('layout.Rmd')
     # file.rename(out, file)
    # }
  # )

# Run the application
shinyApp(ui = ui, server = server)

