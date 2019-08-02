#' PhosphorR: HTMLWidget implementation of PhosphorJS layout manager
#'
#' @docType package
#'
#' @importFrom magrittr "%>%"
#'
#' @name PhosphorR-package
#'
#' @seealso \code{\link{phosphorr}}, \code{\link{phosphorrOutput}}
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(phosphorr)
#'   shinyApp(
#'     ui = fluidPage(
#'       titlePanel("Old Faithful Geyser Data"),
#'       fluidRow(column(12, phosphorrOutput('pjs', height='90vh')))
#'     ),
#'     server = function(input, output) {
#'       output$pjs <- renderPhosphorr(
#'         phosphorr() %>%
#'           addWidget("widget-slider",
#'                     title = "Slider",
#'                     ui = sliderInput("bins", "Number of bins:",
#'                                      min = 1, max = 50, value = 30)) %>%
#'           addWidget("widget-plot",
#'                     title = "Plot",
#'                     insertmode = "split-right",
#'                     refwidgetID = "widget-slider",
#'                     relsize = 0.75,
#'                     ui = plotOutput("distPlot"))
#'       )
#'
#'       output$distPlot <- renderPlot({
#'         x <- faithful[, 2]
#'         bins <- seq(min(x), max(x), length.out = input$bins + 1)
#'         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#'       })
#'     }
#'   )
#' }
NULL

#' Create a PhosphorR dock htmlwidget
#'
#' This function creates a PhosphorR dock using htmlwidgets. The widget can be
#' rendered on HTML pages generated from R Markdown, Shiny, or other applications.
#'
#' @param items Options for htmlwidget (currently not used in this function)
#' @param width,height Fixed width/height for widget (in css units). The default
#'   is NULL, which results in intelligent automatic sizing based on the widget's
#'   container. (currently not used in this function - see
#'   \code{\link{phosphorrOutput}} to set size specifications)
#' @param elementId Use an explicit element ID for the widget (rather than an
#'   automatically generated one). Useful if you have other JavaScript that
#'   needs to explicitly discover and interact with a specific widget instance.
#'
#' @import htmlwidgets
#'
#' @export
phosphorr <- function(items = NULL, width = "100%", height = "72vh", elementId = NULL) {

  # Default options
  options = list(widgets = list())

  if (!is.null(items)) {
    x = list(
      items = utils::modifyList(options, items)
    )
  } else {
    x = list(
      items = options
    )
  }

  # create widget
  htmlwidgets::createWidget(
    name = 'phosphorr',
    x,
    width = width,
    height = height,
    package = 'phosphorr',
    elementId = elementId
  )
}

#' Helper functions for using PhosphorJS in Shiny
#'
#' These two functions are like most fooOutput() and renderFoo() functions in
#' the shiny package. The former is used to create an html container for a
#' PhosphorJS dock panel, and the latter is used in the server logic to render
#' the PhosphorJS dock panel.
#'
#' @param outputId ID of htmlwidgets container
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression to create a PhosphorJS dock htmlwidget (normally
#'   via \code{\link{phosphorr}()}).
#' @param env The environment in which to evaluate expr.
#' @param quoted Is expr a quoted expression (with \code{quote()})? This is
#'   useful if you want to save an expression in a variable.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(phosphorr)
#'   shinyApp(
#'     ui = fluidPage(
#'       fluidRow(column(12, phosphorrOutput('pjs', height='90vh')))
#'     ),
#'     server = function(input, output) {
#'       output$pjs <- renderPhosphorr(
#'         phosphorr() %>%
#'           addWidget("mywidget")
#'       )
#'     }
#'   )
#' }
#'
#' @name phosphorrOutput
NULL

#' @rdname phosphorrOutput
#'
#' @export
phosphorrOutput <- function(outputId, width = '100%', height = 'auto'){
  htmlwidgets::shinyWidgetOutput(outputId, 'phosphorr', width, height, package = 'phosphorr')
}

#' @rdname phosphorrOutput
#'
#' @export
renderPhosphorr <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, phosphorrOutput, env, quoted = TRUE)
}

# Add custom HTML to wrap the widget (called automatically by createWidget)
phosphorr_html <- function(id, style, class, ...) {
  htmltools::tags$div(
    id = id, class = class, style = style,
    htmltools::tags$div(class = "phosphorr-shim")
  )
}

#' Create a PhosphorR proxy object
#'
#' @param id Name of the PhosphorR htmlwidget
#' @param session Valid session object
#'
#' @return Proxy PhosphorR object
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(phosphorr)
#'   shinyApp(
#'     ui = fluidPage(
#'       sidebarLayout(
#'         sidebarPanel(
#'           actionButton('add', 'Add Widgets', icon = icon('plus'))
#'         ),
#'         mainPanel(
#'           phosphorrOutput('pjs', height='90vh')
#'         )
#'       )
#'     ),
#'     server = function(input, output) {
#'       output$pjs <- renderPhosphorr(
#'         phosphorr()
#'       )
#'
#'       observeEvent(input$add, {
#'         phosphorrProxy('pjs') %>%
#'           addWidget("mywidget")
#'       })
#'     }
#'   )
#' }
#'
#' @export
phosphorrProxy <- function(id, session = shiny::getDefaultReactiveDomain()) {
  object        <- list( id = id, session = session )
  class(object) <- "phosphorrProxy"

  return(object)
}

#' Adds a new widget to the dock
#'
#' @param proxy Either output from the \code{\link{phosphorr}} function, or a
#'   \link[=phosphorrProxy]{Proxy PhosphorR object}
#' @param id ID for phosphorr widget
#' @param title Title for phosphorr widget
#' @param caption Caption for phosphorr widget
#' @param icon Font Awesome icon (specified via the \code{icon} function)
#' @param closable Create removable
#' @param insertmode How should the widget be added? Options include \code{split-right}, \code{split-left},
#'   \code{split-bottom}, \code{split-top}, \code{tab-before}, and \code{tab-after} (default)
#' @param refwidgetID Reference widget ID for \code{insertmode} action
#' @param relsize Relative size of widget (between 0 and 1) in relation to \code{refwidget} (or last widget
#'   if \code{refwidget} isn't specified)
#' @param ui UI content.  If just text, need to use HTML(...)
#'
#' @seealso \code{\link{removeWidgets}}
#'
#' @examples
#' # Add widget on render
#' if (interactive()) {
#'   library(shiny)
#'   library(phosphorr)
#'   shinyApp(
#'     ui = fluidPage(
#'       fluidRow(column(12, phosphorrOutput('pjs', height='90vh')))
#'     ),
#'     server = function(input, output) {
#'       output$pjs <- renderPhosphorr(
#'         phosphorr() %>%
#'           addWidget("mywidget")
#'       )
#'     }
#'   )
#' }
#'
#' # Add widget on event
#' if (interactive()) {
#'   library(shiny)
#'   library(phosphorr)
#'   shinyApp(
#'     ui = fluidPage(
#'       sidebarLayout(
#'         sidebarPanel(
#'           actionButton('add', 'Add Widgets', icon = icon('plus'))
#'         ),
#'         mainPanel(
#'           phosphorrOutput('pjs', height='90vh')
#'         )
#'       )
#'     ),
#'     server = function(input, output) {
#'       output$pjs <- renderPhosphorr(
#'         phosphorr()
#'       )
#'
#'       observeEvent(input$add, {
#'         phosphorrProxy('pjs') %>%
#'           addWidget("mywidget")
#'       })
#'     }
#'   )
#' }
#'
#' @return \link[=phosphorrProxy]{Proxy PhosphorR object}
#' @export
addWidget <- function(proxy,
                      id,
                      title = "Widget",
                      caption = "Widget",
                      icon = "",
                      closable = TRUE,
                      insertmode = "tab-after",
                      refwidgetID = NULL,
                      relsize = NULL,
                      ui = shiny::HTML("I am a widget!")) {

  # Process icon
  iconClass <- ifelse(class(icon) == "shiny.tag", icon$attribs$class, icon)

  server <- ((class(ui) == "shiny.tag") && (ui$attribs$class == "shiny-html-output"))
  if (server) {
    # Trigger warning if uiOutput and id doesn't match widgetID
    if (ui$attribs$id != id) {
      warning("Changing uiOutput ID to match widget ID.  Please change server-side renderUI accordingly.")
    }
    ui <- '' # Won't need this
  }

  if (all(c("phosphorr", "htmlwidget") %in% class(proxy))) {
    # Add widget later on render
    proxy$x$items$widgets <- c(proxy$x$items$widgets, list(
      list(
        widgetID = id,
        title = title,
        caption = caption,
        iconClass = iconClass,
        closable = closable,
        insertmode = insertmode,
        refwidgetID = refwidgetID,
        relsize = relsize,
        server = server,
        ui = processDeps(ui, proxy$session)
      )
    ))
  } else {
    # Add widget to already rendered dock
    data <- list(dockID = proxy$id,
                 widgetID = id,
                 title = title,
                 caption = caption,
                 iconClass = iconClass,
                 closable = closable,
                 insertmode = insertmode,
                 refwidgetID = refwidgetID,
                 relsize = relsize,
                 server = server
    )

    # Namespacing to avoid conflicts (http://deanattali.com/blog/htmlwidgets-tips/)
    proxy$session$sendCustomMessage("phosphorr:addWidget", data)

    shiny::insertUI(
      selector = paste(
        paste0("#", data$dockID),
        paste0("#", data$widgetID, '.widget-content')
      ),
      ui = ui
    )
  }

  return(proxy)
}

#' Maximize or minimize a widget in the dock
#'
#' Technically, minimizing a widget is more like un-maximizing a widget.  When
#' "minimized," the widget will return to its original position in the layout.
#' When a widget is maximized, it will take up the full size of the dock.
#'
#' @param proxy \link[=phosphorrProxy]{Proxy PhosphorR object}
#' @param widgetId ID for phosphorr widget
#'
#' @return \link[=phosphorrProxy]{Proxy PhosphorR object}
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(phosphorr)
#'   shinyApp(
#'     ui = fluidPage(
#'       titlePanel("Old Faithful Geyser Data"),
#'       sidebarLayout(
#'         sidebarPanel(
#'           actionButton('maximize', 'Maximize Plot', icon = icon('window-maximize')),
#'           actionButton('minimize', 'Minimize Plot', icon = icon('window-minimize'))
#'         ),
#'         mainPanel(
#'           phosphorrOutput('pjs', height='90vh')
#'         )
#'       )
#'     ),
#'     server = function(input, output) {
#'       output$pjs <- renderPhosphorr(
#'         phosphorr() %>%
#'           addWidget("widget-slider",
#'                     title = "Slider",
#'                     ui = sliderInput("bins", "Number of bins:",
#'                                      min = 1, max = 50, value = 30)) %>%
#'           addWidget("widget-plot",
#'                     title = "Plot",
#'                     insertmode = "split-right",
#'                     refwidgetID = "widget-slider",
#'                     relsize = 0.75,
#'                     ui = plotOutput("distPlot"))
#'       )
#'
#'       output$distPlot <- renderPlot({
#'         x <- faithful[, 2]
#'         bins <- seq(min(x), max(x), length.out = input$bins + 1)
#'         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#'       })
#'
#'       observeEvent(input$maximize, {
#'         phosphorrProxy('pjs') %>%
#'           maximizeWidget('widget-plot')
#'       })
#'
#'       observeEvent(input$minimize, {
#'         phosphorrProxy('pjs') %>%
#'           minimizeWidget('widget-plot')
#'       })
#'     }
#'   )
#' }
#'
#' @name maximizeWidget
NULL

#' @rdname maximizeWidget
#'
#' @export
maximizeWidget <- function(proxy,
                           widgetId) {
  if (all(c("phosphorr", "htmlwidget") %in% class(proxy))) {
    stop("Maximizing widgets can only occur after render.")
  } else {
    data <- list(dockID = proxy$id, widgetID = widgetId)
    proxy$session$sendCustomMessage("phosphorr:maximizeWidget", data)
  }

  return(proxy)
}

#' @rdname maximizeWidget
#'
#' @export
minimizeWidget <- function(proxy,
                           widgetId) {
  if (all(c("phosphorr", "htmlwidget") %in% class(proxy))) {
    stop("Minimizing widgets can only occur after render.")
  } else {
    data <- list(dockID = proxy$id, widgetID = widgetId)
    proxy$session$sendCustomMessage("phosphorr:minimizeWidget", data)
  }

  return(proxy)
}

#' Remove widgets in the dock
#'
#' @param proxy \link[=phosphorrProxy]{Proxy PhosphorR object}
#' @param widgetIDs IDs for phosphorr widget
#' @param .all Remove all widgets?  This will override any ids specified in \code{widgetIDs}
#'
#' @return \link[=phosphorrProxy]{Proxy PhosphorR object}
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(phosphorr)
#'   shinyApp(
#'     ui = fluidPage(
#'       titlePanel("Old Faithful Geyser Data"),
#'       sidebarLayout(
#'         sidebarPanel(
#'           actionButton('rerender', 'Rerender', icon = icon('redo')),
#'           actionButton('remove_slider', 'Remove Slider', icon = icon('times')),
#'           actionButton('remove_all', 'Remove All', icon = icon('times'))
#'         ),
#'         mainPanel(
#'           phosphorrOutput('pjs', height='90vh')
#'         )
#'       )
#'     ),
#'     server = function(input, output) {
#'       output$pjs <- renderPhosphorr({
#'         input$rerender
#'         initval <- isolate({if (!is.null(input$bins)) input$bins else 30})
#'
#'         phosphorr() %>%
#'           addWidget("widget-slider",
#'                     title = "Slider",
#'                     ui = sliderInput("bins", "Number of bins:",
#'                                      min = 1, max = 50, value = initval)) %>%
#'           addWidget("widget-plot",
#'                     title = "Plot",
#'                     insertmode = "split-right",
#'                     refwidgetID = "widget-slider",
#'                     relsize = 0.75,
#'                     ui = plotOutput("distPlot"))
#'       })
#'
#'       output$distPlot <- renderPlot({
#'         x <- faithful[, 2]
#'         bins <- seq(min(x), max(x), length.out = input$bins + 1)
#'         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#'       })
#'
#'       observeEvent(input$remove_slider, {
#'         phosphorrProxy('pjs') %>%
#'           removeWidgets('widget-slider')
#'       })
#'
#'       observeEvent(input$remove_all, {
#'         phosphorrProxy('pjs') %>%
#'           removeWidgets(.all = TRUE)
#'       })
#'     }
#'   )
#' }
#'
#' @export
removeWidgets <- function(proxy,
                          widgetIDs = NULL,
                          .all = FALSE) {

  if (.all && (length(widgetIDs) > 0)) {
    warning("Duplicate specifications: .all and widgetIDs are both specified. All widgets will be removed.")
  }

  if (all(c("phosphorr", "htmlwidget") %in% class(proxy))) {
    stop("Removing widgets can only occur after render.")
  } else {
    data <- list(dockID = proxy$id, widgetIDs = widgetIDs, all = .all)
    proxy$session$sendCustomMessage("phosphorr:removeWidgets", data)
  }

  return(proxy)
}

# Given a Shiny tag object, process singletons and dependencies. Returns a list
# with rendered HTML and dependency objects.
# Ref: https://github.com/rstudio/shiny/blob/353615da897bb6015ca805ae3c830324c1dad95f/R/html-deps.R#L43
processDeps <- function(tags, session) {
  ui <- htmltools::takeSingletons(tags, session$singletons, desingleton=FALSE)$ui
  ui <- htmltools::surroundSingletons(ui)
  dependencies <- lapply(
    htmltools::resolveDependencies(htmltools::findDependencies(ui)),
    shiny::createWebDependency
  )
  names(dependencies) <- NULL

  list(
    html = htmltools::doRenderTags(ui),
    deps = dependencies
  )
}
