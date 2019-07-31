#' <Add Title>
#'
#' <Add Description>
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

#' Shiny bindings for phosphorr
#'
#' Output and render functions for using phosphorr within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a phosphorr
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name phosphorr-shiny
#'
#' @export
phosphorrOutput <- function(outputId, width = '100%', height = 'auto'){
  htmlwidgets::shinyWidgetOutput(outputId, 'phosphorr', width, height, package = 'phosphorr')
}

#' @rdname phosphorr-shiny
#' @export
renderPhosphorr <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, phosphorrOutput, env, quoted = TRUE)
}

# Custom HTML ----

# Add custom HTML to wrap the widget (called automatically by createWidget)
phosphorr_html <- function(id, style, class, ...) {
  htmltools::tags$div(
    id = id, class = class, style = style,
    htmltools::tags$div(class = "phosphorr-shim")
  )
}

# API ---

#' Create a phosphorr proxy object
#'
#' @param id Name of the phosphorr panel
#' @param session Valid session object
#'
#' @return panel proxy object
#' @export
phosphorrProxy <- function(id, session = shiny::getDefaultReactiveDomain()) {
  object        <- list( id = id, session = session )
  class(object) <- "phosphorrProxy"

  return(object)
}

#' Adds a new widget to the dock
#'
#' @param proxy Proxy phosphorr object
#' @param id ID for phosphorr widget
#' @param title Title for phosphorr widget
#' @param caption Caption for phosphorr widget
#' @param icon Font Awesome icon (specified via the \code{icon} function)
#' @param closable Create removable
#' @param content Code for phosphorr widget UI
#' @param insertmode How should the widget be added? Options include \code{split-right}, \code{split-left},
#'   \code{split-bottom}, \code{split-top}, \code{tab-before}, and \code{tab-after} (default)
#' @param refwidget Reference widget ID for \code{insertmode} action
#' @param relsize Relative size of widget (between 0 and 1) in relation to \code{refwidget} (or last widget
#'   if \code{refwidget} isn't specified)
#' @param ui UI content.  If just text, need to use HTML(...)
#'
#' @return phosphorrProxy
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
                      ui = HTML("I am a widget!")) {

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

    insertUI(
      selector = paste(
        paste0("#", data$dockID),
        paste0("#", data$widgetID, '.widget-content')
      ),
      ui = ui
    )
  }

  return(proxy)
}

#' Maximize a widget in the dock
#'
#' @param proxy Proxy phosphorr object
#' @param widgetId ID for phosphorr widget
#'
#' @return phosphorrProxy
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

#' Minimize a widget in the dock
#'
#' @param proxy Proxy phosphorr object
#' @param widgetId ID for phosphorr widget
#'
#' @return phosphorrProxy
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
#' @param proxy Proxy phosphorr object
#' @param widgetIDs IDs for phosphorr widget
#' @param .all Remove all widgets?  This will override any ids specified in \code{widgetIDs}
#'
#' @return phosphorrProxy
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
