#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
phosphorr <- function(items = NULL, width = "100%", height = "72vh", elementId = NULL) {

  # Default options
  options = list()

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
    dependencies = list(rmarkdown::html_dependency_jquery(),
                        rmarkdown::html_dependency_font_awesome()), # Widgets don't load these automatically like Shiny
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
#' @param phosphorrProxy Proxy phosphorr object
#' @param id ID for phosphorr widget
#' @param closable Create removable
#' @param content Code for phosphorr widget UI
#' @param insertmode How should the widget be added? Options include \code{split-right}, \code{split-left},
#'   \code{split-bottom}, \code{split-top}, \code{tab-before}, and \code{tab-after} (default)
#' @param refwidget Reference widget ID for \code{insertmode} action
#' @param relsize Relative size of widget (between 0 and 1) in relation to \code{refwidget} (or last widget
#'   if \code{refwidget} isn't specified)
#' @param ui Shiny UI content.  If just text, need to use HTML(...)
#'
#' @return phosphorrProxy
#' @export
addWidget <- function(phosphorrProxy,
                      id,
                      title = "Widget",
                      closable = TRUE,
                      insertmode = "tab-after",
                      refwidgetID = NULL,
                      relsize = NULL,
                      content = "",
                      ui = HTML("I am a widget!")) {

  data <- list(dockID = phosphorrProxy$id,
               widgetID = id,
               title = title,
               closable = closable,
               mode = insertmode,
               refwidgetID = refwidgetID,
               size = relsize,
               content = as.character(tags$div(id = id,
                                               class = "content",
                                               tags$div(content))
               )
  )

  # Namespacing to avoid conflicts (http://deanattali.com/blog/htmlwidgets-tips/)
  phosphorrProxy$session$sendCustomMessage("phosphorr:addWidget", data)

  insertUI(
    selector = paste(
      paste0("#", data$dockID),
      paste0("#", data$widgetID),
      "div"),
    ui = ui
  )

  return(phosphorrProxy)
}
