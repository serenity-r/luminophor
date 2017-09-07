#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
phosphorr <- function(items = NULL, width = "100%", height = "auto", elementId = NULL) {

  # Default options
  options = list()

  if (!is.null(items)) {
    x = list(
      items = utils::modifyList(options, items)
    )
  } else {
    x = options
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
    htmltools::tags$div(class = "phosphorr-wrap")
  )
}
