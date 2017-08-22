#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
phosphorus <- function(items = NULL, width = "100%", height = "auto", elementId = NULL) {

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
    name = 'phosphorus',
    x,
    width = width,
    height = height,
    package = 'phosphorus',
    dependencies = rmarkdown::html_dependency_font_awesome(), # Widgets don't load these automatically like Shiny
    elementId = elementId
  )
}

#' Shiny bindings for phosphorus
#'
#' Output and render functions for using phosphorus within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a phosphorus
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name phosphorus-shiny
#'
#' @export
phosphorusOutput <- function(outputId, width = '100%', height = 'auto'){
  htmlwidgets::shinyWidgetOutput(outputId, 'phosphorus', width, height, package = 'phosphorus')
}

#' @rdname phosphorus-shiny
#' @export
renderphosphorus <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, phosphorusOutput, env, quoted = TRUE)
}
