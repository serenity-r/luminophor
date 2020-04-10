.onLoad <- function(libname, pkgname) {
  shiny::registerInputHandler("layout_handler", convertLayout, force = TRUE)
  invisible()
}
