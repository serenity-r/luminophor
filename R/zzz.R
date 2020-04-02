.onLoad <- function(libname, pkgname){registerInputHandler(
  "layout_handler", convertLayout, force = TRUE)
  invisible()}
