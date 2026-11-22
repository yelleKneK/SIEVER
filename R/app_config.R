#' Access files in the current app
#'
#' Locates a file shipped inside the installed SIEVER package.
#'
#' @param ... Character vectors, specifying subdirectory and file(s)
#'   within the installed package.
#' @return A character path returned by [base::system.file()].
#' @author Ken Kelley
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "SIEVER")
}
