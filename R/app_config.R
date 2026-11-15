#' Access files in the current app
#'
#' @param ... Character vectors, relative to `inst/app`.
#' @param package Package name. Defaults to the current package.
#' @return A character vector of file paths.
#' @noRd
app_sys <- function(..., package = "modeldash") {
  system.file(..., package = package)
}
