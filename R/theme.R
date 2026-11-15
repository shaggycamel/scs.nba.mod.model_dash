#' Shared NBA Shiny theme
#'
#' Mirrors `core::nba_theme()` from the league dashboard so every Shiny app in
#' the workspace looks the same. Kept local because this app does not depend on
#' the league dashboard's package library.
#'
#' @return A `bslib::bs_theme()` object.
#' @noRd
nba_theme <- function() {
  bslib::bs_theme(
    version = 5,
    preset = "litera",
    primary = "#133DEF"
  )
}
