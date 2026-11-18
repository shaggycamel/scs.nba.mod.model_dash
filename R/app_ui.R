#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    page_navbar(
      id = "main_nav",
      window_title = "NBA Model Dashboard",
      title = tags$span("Model Dashboard"),
      nav_spacer(),
      nav_panel("Overview", mod_scorecard_ui("scorecard_1")),
      nav_panel("Game day", mod_game_day_ui("game_day_1")),
      nav_panel("Player stats", mod_player_stats_ui("player_stats_1")),
      nav_panel(
        "Injuries",
        bslib::navset_card_tab(
          nav_panel("Injury log", mod_injuries_ui("injuries_1")),
          nav_panel("Accuracy trend", mod_accuracy_trend_ui("accuracy_trend_1"))
        )
      ),
      nav_item(uiOutput("bake_stamp")),
      theme = nba_theme()
    )
  )
}

#' Add external resources to the application
#'
#' @import shiny
#' @importFrom golem add_resource_path
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
    tags$style(HTML(
      ".navbar .nav-last-update {
         font-size: 0.7rem; font-weight: 400; opacity: 0.85;
         margin-left: 2rem; white-space: nowrap;
       }
       .card-body > .shiny-spinner-output-container { flex: 1 1 auto; min-height: 0; }
       .value-box .value { font-size: 1.6rem; }"
    ))
  )
}
