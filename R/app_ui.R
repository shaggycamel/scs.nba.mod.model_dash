#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    page_navbar(
      id = "main_nav",
      window_title = "NBA Model Accuracy",
      title = tags$span("Model Accuracy"),
      nav_spacer(),
      nav_panel("Injuries", mod_injuries_ui("injuries_1")),
      nav_panel("Overview", mod_scorecard_ui("scorecard_1")),
      nav_panel("Injury Duration", mod_model_page_ui("injury_1", "injury_duration")),
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
