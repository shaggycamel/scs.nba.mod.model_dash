#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @noRd
app_server <- function(input, output, session) {
  options(spinner.type = 1, spinner.color = "#133DEF")

  output$bake_stamp <- renderUI({
    baked <- max(mdl_meta$baked_at, na.rm = TRUE)
    cutoff <- max(mdl_meta$cutoff, na.rm = TRUE)
    span(
      class = "nav-last-update",
      glue::glue("Baked {format(baked, '%d %b %H:%M UTC', tz = 'UTC')} | outcomes to {format(cutoff, '%d %b')}")
    )
  })

  mod_injuries_server("injuries_1")
  mod_accuracy_trend_server("accuracy_trend_1")
  mod_scorecard_server("scorecard_1")
  mod_model_page_server("injury_1", "injury_duration")
}
