#' scorecard UI
#'
#' @param id Module namespace.
#' @noRd
mod_scorecard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_column_wrap(
      width = "220px",
      fill = FALSE,
      bslib::value_box("Models monitored", textOutput(ns("n_models"))),
      bslib::value_box("Scored 7-day forecasts", textOutput(ns("n_scored"))),
      bslib::value_box("Spells resolved", textOutput(ns("n_resolved"))),
      bslib::value_box("Onset games error", textOutput(ns("games_mae")))
    ),
    card(
      card_header("Model scorecard"),
      reactable::reactableOutput(ns("table"))
    ),
    htmltools::p(
      class = "text-muted small mt-2",
      "Onset scores the first forecast per spell. Nowcast scores every daily forecast."
    )
  )
}

#' scorecard server
#'
#' @param id Module namespace.
#' @noRd
mod_scorecard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    scorecard <- reactive({
      scorecard_table(mdl_prob, mdl_point, mdl_unit, model_registry())
    })

    output$n_models <- renderText(nrow(scorecard()))
    output$n_scored <- renderText({
      d <- dplyr::filter(mdl_prob, .data$scope == "nowcast", .data$horizon == "7d", .data$scoreable)
      format(sum(d$scoreable), big.mark = ",")
    })
    output$n_resolved <- renderText(sum(mdl_unit$returned))
    output$games_mae <- renderText({
      value <- scorecard()$games_mae_onset[[1]]
      if (is.na(value)) "n/a" else sprintf("%.1f games", value)
    })

    output$table <- reactable::renderReactable({
      d <- scorecard()
      reactable::reactable(
        d,
        defaultColDef = reactable::colDef(align = "right"),
        columns = list(
          label = reactable::colDef(name = "Model", align = "left"),
          brier_next_onset = reactable::colDef(name = "Brier next | onset", format = reactable::colFormat(digits = 3)),
          brier_next_now = reactable::colDef(name = "Brier next | nowcast", format = reactable::colFormat(digits = 3)),
          brier_7d_onset = reactable::colDef(name = "Brier 7d | onset", format = reactable::colFormat(digits = 3)),
          brier_7d_now = reactable::colDef(name = "Brier 7d | nowcast", format = reactable::colFormat(digits = 3)),
          auc_7d_onset = reactable::colDef(name = "AUC 7d | onset", format = reactable::colFormat(digits = 3)),
          games_mae_onset = reactable::colDef(name = "MAE games | onset", format = reactable::colFormat(digits = 2)),
          games_mae_now = reactable::colDef(name = "MAE games | nowcast", format = reactable::colFormat(digits = 2)),
          c_index = reactable::colDef(name = "C-index | onset", format = reactable::colFormat(digits = 3)),
          resolved = reactable::colDef(name = "Resolved"),
          censored = reactable::colDef(name = "Censored")
        )
      )
    })
  })
}
