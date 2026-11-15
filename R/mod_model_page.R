#' Model page UI
#'
#' @param id Module namespace.
#' @param model_id Registered model identifier.
#' @noRd
mod_model_page_ui <- function(id, model_id) {
  ns <- NS(id)
  info <- model_row(model_id)
  dates <- model_dates(model_id)

  layout_sidebar(
    sidebar = sidebar(
      width = 280,
      htmltools::p(class = "fw-bold mb-1", info$label),
      htmltools::p(class = "text-muted small", info$description),
      selectInput(ns("scope"), "Scope", choices = scope_choices(), selected = "both"),
      selectInput(ns("horizon"), "Horizon", choices = model_horizons(model_id), selected = "7d"),
      dateRangeInput(
        ns("dates"),
        "Prediction dates",
        start = dates[1],
        end = dates[2],
        min = dates[1],
        max = dates[2],
        weekstart = 1
      ),
      selectInput(ns("version"), "Model version", choices = version_choices(model_versions(model_id))),
      htmltools::p(
        class = "text-muted small mt-3",
        "Every panel recomputes from the baked daily forecasts, so the filters apply to all tabs."
      )
    ),
    bslib::navset_card_tab(
      id = ns("tabs"),
      nav_panel("Calibration", mod_calibration_ui(ns("calibration"))),
      nav_panel("Accuracy", mod_accuracy_ui(ns("accuracy"))),
      nav_panel("Trend", mod_trend_ui(ns("trend"))),
      nav_panel("Breakdowns", mod_breakdowns_ui(ns("breakdowns"))),
      nav_panel("Spells", mod_spells_ui(ns("spells"))),
      nav_panel("Live", mod_live_ui(ns("live"))),
      nav_panel("Data quality", mod_quality_ui(ns("quality")))
    )
  )
}

#' Model page server
#'
#' @param id Module namespace.
#' @param model_id Registered model identifier.
#' @noRd
mod_model_page_server <- function(id, model_id) {
  moduleServer(id, function(input, output, session) {
    filters <- reactive({
      req(input$scope, input$horizon, input$dates, input$version)
      list(
        model_id = model_id,
        scope = input$scope,
        horizon = input$horizon,
        dates = input$dates,
        version = input$version
      )
    })

    mod_calibration_server("calibration", filters)
    mod_accuracy_server("accuracy", filters)
    mod_trend_server("trend", filters)
    mod_breakdowns_server("breakdowns", filters)
    mod_spells_server("spells", filters)
    mod_live_server("live", filters)
    mod_quality_server("quality", filters)
  })
}
