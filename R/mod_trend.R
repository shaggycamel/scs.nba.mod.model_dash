#' Trend UI
#'
#' @param id Module namespace.
#' @noRd
mod_trend_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        "Rolling score by prediction date",
        selectInput(
          ns("metric"),
          NULL,
          width = "200px",
          choices = c(
            "Brier" = "brier",
            "MAE games" = "mae_games",
            "MAE days" = "mae_days"
          )
        )
      ),
      plotly::plotlyOutput(ns("plot"), height = 420)
    ),
    htmltools::p(
      class = "text-muted small",
      "Line is a 28-day weighted rolling mean; points are single prediction days. Dashed line is a model refit."
    )
  )
}

#' Trend server
#'
#' @param id Module namespace.
#' @param filters Reactive filter list.
#' @noRd
mod_trend_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    daily <- reactive({
      f <- filters()
      metric <- input$metric
      if (metric == "brier") {
        d <- prob_view(f) |>
          filter_horizon(f$horizon) |>
          filter_scoreable() |>
          dplyr::group_by(.data$scope, .data$as_of) |>
          dplyr::group_modify(~ prob_metrics(.x$p, .x$y)) |>
          dplyr::ungroup() |>
          dplyr::transmute(scope = .data$scope, as_of = .data$as_of, value = .data$brier, n = .data$n)
      } else {
        measure <- if (metric == "mae_games") "games_missed" else "return_days"
        d <- point_view(f) |>
          filter_measure(measure) |>
          filter_scoreable() |>
          dplyr::group_by(.data$scope, .data$as_of) |>
          dplyr::group_modify(~ point_metrics(.x$predicted, .x$actual, POINT_TOLERANCES[[measure]])) |>
          dplyr::ungroup() |>
          dplyr::transmute(scope = .data$scope, as_of = .data$as_of, value = .data$mae, n = .data$n)
      }
      d |>
        dplyr::group_by(.data$scope) |>
        dplyr::mutate(rolled = rolling_weighted(.data$as_of, .data$value, .data$n, 28)) |>
        dplyr::ungroup() |>
        dplyr::mutate(scope = scope_factor(.data$scope))
    })

    output$plot <- plotly::renderPlotly({
      d <- daily()
      validate(need(nrow(d) > 0, "No scored forecasts for this filter."))
      label <- c(brier = "Brier", mae_games = "MAE games", mae_days = "MAE days")[[input$metric]]
      refits <- dplyr::filter(mdl_meta, .data$model_id == filters()$model_id)$model_created_at
      shapes <- lapply(as.Date(refits), vline_shape)

      p <- plotly::plot_ly()
      for (s in intersect(SCOPE_LEVELS, unique(d$scope))) {
        ds <- dplyr::filter(d, .data$scope == s)
        p <- plotly::add_markers(
          p,
          x = ds$as_of,
          y = ds$value,
          name = scope_label(s),
          marker = list(color = SCOPE_COLOURS[[s]], size = 5, opacity = 0.35),
          text = paste0("n = ", ds$n),
          hoverinfo = "text+y+name"
        ) |>
          plotly::add_lines(
            x = ds$as_of,
            y = ds$rolled,
            line = list(color = SCOPE_COLOURS[[s]], width = 3),
            showlegend = FALSE,
            hoverinfo = "skip",
            inherit = FALSE
          )
      }

      p |>
        plotly::layout(
          shapes = shapes,
          xaxis = list(title = ""),
          yaxis = list(title = label, rangemode = "tozero"),
          legend = list(orientation = "h", y = -0.2)
        )
    })
  })
}
