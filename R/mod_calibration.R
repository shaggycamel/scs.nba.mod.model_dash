#' Calibration UI
#'
#' @param id Module namespace.
#' @noRd
mod_calibration_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("Reliability"),
        plotly::plotlyOutput(ns("plot"), height = 380)
      ),
      card(
        card_header("Scores for the selected horizon"),
        reactable::reactableOutput(ns("table"))
      )
    ),
    card(
      card_header("Brier score by horizon"),
      plotly::plotlyOutput(ns("horizon_plot"), height = 260)
    )
  )
}

#' Calibration server
#'
#' @param id Module namespace.
#' @param filters Reactive filter list.
#' @noRd
mod_calibration_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    scored <- reactive({
      prob_view(filters()) |>
        filter_horizon(filters()$horizon) |>
        filter_scoreable()
    })

    output$plot <- plotly::renderPlotly({
      d <- scored()
      validate(need(nrow(d) > 0, "No scored forecasts for this filter."))
      cal <- d |>
        dplyr::group_by(.data$scope) |>
        dplyr::group_modify(~ calibration_table(.x$p, .x$y)) |>
        dplyr::ungroup()

      p <- plotly::plot_ly()
      for (s in intersect(SCOPE_LEVELS, unique(cal$scope))) {
        ds <- dplyr::filter(cal, .data$scope == s)
        p <- plotly::add_markers(
          p,
          x = ds$p_mean,
          y = ds$y_rate,
          name = scope_label(s),
          size = ds$n,
          sizes = c(60, 900),
          marker = list(color = SCOPE_COLOURS[[s]], opacity = 0.8),
          text = paste0("n = ", ds$n, "<br>predicted ", round(ds$p_mean, 3), "<br>observed ", round(ds$y_rate, 3)),
          hoverinfo = "text+name"
        )
      }
      p |>
        plotly::add_lines(
          x = c(0, 1),
          y = c(0, 1),
          inherit = FALSE,
          showlegend = FALSE,
          line = list(dash = "dash", color = "#9aa0a6", width = 1)
        ) |>
        plotly::layout(
          xaxis = list(title = "Predicted probability", range = c(0, 1)),
          yaxis = list(title = "Observed rate", range = c(0, 1)),
          legend = list(orientation = "h", y = -0.2)
        )
    })

    output$table <- reactable::renderReactable({
      d <- scored()
      if (nrow(d) == 0) {
        return(reactable::reactable(tibble::tibble()))
      }
      scores <- d |>
        prob_metrics_by(.data$scope) |>
        dplyr::mutate(scope = scope_label(.data$scope)) |>
        dplyr::select(
          "Scope" = "scope",
          "n" = "n",
          "Base rate" = "base_rate",
          "Mean p" = "mean_p",
          "Brier" = "brier",
          "Skill" = "brier_skill",
          "Log loss" = "log_loss",
          "AUC" = "auc"
        )
      reactable::reactable(
        scores,
        defaultColDef = reactable::colDef(format = reactable::colFormat(digits = 3), align = "right", na = "-"),
        columns = list(Scope = reactable::colDef(align = "left"))
      )
    })

    output$horizon_plot <- plotly::renderPlotly({
      d <- prob_view(filters()) |> filter_scoreable()
      validate(need(nrow(d) > 0, "No scored forecasts for this filter."))
      scores <- d |>
        prob_metrics_by(.data$scope, .data$horizon) |>
        dplyr::mutate(horizon = factor(.data$horizon, levels = names(BASE_RATE_LABELS)))

      p <- plotly::plot_ly()
      for (s in intersect(SCOPE_LEVELS, unique(scores$scope))) {
        ds <- dplyr::filter(scores, .data$scope == s)
        p <- plotly::add_bars(
          p,
          x = ds$horizon,
          y = ds$brier,
          name = scope_label(s),
          marker = list(color = SCOPE_COLOURS[[s]]),
          text = paste0("n = ", ds$n),
          hoverinfo = "text+y+name"
        )
      }
      p |>
        plotly::layout(
          barmode = "group",
          xaxis = list(title = ""),
          yaxis = list(title = "Brier (lower is better)"),
          legend = list(orientation = "h", y = -0.3)
        )
    })
  })
}
