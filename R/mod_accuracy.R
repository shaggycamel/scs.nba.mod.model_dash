#' Accuracy UI
#'
#' @param id Module namespace.
#' @noRd
mod_accuracy_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("Forecast error"),
        selectInput(ns("measure"), NULL, choices = measure_choices(), width = "220px"),
        plotly::plotlyOutput(ns("plot"), height = 360)
      ),
      card(
        card_header("Error by days already out"),
        plotly::plotlyOutput(ns("bucket_plot"), height = 360)
      )
    ),
    card(
      card_header("Scores"),
      reactable::reactableOutput(ns("table"))
    )
  )
}

#' Accuracy server
#'
#' @param id Module namespace.
#' @param filters Reactive filter list.
#' @noRd
mod_accuracy_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    measure <- reactive({
      req(input$measure)
      input$measure
    })
    tolerances <- reactive(POINT_TOLERANCES[[measure()]])

    scored <- reactive({
      point_view(filters()) |>
        filter_measure(measure()) |>
        filter_scoreable() |>
        dplyr::mutate(
          error = .data$predicted - .data$actual,
          scope = scope_factor(.data$scope)
        )
    })

    output$plot <- plotly::renderPlotly({
      d <- scored()
      validate(need(nrow(d) > 0, "No scored forecasts for this filter."))
      plotly::plot_ly(
        d,
        x = ~error,
        color = ~scope,
        colors = scope_palette(),
        type = "histogram",
        opacity = 0.65
      ) |>
        plotly::layout(
          barmode = "overlay",
          shapes = list(hline_shape(0)),
          xaxis = list(
            title = if (measure() == "return_days") {
              "Predicted minus actual return date (days)"
            } else {
              "Predicted minus actual games missed"
            }
          ),
          yaxis = list(title = "Forecasts"),
          legend = list(orientation = "h", y = -0.2)
        )
    })

    output$bucket_plot <- plotly::renderPlotly({
      d <- scored()
      validate(need(nrow(d) > 0, "No scored forecasts for this filter."))
      plotly::plot_ly(
        d,
        x = ~days_bucket,
        y = ~error,
        color = ~scope,
        colors = scope_palette(),
        type = "box",
        boxpoints = "all",
        jitter = 0.4,
        pointpos = 0
      ) |>
        plotly::layout(
          shapes = list(hline_shape(0)),
          xaxis = list(
            title = "Days already out when the forecast was made",
            categoryorder = "array",
            categoryarray = c("0-3", "4-7", "8-14", "15-30", "31+")
          ),
          yaxis = list(title = "Error"),
          legend = list(orientation = "h", y = -0.2),
          boxmode = "group"
        )
    })

    output$table <- reactable::renderReactable({
      d <- scored()
      if (nrow(d) == 0) {
        return(reactable::reactable(tibble::tibble()))
      }
      tols <- tolerances()
      scores <- d |>
        point_metrics_by(tols, .data$scope) |>
        dplyr::mutate(scope = scope_label(.data$scope)) |>
        dplyr::select(
          "Scope" = "scope",
          "n" = "n",
          "MAE" = "mae",
          "Median AE" = "median_ae",
          "P90 AE" = "p90_ae",
          "Bias" = "bias",
          dplyr::all_of(stats::setNames(paste0("within_", tols), paste0("Within +/-", tols)))
        )
      reactable::reactable(
        scores,
        defaultColDef = reactable::colDef(format = reactable::colFormat(digits = 2), align = "right", na = "-"),
        columns = list(Scope = reactable::colDef(align = "left"))
      )
    })
  })
}
