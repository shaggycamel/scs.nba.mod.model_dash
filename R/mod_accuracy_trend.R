#' Accuracy trend UI
#'
#' @param id Module namespace.
#' @noRd
mod_accuracy_trend_ui <- function(id) {
  ns <- NS(id)
  ailments <- sort(unique(mdl_unit$ailment_class))
  body_parts <- sort(unique(mdl_unit$body_region))

  layout_sidebar(
    sidebar = sidebar(
      width = 260,
      selectInput(ns("metric"), "Metric", choices = TREND_METRICS, selected = "brier_7d"),
      selectInput(
        ns("ailment"),
        "Ailment class",
        choices = c("All" = "all", stats::setNames(ailments, ailment_label(ailments)))
      ),
      selectInput(
        ns("body_part"),
        "Body part",
        choices = c("All" = "all", stats::setNames(body_parts, body_part_label(body_parts)))
      )
    ),
    card(
      card_header("Model accuracy over time"),
      plotly::plotlyOutput(ns("plot"), height = 560)
    )
  )
}

#' Accuracy trend server
#'
#' @param id Module namespace.
#' @noRd
mod_accuracy_trend_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    daily <- reactive({
      req(input$metric, input$ailment, input$body_part)
      trend_table(input$metric, input$ailment, input$body_part)
    })

    output$plot <- plotly::renderPlotly({
      d <- daily()
      validate(need(nrow(d) > 0, "No scored forecasts for this slice."))

      p <- plotly::plot_ly()
      p <- plotly::add_markers(
        p,
        x = d$as_of,
        y = d$value,
        name = "Daily",
        marker = list(color = "#8A8D93", size = 5, opacity = 0.4),
        text = paste0("n = ", d$n),
        hoverinfo = "text+y+name"
      )
      p <- plotly::add_lines(
        p,
        x = d$as_of,
        y = d$rolled,
        name = "28-day mean",
        line = list(color = "#133DEF", width = 3),
        inherit = FALSE
      )

      plotly::layout(
        p,
        xaxis = list(title = ""),
        yaxis = list(title = TREND_METRICS[[input$metric]], rangemode = "tozero"),
        legend = list(orientation = "h", y = -0.15)
      )
    })
  })
}
