#' Spells UI
#'
#' @param id Module namespace.
#' @noRd
mod_spells_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Spell trajectory"),
      selectInput(ns("unit"), NULL, choices = character(0), width = "100%"),
      plotly::plotlyOutput(ns("plot"), height = 340)
    ),
    card(
      card_header("Daily forecasts"),
      reactable::reactableOutput(ns("table"))
    )
  )
}

#' Spells server
#'
#' @param id Module namespace.
#' @param filters Reactive filter list.
#' @noRd
mod_spells_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    units <- reactive({
      units_view(filters()) |>
        dplyr::arrange(dplyr::desc(.data$onset_date))
    })

    observeEvent(units(), {
      d <- units()
      choices <- stats::setNames(d$unit_id, paste0(d$label, " | ", format(d$onset_date, "%d %b %Y")))
      selected <- if (length(choices) > 0) choices[[1]] else NULL
      updateSelectInput(session, "unit", choices = choices, selected = selected)
    })

    selected <- reactive({
      req(input$unit)
      dplyr::filter(mdl_unit, .data$unit_id == input$unit)
    })

    trajectory <- reactive({
      f <- filters()
      dplyr::filter(mdl_prob, .data$model_id == f$model_id, .data$unit_id == input$unit, .data$scope == "nowcast") |>
        filter_version(f$version)
    })

    output$plot <- plotly::renderPlotly({
      d <- trajectory()
      validate(need(nrow(d) > 0, "Pick a spell with forecasts."))
      next_game <- dplyr::filter(d, .data$horizon == "next_game")
      week <- dplyr::filter(d, .data$horizon == "7d")
      info <- selected()
      shapes <- list()
      if (nrow(info) == 1 && !is.na(info$actual_return_date)) {
        shapes <- list(vline_shape(info$actual_return_date, colour = "#2E7D32", width = 1.5))
      }

      plotly::plot_ly() |>
        plotly::add_lines(
          x = next_game$as_of,
          y = next_game$p,
          name = "P(return next game)",
          line = list(color = "#133DEF", width = 2)
        ) |>
        plotly::add_lines(
          x = week$as_of,
          y = week$p,
          name = "P(return within 7 days)",
          line = list(color = "#8A8D93", width = 2, dash = "dot")
        ) |>
        plotly::layout(
          shapes = shapes,
          xaxis = list(title = ""),
          yaxis = list(title = "Probability", range = c(0, 1)),
          legend = list(orientation = "h", y = -0.2)
        )
    })

    output$table <- reactable::renderReactable({
      d <- dplyr::filter(trajectory(), .data$horizon == "next_game") |>
        dplyr::transmute(
          as_of = format(.data$as_of, "%Y-%m-%d"),
          days_missed_so_far,
          days_bucket,
          status,
          p,
          pred_return_date = format(.data$pred_return_date, "%d %b %Y")
        ) |>
        dplyr::arrange(dplyr::desc(.data$as_of))
      if (nrow(d) == 0) {
        return(reactable::reactable(tibble::tibble()))
      }
      reactable::reactable(
        d,
        defaultColDef = reactable::colDef(align = "right"),
        columns = list(
          as_of = reactable::colDef(name = "Forecast date", align = "left"),
          days_missed_so_far = reactable::colDef(name = "Days out"),
          days_bucket = reactable::colDef(name = "Bucket", align = "left"),
          status = reactable::colDef(name = "Status", align = "left"),
          p = reactable::colDef(name = "P(next game)", format = reactable::colFormat(digits = 3)),
          pred_return_date = reactable::colDef(name = "Predicted return", align = "left")
        )
      )
    })
  })
}
