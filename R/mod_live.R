#' Live UI
#'
#' @param id Module namespace.
#' @noRd
mod_live_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_column_wrap(
      width = "220px",
      fill = FALSE,
      bslib::value_box("Still out", textOutput(ns("n_active"))),
      bslib::value_box("Predicted back within 7 days", textOutput(ns("n_back"))),
      bslib::value_box("Overdue", textOutput(ns("n_overdue")))
    ),
    card(
      card_header("Current spells"),
      reactable::reactableOutput(ns("table"))
    ),
    card(
      card_header("Predicted returns, next 7 days"),
      plotly::plotlyOutput(ns("plot"), height = 240)
    )
  )
}

#' Live server
#'
#' @param id Module namespace.
#' @param filters Reactive filter list.
#' @noRd
mod_live_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    live <- reactive({
      f <- filters()
      latest <- mdl_prob |>
        dplyr::filter(
          .data$model_id == f$model_id,
          .data$scope == "nowcast",
          .data$horizon == "next_game"
        ) |>
        filter_version(f$version) |>
        dplyr::group_by(.data$unit_id) |>
        dplyr::slice_max(.data$as_of, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()

      week <- mdl_prob |>
        dplyr::filter(
          .data$model_id == f$model_id,
          .data$scope == "nowcast",
          .data$horizon == "7d"
        ) |>
        filter_version(f$version) |>
        dplyr::group_by(.data$unit_id) |>
        dplyr::slice_max(.data$as_of, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::transmute(unit_id = .data$unit_id, p_7d = .data$p)

      latest |>
        dplyr::left_join(week, by = "unit_id") |>
        dplyr::left_join(
          dplyr::select(mdl_unit, "unit_id", "label", "team_slug", "returned", "censored"),
          by = "unit_id"
        ) |>
        dplyr::filter(!.data$returned) |>
        dplyr::mutate(
          cutoff = dplyr::filter(mdl_meta, .data$model_id == f$model_id)$cutoff[[1]],
          overdue = !is.na(.data$pred_return_date) & .data$pred_return_date < .data$cutoff
        )
    })

    output$n_active <- renderText(nrow(live()))
    output$n_back <- renderText({
      d <- live()
      cutoff <- dplyr::filter(mdl_meta, .data$model_id == filters()$model_id)$cutoff[[1]]
      sum(!is.na(d$pred_return_date) & d$pred_return_date >= cutoff & d$pred_return_date <= cutoff + 7)
    })
    output$n_overdue <- renderText(sum(live()$overdue))

    output$table <- reactable::renderReactable({
      d <- live() |>
        dplyr::transmute(
          player = .data$label,
          team = .data$team_slug,
          status,
          days_out = .data$days_missed_so_far,
          p_next = .data$p,
          p_7d,
          predicted = .data$pred_return_date,
          overdue
        ) |>
        dplyr::arrange(dplyr::desc(.data$p_next))
      if (nrow(d) == 0) {
        return(reactable::reactable(tibble::tibble()))
      }
      reactable::reactable(
        d,
        defaultColDef = reactable::colDef(align = "right"),
        columns = list(
          player = reactable::colDef(name = "Player", align = "left"),
          team = reactable::colDef(name = "Team", align = "left"),
          status = reactable::colDef(name = "Status", align = "left"),
          days_out = reactable::colDef(name = "Days out"),
          p_next = reactable::colDef(name = "P(next game)", format = reactable::colFormat(digits = 3)),
          p_7d = reactable::colDef(name = "P(7 days)", format = reactable::colFormat(digits = 3)),
          predicted = reactable::colDef(
            name = "Predicted return",
            align = "left",
            format = reactable::colFormat(date = TRUE)
          ),
          overdue = reactable::colDef(name = "Overdue", align = "center")
        ),
        defaultSorted = "p_next",
        defaultSortOrder = "desc"
      )
    })

    output$plot <- plotly::renderPlotly({
      d <- live()
      cutoff <- dplyr::filter(mdl_meta, .data$model_id == filters()$model_id)$cutoff[[1]]
      back <- d |>
        dplyr::filter(
          !is.na(.data$pred_return_date),
          .data$pred_return_date >= .data$cutoff,
          .data$pred_return_date <= .data$cutoff + 7
        ) |>
        dplyr::count(.data$pred_return_date)
      validate(need(nrow(back) > 0, "No predicted returns in the next 7 days."))
      plotly::plot_ly(
        back,
        x = ~pred_return_date,
        y = ~n,
        type = "bar",
        marker = list(color = "#133DEF"),
        hoverinfo = "x+y"
      ) |>
        plotly::layout(xaxis = list(title = ""), yaxis = list(title = "Players"))
    })
  })
}
