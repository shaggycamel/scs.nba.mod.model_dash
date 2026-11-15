#' Data quality UI
#'
#' @param id Module namespace.
#' @noRd
mod_quality_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_column_wrap(
      width = "220px",
      fill = FALSE,
      bslib::value_box("Prediction days", textOutput(ns("n_days"))),
      bslib::value_box("Missing days", textOutput(ns("n_missing"))),
      bslib::value_box("Censored spells", textOutput(ns("n_censored"))),
      bslib::value_box("History imputed", textOutput(ns("n_imputed")))
    ),
    bslib::layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("Forecasts published per day"),
        plotly::plotlyOutput(ns("coverage"), height = 300)
      ),
      card(
        card_header("Why spells are censored"),
        reactable::reactableOutput(ns("censor"))
      )
    ),
    card(
      card_header("Outcome consistency"),
      reactable::reactableOutput(ns("definition"))
    ),
    card(
      card_header("Model and bake"),
      reactable::reactableOutput(ns("model_info"))
    )
  )
}

#' Data quality server
#'
#' @param id Module namespace.
#' @param filters Reactive filter list.
#' @noRd
mod_quality_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    meta <- reactive({
      dplyr::filter(mdl_meta, .data$model_id == filters()$model_id)
    })

    output$n_days <- renderText(meta()$prediction_days[[1]])
    output$n_missing <- renderText(meta()$missing_days[[1]])
    output$n_censored <- renderText(meta()$censored_spells[[1]])
    output$n_imputed <- renderText(meta()$history_imputed_spells[[1]])

    output$coverage <- plotly::renderPlotly({
      d <- dplyr::filter(mdl_coverage, .data$model_id == filters()$model_id)
      validate(need(nrow(d) > 0, "No coverage rows."))
      plotly::plot_ly(
        d,
        x = ~as_of,
        y = ~spells_active,
        type = "bar",
        name = "Active spells",
        marker = list(color = "#D6DCE5"),
        hoverinfo = "x+y"
      ) |>
        plotly::add_lines(
          x = d$as_of,
          y = d$scoreable_next,
          name = "Next game scoreable",
          line = list(color = "#133DEF", width = 2),
          yaxis = "y"
        ) |>
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Spells"),
          legend = list(orientation = "h", y = -0.25)
        )
    })

    output$censor <- reactable::renderReactable({
      d <- dplyr::filter(mdl_censor, .data$model_id == filters()$model_id) |>
        dplyr::arrange(dplyr::desc(.data$n)) |>
        dplyr::transmute(reason = .data$censor_reason, n = .data$n)
      if (nrow(d) == 0) {
        return(reactable::reactable(tibble::tibble()))
      }
      reactable::reactable(
        d,
        columns = list(reason = reactable::colDef(name = "Reason", align = "left"))
      )
    })

    output$definition <- reactable::renderReactable({
      m <- meta()
      d <- tibble::tibble(
        check = c(
          "Spells returned on a game still filed Out",
          "Out report rows on games played",
          "Share of all Out report rows"
        ),
        value = c(
          sprintf("%d of %d", m$out_on_return_spells[[1]], m$resolved_spells[[1]]),
          format(m$stale_out_rows[[1]], big.mark = ","),
          sprintf("%.2f%%", 100 * m$stale_out_share[[1]])
        )
      )
      reactable::reactable(
        d,
        columns = list(
          check = reactable::colDef(name = "Check", align = "left"),
          value = reactable::colDef(name = "Value", align = "right")
        )
      )
    })

    output$model_info <- reactable::renderReactable({
      m <- meta()
      d <- tibble::tibble(
        field = c("Model trained through", "Model created", "Latest forecast generated", "Outcome cutoff", "Baked at"),
        value = c(
          m$model_trained_through[[1]],
          format(m$model_created_at[[1]], "%d %b %Y %H:%M UTC", tz = "UTC"),
          format(m$latest_generated_at[[1]], "%d %b %Y %H:%M UTC", tz = "UTC"),
          format(m$cutoff[[1]], "%d %b %Y"),
          format(m$baked_at[[1]], "%d %b %Y %H:%M UTC", tz = "UTC")
        )
      )
      reactable::reactable(
        d,
        columns = list(
          field = reactable::colDef(name = "Field", align = "left"),
          value = reactable::colDef(name = "Value", align = "right")
        )
      )
    })
  })
}
