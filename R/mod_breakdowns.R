#' Breakdowns UI
#'
#' @param id Module namespace.
#' @noRd
mod_breakdowns_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        "Probability by slice",
        selectInput(ns("dim"), NULL, width = "220px", choices = dim_choices())
      ),
      reactable::reactableOutput(ns("prob_table"))
    ),
    card(
      card_header(
        "Point error by slice",
        selectInput(ns("measure"), NULL, width = "220px", choices = measure_choices())
      ),
      reactable::reactableOutput(ns("point_table"))
    )
  )
}

#' Breakdowns server
#'
#' @param id Module namespace.
#' @param filters Reactive filter list.
#' @noRd
mod_breakdowns_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    dim <- reactive({
      req(input$dim)
      input$dim
    })

    slice_frame <- function(d, dimension) {
      d |>
        dplyr::mutate(value = as.character(.data[[dimension]]), value = dplyr::coalesce(.data$value, "(missing)"))
    }

    measure <- reactive({
      req(input$measure)
      input$measure
    })

    output$prob_table <- reactable::renderReactable({
      f <- filters()
      d <- prob_view(f) |>
        filter_horizon(f$horizon) |>
        filter_scoreable()
      dimension <- dim()
      if (dimension %in% c("body_region", "ailment_class", "team_slug", "history_imputed")) {
        d <- with_unit_dims(d, units_view(f), dimension)
      }
      if (nrow(d) == 0) {
        return(reactable::reactable(tibble::tibble()))
      }
      d <- slice_frame(d, dimension)
      scores <- d |>
        prob_metrics_by(.data$value, .data$scope) |>
        dplyr::mutate(scope = scope_label(.data$scope)) |>
        dplyr::select(
          "value" = "value",
          "Scope" = "scope",
          "n" = "n",
          "Base rate" = "base_rate",
          "Brier" = "brier",
          "Skill" = "brier_skill",
          "AUC" = "auc"
        ) |>
        dplyr::arrange(.data$value, .data$Scope)

      reactable::reactable(
        scores,
        defaultColDef = reactable::colDef(format = reactable::colFormat(digits = 3), align = "right", na = "-"),
        columns = list(
          value = reactable::colDef(name = DIM_LABELS[[dimension]], align = "left"),
          Scope = reactable::colDef(align = "left")
        ),
        defaultSorted = "Brier"
      )
    })

    output$point_table <- reactable::renderReactable({
      f <- filters()
      d <- point_view(f) |>
        filter_measure(measure()) |>
        filter_scoreable()
      dimension <- dim()
      if (dimension %in% c("body_region", "ailment_class", "team_slug", "history_imputed")) {
        d <- with_unit_dims(d, units_view(f), dimension)
      }
      if (nrow(d) == 0) {
        return(reactable::reactable(tibble::tibble()))
      }
      d <- slice_frame(d, dimension)
      scores <- d |>
        point_metrics_by(POINT_TOLERANCES[[measure()]], .data$value, .data$scope) |>
        dplyr::mutate(scope = scope_label(.data$scope)) |>
        dplyr::select(
          "value" = "value",
          "Scope" = "scope",
          "n" = "n",
          "MAE" = "mae",
          "Median AE" = "median_ae",
          "Bias" = "bias"
        ) |>
        dplyr::arrange(.data$value, .data$Scope)

      reactable::reactable(
        scores,
        defaultColDef = reactable::colDef(format = reactable::colFormat(digits = 2), align = "right", na = "-"),
        columns = list(
          value = reactable::colDef(name = DIM_LABELS[[dimension]], align = "left"),
          Scope = reactable::colDef(align = "left")
        ),
        defaultSorted = "MAE"
      )
    })
  })
}
