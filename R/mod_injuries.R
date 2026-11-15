#' Injuries UI
#'
#' @param id Module namespace.
#' @noRd
mod_injuries_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("Current injuries"),
    htmltools::p(
      class = "text-muted small mb-2",
      "Latest forecast per active spell. Dates are the first missed game and the median predicted return."
    ),
    reactable::reactableOutput(ns("table"))
  )
}

#' Injuries server
#'
#' @param id Module namespace.
#' @noRd
mod_injuries_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    injuries <- reactive({
      injuries_table("injury_duration")
    })

    output$table <- reactable::renderReactable({
      d <- injuries()
      if (nrow(d) == 0) {
        return(reactable::reactable(tibble::tibble()))
      }
      reactable::reactable(
        d,
        searchable = TRUE,
        filterable = TRUE,
        defaultPageSize = 25,
        defaultSorted = "predicted",
        bordered = TRUE,
        highlight = TRUE,
        defaultColDef = reactable::colDef(
          align = "left",
          minWidth = 120,
          headerStyle = list(background = "#cce5ff")
        ),
        columns = list(
          forecast = reactable::colDef(
            name = "Forecast date",
            align = "left",
            format = reactable::colFormat(date = TRUE)
          ),
          team = reactable::colDef(name = "Team", align = "left"),
          player = reactable::colDef(name = "Player", align = "left"),
          ailment = reactable::colDef(name = "Ailment class", align = "left"),
          body_part = reactable::colDef(name = "Body part", align = "left"),
          injured = reactable::colDef(
            name = "Date of injury",
            align = "left",
            format = reactable::colFormat(date = TRUE)
          ),
          predicted = reactable::colDef(
            name = "Predicted return",
            align = "left",
            format = reactable::colFormat(date = TRUE)
          )
        )
      )
    })
  })
}
