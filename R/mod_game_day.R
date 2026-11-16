#' Game day UI
#'
#' @param id Module namespace.
#' @noRd
mod_game_day_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Upcoming player availability"),
      reactable::reactableOutput(ns("table"))
    ),
    htmltools::p(
      class = "text-muted small mt-2",
      "One row per player and unplayed fixture. Availability is the injury report
       when it covers the game date, otherwise the injury-duration curve,
       otherwise the hierarchy's state-only probability."
    )
  )
}

#' Game day server
#'
#' @param id Module namespace.
#' @noRd
mod_game_day_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$table <- reactable::renderReactable({
      d <- game_day_table()
      validate(need(nrow(d) > 0, "No upcoming fixtures in this bake."))

      reactable::reactable(
        d,
        pagination = TRUE,
        defaultPageSize = 20,
        filterable = TRUE,
        defaultSorted = "game_date",
        defaultSortOrder = "asc",
        bordered = TRUE,
        highlight = TRUE,
        defaultColDef = reactable::colDef(
          align = "left",
          minWidth = 90,
          headerStyle = list(background = "#cce5ff")
        ),
        columns = list(
          game_date = reactable::colDef(
            name = "Game date",
            format = reactable::colFormat(date = TRUE)
          ),
          team = reactable::colDef(name = "Team"),
          matchup = reactable::colDef(name = "Matchup"),
          player = reactable::colDef(name = "Player", minWidth = 150),
          position = reactable::colDef(name = "Pos"),
          depth = reactable::colDef(name = "Depth", align = "right"),
          report = reactable::colDef(name = "Report", na = "-"),
          p_play = reactable::colDef(
            name = "P(play)",
            align = "right",
            format = reactable::colFormat(digits = 3)
          ),
          source = reactable::colDef(name = "Source"),
          minutes_if_plays = reactable::colDef(
            name = "Min if plays",
            align = "right",
            format = reactable::colFormat(digits = 1)
          ),
          expected_minutes = reactable::colDef(
            name = "Exp min",
            align = "right",
            format = reactable::colFormat(digits = 1)
          ),
          expected_usage = reactable::colDef(
            name = "Exp usage",
            align = "right",
            format = reactable::colFormat(digits = 3)
          )
        )
      )
    })
  })
}
