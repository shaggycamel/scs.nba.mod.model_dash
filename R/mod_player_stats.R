#' Player stat projections UI
#'
#' @param id Module namespace.
#' @noRd
mod_player_stats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Player stat projections"),
      layout_column_wrap(
        width = 1 / 4,
        selectInput(ns("game_date"), "Game date", choices = NULL),
        selectInput(ns("team"), "Team", choices = c("All teams" = "all")),
        textInput(ns("player"), "Player", placeholder = "Search"),
        selectInput(ns("sort_stat"), "Sort by", choices = player_stat_choices())
      ),
      reactable::reactableOutput(ns("table"))
    ),
    htmltools::p(
      class = "text-muted small mt-2",
      "Per-game means conditional on playing. P(play) is the chance the player
       takes the floor; multiply the projection by it for an ex-ante expected
       value. Horizon is game date minus prediction date."
    )
  )
}

#' Player stat projections server
#'
#' @param id Module namespace.
#' @noRd
mod_player_stats_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    dates <- sort(unique(player_stats$game_date))
    upcoming <- dates[dates >= Sys.Date()]
    initial <- if (length(upcoming) > 0) {
      min(upcoming)
    } else if (length(dates) > 0) {
      max(dates)
    } else {
      Sys.Date()
    }
    updateSelectInput(
      session,
      "game_date",
      choices = setNames(format(dates, "%Y-%m-%d"), format(dates, "%d %b")),
      selected = format(initial, "%Y-%m-%d")
    )

    observeEvent(
      input$game_date,
      {
        teams <- player_stats |>
          dplyr::filter(.data$game_date == as.Date(input$game_date)) |>
          dplyr::distinct(.data$team_abbreviation) |>
          dplyr::arrange(.data$team_abbreviation) |>
          dplyr::pull(.data$team_abbreviation)
        updateSelectInput(
          session,
          "team",
          choices = c("All teams" = "all", setNames(teams, teams)),
          selected = "all"
        )
      },
      ignoreInit = FALSE
    )

    output$table <- reactable::renderReactable({
      d <- player_stats_table(
        game_date = input$game_date,
        team = if (is.null(input$team)) "all" else input$team,
        player = if (is.null(input$player)) "" else input$player,
        sort_stat = if (is.null(input$sort_stat)) "pred_pts" else input$sort_stat
      )
      validate(need(nrow(d) > 0, "No projections for this filter."))

      stat_columns <- lapply(PLAYER_STAT_LABELS, function(label) {
        reactable::colDef(
          name = label,
          align = "right",
          format = reactable::colFormat(digits = 1)
        )
      })

      reactable::reactable(
        d,
        pagination = TRUE,
        defaultPageSize = 20,
        filterable = TRUE,
        defaultSorted = if (is.null(input$sort_stat)) "pred_pts" else input$sort_stat,
        defaultSortOrder = "desc",
        bordered = TRUE,
        highlight = TRUE,
        defaultColDef = reactable::colDef(
          align = "left",
          minWidth = 90,
          headerStyle = list(background = "#cce5ff")
        ),
        columns = c(
          list(
            game_date = reactable::colDef(
              name = "Game date",
              format = reactable::colFormat(date = TRUE)
            ),
            horizon = reactable::colDef(name = "H", align = "right"),
            team = reactable::colDef(name = "Team"),
            matchup = reactable::colDef(name = "Matchup"),
            player = reactable::colDef(name = "Player", minWidth = 150),
            p_play = reactable::colDef(
              name = "P(play)",
              align = "right",
              format = reactable::colFormat(digits = 3)
            ),
            minutes_if_plays = reactable::colDef(
              name = "Min if plays",
              align = "right",
              format = reactable::colFormat(digits = 1)
            ),
            expected_usage = reactable::colDef(
              name = "Exp usage",
              align = "right",
              format = reactable::colFormat(digits = 3)
            )
          ),
          stat_columns
        )
      )
    })
  })
}
