# Game day contract ---------------------------------------------------------
# One row per player per unplayed fixture from the bake date onward. The view
# already resolves availability (report > injury curve > state), so this script
# only narrows the columns the dashboard shows.

game_day <- DBI::dbGetQuery(
  db_con,
  "SELECT game_date, game_id, team_abbreviation, opponent, home, season,
          player_id, player_name, position, depth_rank,
          report_status, report_available, p_play, p_play_source,
          p_play_injury, minutes_if_plays, expected_minutes, expected_usage,
          spell_id, injury_start_date, pred_return_date, p_next_game,
          p_return_7d, generated_at, model_version
   FROM mod.game_day_vw"
) |>
  as_tibble() |>
  mutate(
    player_id = as.numeric(.data$player_id),
    depth_rank = as.numeric(.data$depth_rank),
    across(
      c(
        "p_play",
        "p_play_injury",
        "minutes_if_plays",
        "expected_minutes",
        "expected_usage",
        "p_next_game",
        "p_return_7d"
      ),
      as.numeric
    )
  )

save_data("game_day", game_day)
