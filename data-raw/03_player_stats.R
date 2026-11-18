# Player stat projections ----------------------------------------------------
# One row per player and fixture from the latest published snapshot. The page
# defaults to the next unplayed slate; completed dates remain available because
# the forecast table keeps the backfill.

player_stats <- DBI::dbGetQuery(
  db_con,
  "SELECT prediction_date, horizon_days, game_date, game_id, team_id,
          player_id, season, team_abbreviation, opponent, home, player_name,
          p_play, minutes_if_plays, expected_minutes, expected_usage,
          pred_fga, pred_fgm, pred_fg3a, pred_fg3m, pred_fta, pred_ftm,
          pred_oreb, pred_dreb, pred_ast, pred_stl, pred_blk, pred_tov,
          pred_pf, pred_pts, pred_reb, model_version, model_cutoff, generated_at
   FROM mod.player_stat_forecast_vw
   WHERE season = (SELECT max(season) FROM mod.player_stat_forecast_vw)"
) |>
  as_tibble() |>
  mutate(
    prediction_date = as.Date(.data$prediction_date),
    game_date = as.Date(.data$game_date),
    across(c("game_id", "team_id", "player_id"), as.numeric),
    across(
      c(
        "p_play",
        "minutes_if_plays",
        "expected_minutes",
        "expected_usage",
        "pred_fga",
        "pred_fgm",
        "pred_fg3a",
        "pred_fg3m",
        "pred_fta",
        "pred_ftm",
        "pred_oreb",
        "pred_dreb",
        "pred_ast",
        "pred_stl",
        "pred_blk",
        "pred_tov",
        "pred_pf",
        "pred_pts",
        "pred_reb"
      ),
      as.numeric
    )
  )

save_data("player_stats", player_stats)
