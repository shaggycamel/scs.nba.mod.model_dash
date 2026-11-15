# Injury duration model -----------------------------------------------------
# Bakes the published daily forecast view into the frames the dashboard reads.
# All outcome derivation lives here so the app never queries the database.

MODEL_ID <- "injury_duration"

rows <- DBI::dbGetQuery(db_con, "SELECT * FROM mod.injury_daily_forecast_vw") |>
  as_tibble() |>
  mutate(
    player_id = as.numeric(.data$player_id),
    games_missed_so_far = as.numeric(.data$games_missed_so_far),
    days_missed_so_far = as.numeric(.data$days_missed_so_far),
    pred_games_median = as.numeric(.data$pred_games_median),
    pred_games_mean = as.numeric(.data$pred_games_mean),
    actual_games_missed = as.numeric(.data$actual_games_missed),
    version = format(.data$model_created_at, "%Y-%m-%d", tz = "UTC")
  )

if (nrow(rows) == 0) {
  stop("mod.injury_daily_forecast_vw returned no rows", call. = FALSE)
}

# A game counts as observed only once it has a winner, so a scheduled row never
# makes a horizon look scoreable.
cutoff <- DBI::dbGetQuery(
  db_con,
  "SELECT max(game_date) AS cutoff
   FROM nba.league_game_schedule_vw
   WHERE team_winner IS NOT NULL"
)$cutoff[[1]]

seasons <- unique(rows$season)
player_ids <- unique(rows$player_id[!is.na(rows$player_id)])

rosters <- DBI::dbGetQuery(
  db_con,
  "SELECT season, team_slug, player_id, entry_date, exit_date
   FROM nba.team_roster_vw"
) |>
  as_tibble() |>
  mutate(player_id = as.numeric(.data$player_id)) |>
  filter(.data$season %in% seasons, !is.na(.data$player_id)) |>
  distinct() |>
  rename(roster_team = "team_slug")

schedule <- DBI::dbGetQuery(
  db_con,
  "SELECT season, game_id, game_date, team
   FROM nba.league_game_schedule_vw
   WHERE season_type = 'Regular Season'"
) |>
  as_tibble() |>
  filter(.data$season %in% seasons)

box <- if (length(player_ids) > 0) {
  DBI::dbGetQuery(
    db_con,
    sprintf(
      "SELECT DISTINCT player_id, game_id
       FROM nba.player_box_score_vw
       WHERE season_type = 'Regular Season'
         AND min > 0
         AND season IN (%s)
         AND player_id IN (%s)",
      paste(sprintf("'%s'", seasons), collapse = ","),
      paste(player_ids, collapse = ",")
    )
  ) |>
    as_tibble() |>
    mutate(player_id = as.numeric(.data$player_id), played = TRUE)
} else {
  tibble::tibble(player_id = numeric(), game_id = numeric(), played = logical())
}

# Next team game after each prediction, following the player's roster stint so a
# mid-absence trade uses the new team's calendar.
stints <- rows |>
  select("spell_id", "prediction_date", "player_id", "season", "team_slug") |>
  left_join(
    rosters,
    by = dplyr::join_by("player_id", "season", "prediction_date" >= "entry_date", "prediction_date" < "exit_date")
  ) |>
  distinct(spell_id, prediction_date, .keep_all = TRUE) |>
  mutate(look_team = coalesce(.data$roster_team, .data$team_slug))

next_game <- stints |>
  inner_join(
    schedule,
    by = dplyr::join_by("look_team" == "team", "season", "prediction_date" < "game_date")
  ) |>
  group_by(.data$spell_id, .data$prediction_date) |>
  slice_min(.data$game_date, n = 1, with_ties = TRUE) |>
  ungroup() |>
  left_join(box, by = c("player_id", "game_id")) |>
  mutate(played = !is.na(.data$played)) |>
  group_by(.data$spell_id, .data$prediction_date) |>
  summarise(next_game_date = min(.data$game_date), played = any(.data$played), .groups = "drop")

rows <- rows |>
  left_join(next_game, by = c("spell_id", "prediction_date")) |>
  mutate(
    days_bucket = as.character(cut(
      .data$days_missed_so_far,
      breaks = c(-Inf, 3, 7, 14, 30, Inf),
      labels = c("0-3", "4-7", "8-14", "15-30", "31+"),
      right = TRUE
    )),
    next_game_scoreable = !is.na(.data$next_game_date) & .data$next_game_date <= cutoff
  )

# One row per spell ---------------------------------------------------------
first_rows <- rows |>
  arrange(.data$spell_id, .data$prediction_date) |>
  group_by(.data$spell_id) |>
  slice_min(.data$prediction_date, n = 1, with_ties = FALSE) |>
  ungroup()

mdl_unit <- first_rows |>
  mutate(
    model_id = MODEL_ID,
    unit_id = .data$spell_id,
    label = .data$player_name,
    onset_date = .data$prediction_date,
    first_status = .data$status,
    onset_eligible = .data$days_missed_so_far <= 1,
    duration_days = as.numeric(
      dplyr::if_else(.data$return_seen, .data$actual_return_date, cutoff) - .data$injury_start_date
    ),
    pred_days = as.numeric(.data$pred_return_date - .data$injury_start_date),
    games_missed = .data$actual_games_missed,
    version_onset = .data$version
  ) |>
  select(
    "model_id",
    "unit_id",
    "label",
    "player_id",
    "season",
    "team_slug",
    "injury_start_date",
    "onset_date",
    "first_status",
    "body_region",
    "ailment_class",
    "history_imputed",
    "model_trained_through",
    "onset_eligible",
    "returned" = "return_seen",
    "censored",
    "censor_reason",
    "actual_return_date",
    "duration_days",
    "pred_days",
    "games_missed",
    "version_onset"
  )

# Long probability frame ----------------------------------------------------
mdl_prob <- purrr::map_dfr(c("next_game", "7d", "14d", "30d"), function(horizon) {
  if (horizon == "next_game") {
    out <- rows |>
      transmute(
        model_id = MODEL_ID,
        unit_id = .data$spell_id,
        as_of = .data$prediction_date,
        horizon = horizon,
        p = .data$p_next_game,
        y = as.integer(.data$played),
        scoreable = .data$next_game_scoreable,
        pred_return_date = .data$pred_return_date,
        pred_games_median = .data$pred_games_median,
        days_missed_so_far = .data$days_missed_so_far,
        days_bucket = .data$days_bucket,
        status = .data$status,
        version = .data$version
      )
  } else {
    days <- as.numeric(sub("d", "", horizon))
    scoreable <- rows$prediction_date + days <= cutoff
    out <- rows |>
      transmute(
        model_id = MODEL_ID,
        unit_id = .data$spell_id,
        as_of = .data$prediction_date,
        horizon = horizon,
        p = .data[[paste0("p_return_", horizon)]],
        y = as.integer(!is.na(.data$actual_return_date) & .data$actual_return_date <= .data$prediction_date + days),
        scoreable = scoreable,
        pred_return_date = .data$pred_return_date,
        pred_games_median = .data$pred_games_median,
        days_missed_so_far = .data$days_missed_so_far,
        days_bucket = .data$days_bucket,
        status = .data$status,
        version = .data$version
      )
  }
  out |> mutate(y = ifelse(.data$scoreable, .data$y, NA_integer_))
})

# The onset scope scores the forecast made when the injury was first seen, and
# drops spells already in progress when the table starts.
onset_units <- mdl_unit |>
  filter(.data$onset_eligible) |>
  pull(.data$unit_id)

mdl_prob <- bind_rows(
  mdl_prob |> mutate(scope = "nowcast"),
  mdl_prob |>
    filter(.data$unit_id %in% onset_units) |>
    group_by(.data$unit_id) |>
    filter(.data$as_of == min(.data$as_of)) |>
    ungroup() |>
    mutate(scope = "onset")
)

# Point frame ---------------------------------------------------------------
point_base <- rows

mdl_point <- bind_rows(
  point_base |>
    transmute(
      model_id = MODEL_ID,
      unit_id = .data$spell_id,
      as_of = .data$prediction_date,
      measure = "return_days",
      predicted = as.numeric(.data$pred_return_date - .data$prediction_date),
      actual = as.numeric(.data$actual_return_date - .data$prediction_date),
      scoreable = .data$return_seen,
      days_bucket = .data$days_bucket,
      status = .data$status,
      version = .data$version
    ),
  point_base |>
    transmute(
      model_id = MODEL_ID,
      unit_id = .data$spell_id,
      as_of = .data$prediction_date,
      measure = "games_missed",
      predicted = .data$pred_games_median,
      actual = .data$actual_games_missed,
      scoreable = .data$return_seen,
      days_bucket = .data$days_bucket,
      status = .data$status,
      version = .data$version
    )
) |>
  mutate(across(c("predicted", "actual"), as.numeric))

mdl_point <- bind_rows(
  mdl_point |> mutate(scope = "nowcast"),
  mdl_point |>
    filter(.data$unit_id %in% onset_units) |>
    group_by(.data$unit_id, .data$measure) |>
    slice_min(.data$as_of, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(scope = "onset")
)

# Coverage and quality ------------------------------------------------------
mdl_coverage <- rows |>
  group_by(.data$prediction_date) |>
  summarise(
    spells_active = n(),
    resolved = sum(.data$return_seen),
    censored = sum(.data$censored),
    scoreable_next = sum(.data$next_game_scoreable),
    scoreable_7d = sum(.data$prediction_date + 7 <= cutoff),
    .groups = "drop"
  ) |>
  transmute(
    model_id = MODEL_ID,
    as_of = .data$prediction_date,
    spells_active = .data$spells_active,
    resolved = .data$resolved,
    censored = .data$censored,
    scoreable_next = .data$scoreable_next,
    scoreable_7d = .data$scoreable_7d
  )

mdl_censor <- mdl_unit |>
  filter(.data$censored) |>
  count(.data$censor_reason, name = "n") |>
  transmute(
    model_id = MODEL_ID,
    censor_reason = coalesce(.data$censor_reason, "unknown"),
    n = .data$n
  )

# The model's target is the first game played, so a report that still files a
# played game as Out is a source-data defect, not a different outcome definition.
out_on_return <- DBI::dbGetQuery(
  db_con,
  "WITH sp AS (
     SELECT spell_id, player_id, season, max(actual_return_date) AS ret
     FROM mod.injury_daily_forecast_vw
     WHERE return_seen
     GROUP BY 1, 2, 3
   )
   SELECT count(*) AS spells,
          count(*) FILTER (WHERE EXISTS (
            SELECT 1 FROM nba.injuries_vw i
            WHERE i.nba_id = sp.player_id
              AND i.season = sp.season
              AND i.game_date = sp.ret
              AND i.status = 'Out'
          )) AS out_on_return
   FROM sp"
)

stale_out <- DBI::dbGetQuery(
  db_con,
  "SELECT count(*) AS stale_rows
   FROM nba.injuries_vw i
   WHERE i.status = 'Out'
     AND EXISTS (
       SELECT 1 FROM nba.player_box_score_vw b
       WHERE b.player_id = i.nba_id
         AND b.game_date = i.game_date
         AND b.season = i.season
         AND b.min > 0
     )"
)

out_rows <- DBI::dbGetQuery(db_con, "SELECT count(*) AS n FROM nba.injuries_vw WHERE status = 'Out'")

mdl_meta <- tibble::tibble(
  model_id = MODEL_ID,
  baked_at = Sys.time(),
  cutoff = cutoff,
  first_prediction_date = min(rows$prediction_date),
  last_prediction_date = max(rows$prediction_date),
  prediction_days = n_distinct(rows$prediction_date),
  spells = nrow(mdl_unit),
  resolved_spells = sum(mdl_unit$returned),
  censored_spells = sum(mdl_unit$censored),
  history_imputed_spells = sum(mdl_unit$history_imputed),
  onset_eligible_spells = sum(mdl_unit$onset_eligible),
  out_on_return_spells = as.numeric(out_on_return$out_on_return[[1]]),
  out_report_rows = as.numeric(out_rows$n[[1]]),
  stale_out_rows = as.numeric(stale_out$stale_rows[[1]]),
  model_trained_through = dplyr::first(rows$model_trained_through),
  model_created_at = dplyr::first(rows$model_created_at),
  latest_generated_at = max(rows$generated_at)
) |>
  mutate(
    expected_days = as.numeric(.data$last_prediction_date - .data$first_prediction_date) + 1,
    missing_days = .data$expected_days - .data$prediction_days,
    stale_out_share = .data$stale_out_rows / .data$out_report_rows,
    model_age_days = as.numeric(Sys.Date() - as.Date(.data$model_created_at))
  )

save_data("injury_unit", mdl_unit)
save_data("injury_prob", mdl_prob)
save_data("injury_point", mdl_point)
save_data("injury_coverage", mdl_coverage)
save_data("injury_censor", mdl_censor)
save_data("injury_meta", mdl_meta)
