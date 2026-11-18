# Filtering and shaping helpers for the app --------------------------------

BODY_PART_LABELS <- c(
  achilles = "Achilles",
  ankle = "Ankle",
  back_core = "Back",
  elbow_arm = "Elbow/arm",
  foot = "Foot",
  hamstring = "Hamstring",
  hand_wrist = "Hand/wrist",
  head_face = "Head/face",
  hip_groin = "Hip/groin",
  knee = "Knee",
  knee_ligament = "Knee (ligament)",
  lower_leg = "Lower leg",
  neck = "Neck",
  quad = "Quad",
  shoulder = "Shoulder",
  systemic = "Systemic",
  unknown = "Unknown"
)

body_part_label <- function(x) {
  out <- unname(BODY_PART_LABELS[x])
  out[is.na(out)] <- "Unknown"
  out
}

AILMENT_LABELS <- c(
  illness = "Illness",
  sprain = "Sprain",
  strain = "Strain",
  contusion = "Contusion",
  surgery = "Surgery",
  tendinopathy = "Tendinopathy",
  rupture_tear = "Rupture/tear",
  fracture = "Fracture",
  concussion = "Concussion",
  dislocation = "Dislocation"
)

ailment_label <- function(x) {
  out <- unname(AILMENT_LABELS[x])
  unknown <- is.na(out) & !is.na(x)
  out[unknown] <- paste0(
    toupper(substr(x[unknown], 1, 1)),
    substr(x[unknown], 2, nchar(x[unknown]))
  )
  out[is.na(x)] <- "Not reported"
  out
}

TREND_METRICS <- c(
  brier_next = "Brier - next game",
  brier_7d = "Brier - 7 days",
  brier_14d = "Brier - 14 days",
  mae_games = "MAE - games missed",
  mae_days = "MAE - return days"
)

trend_metric_choices <- function() {
  stats::setNames(names(TREND_METRICS), unname(TREND_METRICS))
}

trend_table <- function(
  metric,
  scope = "nowcast",
  ailment = "all",
  body_part = "all",
  window_days = 28,
  prob = mdl_prob,
  point = mdl_point,
  units = mdl_unit
) {
  dims <- dplyr::select(units, "unit_id", "ailment_class", "body_region")
  scope_value <- scope

  out <- if (startsWith(metric, "brier")) {
    horizon_value <- sub("^brier_", "", metric)
    horizon_value <- if (horizon_value == "next") "next_game" else horizon_value
    d <- prob |>
      dplyr::filter(
        .data$model_id == "injury_duration",
        .data$scope == scope_value,
        .data$horizon == horizon_value,
        .data$scoreable
      ) |>
      dplyr::left_join(dims, by = "unit_id")
    if (ailment != "all") {
      d <- dplyr::filter(d, .data$ailment_class == ailment)
    }
    if (body_part != "all") {
      d <- dplyr::filter(d, .data$body_region == body_part)
    }
    d |>
      dplyr::group_by(.data$as_of) |>
      dplyr::group_modify(~ prob_metrics(.x$p, .x$y)) |>
      dplyr::ungroup() |>
      dplyr::transmute(as_of = .data$as_of, value = .data$brier, n = .data$n)
  } else {
    measure_value <- if (metric == "mae_games") "games_missed" else "return_days"
    d <- point |>
      dplyr::filter(
        .data$model_id == "injury_duration",
        .data$scope == scope_value,
        .data$measure == measure_value,
        .data$scoreable
      ) |>
      dplyr::left_join(dims, by = "unit_id")
    if (ailment != "all") {
      d <- dplyr::filter(d, .data$ailment_class == ailment)
    }
    if (body_part != "all") {
      d <- dplyr::filter(d, .data$body_region == body_part)
    }
    d |>
      dplyr::group_by(.data$as_of) |>
      dplyr::group_modify(
        ~ point_metrics(.x$predicted, .x$actual, POINT_TOLERANCES[[measure_value]])
      ) |>
      dplyr::ungroup() |>
      dplyr::transmute(as_of = .data$as_of, value = .data$mae, n = .data$n)
  }

  out |>
    dplyr::mutate(rolled = rolling_weighted(.data$as_of, .data$value, .data$n, window_days)) |>
    dplyr::arrange(.data$as_of)
}

# One row per injury spell: the latest forecast plus the actual return once the
# spell resolves.
injuries_table <- function(model_id, prob = mdl_prob, units = mdl_unit) {
  latest <- prob |>
    dplyr::filter(.data$model_id == model_id, .data$scope == "nowcast", .data$horizon == "next_game") |>
    dplyr::group_by(.data$unit_id) |>
    dplyr::slice_max(.data$as_of, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  latest |>
    dplyr::left_join(
      dplyr::select(
        units,
        "unit_id",
        "label",
        "team_slug",
        "injury_start_date",
        "ailment_class",
        "body_region",
        "actual_return_date",
        "returned",
        "censor_reason"
      ),
      by = "unit_id"
    ) |>
    dplyr::filter(
      .data$returned | is.na(.data$censor_reason) | !grepl("^left_", .data$censor_reason)
    ) |>
    dplyr::transmute(
      forecast = .data$as_of,
      team = .data$team_slug,
      player = .data$label,
      ailment = ailment_label(.data$ailment_class),
      body_part = body_part_label(.data$body_region),
      injured = .data$injury_start_date,
      predicted = .data$pred_return_date,
      actual = .data$actual_return_date
    ) |>
    dplyr::arrange(dplyr::desc(.data$injured), .data$player)
}

PLAY_SOURCE_LABELS <- c(
  report = "Injury report",
  injury = "Injury curve",
  state = "State only"
)

play_source_label <- function(x) {
  out <- unname(PLAY_SOURCE_LABELS[x])
  out[is.na(out)] <- "Unknown"
  out
}

# One row per player and upcoming fixture, shaped for the game-day table.
game_day_table <- function(rows = game_day) {
  rows |>
    dplyr::transmute(
      game_date = .data$game_date,
      team = .data$team_abbreviation,
      matchup = paste0(dplyr::if_else(.data$home, "vs ", "@ "), .data$opponent),
      player = .data$player_name,
      position = .data$position,
      depth = .data$depth_rank,
      report = .data$report_status,
      p_play = .data$p_play,
      source = play_source_label(.data$p_play_source),
      minutes_if_plays = .data$minutes_if_plays,
      expected_minutes = .data$expected_minutes,
      expected_usage = .data$expected_usage
    ) |>
    dplyr::arrange(.data$game_date, .data$team, .data$depth, .data$player)
}

filter_horizon <- function(df, hz) {
  dplyr::filter(df, .data$horizon == hz)
}

# Player stat projections ---------------------------------------------------

PLAYER_STAT_LABELS <- c(
  pred_pts = "Pts",
  pred_reb = "Reb",
  pred_ast = "Ast",
  pred_fg3m = "3PM",
  pred_stl = "Stl",
  pred_blk = "Blk",
  pred_tov = "TOV",
  pred_fga = "FGA",
  pred_fta = "FTA",
  pred_pf = "PF"
)

player_stat_choices <- function() {
  PLAYER_STAT_LABELS
}

# One row per player and fixture, filtered to a date, team and player.
player_stats_table <- function(
  rows = player_stats,
  game_date = NULL,
  team = "all",
  player = "",
  sort_stat = "pred_pts"
) {
  d <- rows
  if (!is.null(game_date)) {
    d <- dplyr::filter(d, .data$game_date == as.Date(game_date))
  }
  if (!identical(team, "all")) {
    d <- dplyr::filter(d, .data$team_abbreviation == team)
  }
  if (!is.null(player) && nzchar(player)) {
    d <- dplyr::filter(
      d,
      grepl(tolower(player), tolower(.data$player_name), fixed = TRUE)
    )
  }

  d <- d |>
    dplyr::transmute(
      game_date = .data$game_date,
      horizon = .data$horizon_days,
      team = .data$team_abbreviation,
      matchup = paste0(dplyr::if_else(.data$home, "vs ", "@ "), .data$opponent),
      player = .data$player_name,
      p_play = .data$p_play,
      minutes_if_plays = .data$minutes_if_plays,
      expected_usage = .data$expected_usage,
      pred_pts = .data$pred_pts,
      pred_reb = .data$pred_reb,
      pred_ast = .data$pred_ast,
      pred_fg3m = .data$pred_fg3m,
      pred_stl = .data$pred_stl,
      pred_blk = .data$pred_blk,
      pred_tov = .data$pred_tov,
      pred_fga = .data$pred_fga,
      pred_fta = .data$pred_fta,
      pred_pf = .data$pred_pf
    )

  if (!sort_stat %in% names(d)) {
    sort_stat <- "pred_pts"
  }
  dplyr::arrange(d, dplyr::desc(.data[[sort_stat]]), .data$player)
}

# One row per registered model, latest computed headline figures. Uses the full
# baked window so the scorecard always reflects the current state.
scorecard_table <- function(prob, point, units, registry) {
  if (nrow(registry) == 0) {
    return(tibble::tibble())
  }
  rows <- lapply(seq_len(nrow(registry)), function(i) {
    model_id <- registry$model_id[[i]]
    p <- dplyr::filter(prob, .data$model_id == model_id, .data$scoreable)
    pt <- dplyr::filter(point, .data$model_id == model_id, .data$scoreable)
    u <- dplyr::filter(units, .data$model_id == model_id)

    brier <- function(sc, hz) {
      d <- p |> dplyr::filter(.data$scope == sc)
      if (hz != "any") {
        d <- d |> filter_horizon(hz)
      }
      if (nrow(d) == 0) NA_real_ else prob_metrics(d$p, d$y)$brier
    }
    games_mae <- function(sc) {
      d <- pt |> dplyr::filter(.data$scope == sc, .data$measure == "games_missed")
      if (nrow(d) == 0) NA_real_ else point_metrics(d$predicted, d$actual, c(1, 2, 5))$mae
    }
    auc <- function(sc, hz) {
      d <- p |> dplyr::filter(.data$scope == sc) |> filter_horizon(hz)
      if (nrow(d) == 0) NA_real_ else prob_metrics(d$p, d$y)$auc
    }
    cidx <- function() {
      d <- dplyr::filter(u, .data$onset_eligible)
      if (nrow(d) < 3) NA_real_ else c_index(d$duration_days, d$returned, d$pred_days)
    }
    tibble::tibble(
      model_id = model_id,
      label = registry$label[[i]],
      brier_next_onset = brier("onset", "next_game"),
      brier_next_now = brier("nowcast", "next_game"),
      brier_7d_onset = brier("onset", "7d"),
      brier_7d_now = brier("nowcast", "7d"),
      auc_7d_onset = auc("onset", "7d"),
      games_mae_onset = games_mae("onset"),
      games_mae_now = games_mae("nowcast"),
      c_index = cidx(),
      resolved = sum(u$returned),
      censored = sum(u$censored)
    )
  })
  dplyr::bind_rows(rows)
}
