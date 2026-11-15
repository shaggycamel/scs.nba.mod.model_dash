# Filtering and shaping helpers for the app --------------------------------

SCOPE_LEVELS <- c("onset", "nowcast")

BASE_RATE_LABELS <- c(
  next_game = "Next game",
  `7d` = "Within 7 days",
  `14d` = "Within 14 days",
  `30d` = "Within 30 days"
)

MEASURE_LABELS <- c(
  return_days = "Return date (days)",
  games_missed = "Games missed"
)

DIM_LABELS <- c(
  body_region = "Body region",
  ailment_class = "Ailment class",
  status = "Report status",
  team_slug = "Team",
  history_imputed = "History imputed",
  days_bucket = "Days out",
  version = "Model version"
)

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

trend_table <- function(
  metric,
  ailment = "all",
  body_part = "all",
  window_days = 28,
  prob = mdl_prob,
  point = mdl_point,
  units = mdl_unit
) {
  dims <- dplyr::select(units, "unit_id", "ailment_class", "body_region")

  out <- if (startsWith(metric, "brier")) {
    horizon_value <- sub("^brier_", "", metric)
    horizon_value <- if (horizon_value == "next") "next_game" else horizon_value
    d <- prob |>
      dplyr::filter(
        .data$model_id == "injury_duration",
        .data$scope == "nowcast",
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
        .data$scope == "nowcast",
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

# Current injuries: the latest forecast for every spell that has not resolved.
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

SCOPE_COLOURS <- c(onset = "#133DEF", nowcast = "#8A8D93")

scope_factor <- function(x) {
  factor(x, levels = SCOPE_LEVELS)
}

scope_palette <- function() {
  unname(SCOPE_COLOURS[SCOPE_LEVELS])
}

scope_choices <- function() {
  c("Both" = "both", "Onset" = "onset", "Nowcast" = "nowcast")
}

scope_label <- function(scope) {
  c(onset = "Onset", nowcast = "Nowcast")[scope]
}

measure_choices <- function() {
  stats::setNames(names(MEASURE_LABELS), unname(MEASURE_LABELS))
}

dim_choices <- function() {
  stats::setNames(names(DIM_LABELS), unname(DIM_LABELS))
}

filter_scope <- function(df, sc) {
  if (is.null(sc) || identical(sc, "both")) {
    return(df)
  }
  dplyr::filter(df, .data$scope == sc)
}

filter_window <- function(df, date_range) {
  if (is.null(date_range) || any(is.na(date_range))) {
    return(df)
  }
  dplyr::filter(df, .data$as_of >= date_range[1], .data$as_of <= date_range[2])
}

filter_version <- function(df, ver) {
  if (is.null(ver) || identical(ver, "all")) {
    return(df)
  }
  dplyr::filter(df, .data$version == ver)
}

filter_horizon <- function(df, hz) {
  dplyr::filter(df, .data$horizon == hz)
}

filter_measure <- function(df, msr) {
  dplyr::filter(df, .data$measure == msr)
}

filter_scoreable <- function(df) {
  dplyr::filter(df, .data$scoreable)
}

version_choices <- function(versions) {
  c("All versions" = "all", stats::setNames(versions, versions))
}

# Only offer horizons that have at least one scored forecast, so an early-season
# 30-day horizon does not render an empty panel.
model_horizons <- function(model_id) {
  d <- dplyr::filter(mdl_prob, .data$model_id == model_id, .data$scoreable)
  available <- names(BASE_RATE_LABELS)[names(BASE_RATE_LABELS) %in% unique(d$horizon)]
  stats::setNames(available, unname(BASE_RATE_LABELS[available]))
}

model_dates <- function(model_id) {
  d <- dplyr::filter(mdl_coverage, .data$model_id == model_id)
  if (nrow(d) == 0) {
    return(c(Sys.Date(), Sys.Date()))
  }
  range(d$as_of)
}

model_versions <- function(model_id) {
  sort(unique(dplyr::filter(mdl_prob, .data$model_id == model_id)$version))
}

# Attach the unit-level dimensions needed for slicing.
with_unit_dims <- function(df, units, dims) {
  dims <- intersect(dims, c("body_region", "ailment_class", "status", "team_slug", "history_imputed", "days_bucket"))
  if (length(dims) == 0) {
    return(df)
  }
  d <- dplyr::left_join(
    df,
    dplyr::select(units, "unit_id", dplyr::all_of(dims)),
    by = "unit_id"
  )
  d
}

# Filtered views used by every panel. The frames are small enough that each
# panel can recompute from raw rows on every filter change.
prob_view <- function(filters) {
  mdl_prob |>
    dplyr::filter(.data$model_id == filters$model_id) |>
    filter_scope(filters$scope) |>
    filter_window(filters$dates) |>
    filter_version(filters$version)
}

point_view <- function(filters) {
  mdl_point |>
    dplyr::filter(.data$model_id == filters$model_id) |>
    filter_scope(filters$scope) |>
    filter_window(filters$dates) |>
    filter_version(filters$version)
}

units_view <- function(filters) {
  ids <- unique(c(prob_view(filters)$unit_id, point_view(filters)$unit_id))
  dplyr::filter(mdl_unit, .data$model_id == filters$model_id, .data$unit_id %in% ids)
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
