# Shared metric definitions -------------------------------------------------
# The app computes every accuracy figure from these functions, so a metric is
# defined once no matter which model or panel uses it.

PROB_EPSILON <- 1e-6

POINT_TOLERANCES <- list(
  return_days = c(1, 3, 7),
  games_missed = c(1, 2, 5)
)

# Rank-based AUC (Mann-Whitney), so no model dependency beyond stats.
auc_score <- function(p, y) {
  n_pos <- sum(y == 1)
  n_neg <- sum(y == 0)
  if (n_pos == 0 || n_neg == 0) {
    return(NA_real_)
  }
  w <- stats::wilcox.test(p[y == 1], p[y == 0], exact = FALSE)$statistic
  as.numeric(w) / (n_pos * n_neg)
}

prob_metrics <- function(p, y) {
  ok <- !is.na(p) & !is.na(y)
  p <- as.numeric(p[ok])
  y <- as.integer(y[ok])
  n <- length(p)
  if (n == 0) {
    return(tibble::tibble(
      n = 0L,
      n_pos = 0L,
      base_rate = NA_real_,
      mean_p = NA_real_,
      brier = NA_real_,
      log_loss = NA_real_,
      auc = NA_real_,
      brier_skill = NA_real_
    ))
  }
  p <- pmin(pmax(p, PROB_EPSILON), 1 - PROB_EPSILON)
  base <- mean(y)
  brier <- mean((p - y)^2)
  brier_base <- mean((base - y)^2)
  tibble::tibble(
    n = n,
    n_pos = sum(y),
    base_rate = base,
    mean_p = mean(p),
    brier = brier,
    log_loss = -mean(y * log(p) + (1 - y) * log(1 - p)),
    auc = auc_score(p, y),
    brier_skill = if (brier_base > 0) 1 - brier / brier_base else NA_real_
  )
}

prob_metrics_by <- function(df, ...) {
  df |>
    dplyr::group_by(...) |>
    dplyr::group_modify(~ prob_metrics(.x$p, .x$y)) |>
    dplyr::ungroup()
}

calibration_table <- function(p, y, bins = 10) {
  ok <- !is.na(p) & !is.na(y)
  p <- as.numeric(p[ok])
  y <- as.integer(y[ok])
  if (length(p) == 0) {
    return(tibble::tibble(bin = integer(), p_mean = numeric(), y_rate = numeric(), n = integer()))
  }
  tibble::tibble(p = p, y = y, bin = pmin(floor(p * bins) + 1L, bins)) |>
    dplyr::group_by(.data$bin) |>
    dplyr::summarise(p_mean = mean(.data$p), y_rate = mean(.data$y), n = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(.data$bin)
}

point_metrics <- function(predicted, actual, tolerances) {
  ok <- !is.na(predicted) & !is.na(actual)
  error <- as.numeric(predicted[ok]) - as.numeric(actual[ok])
  n <- length(error)
  if (n == 0) {
    out <- tibble::tibble(n = 0L, mae = NA_real_, median_ae = NA_real_, bias = NA_real_, p90_ae = NA_real_)
  } else {
    abs_error <- abs(error)
    out <- tibble::tibble(
      n = n,
      mae = mean(abs_error),
      median_ae = stats::median(abs_error),
      bias = mean(error),
      p90_ae = unname(stats::quantile(abs_error, 0.9))
    )
  }
  for (tol in tolerances) {
    out[[paste0("within_", tol)]] <- if (n == 0) NA_real_ else mean(abs(error) <= tol)
  }
  out
}

point_metrics_by <- function(df, tolerances, ...) {
  df |>
    dplyr::group_by(...) |>
    dplyr::group_modify(~ point_metrics(.x$predicted, .x$actual, tolerances)) |>
    dplyr::ungroup()
}

# Harrell's C on right-censored durations. `predicted` is a duration in the same
# unit as `time`, so a larger value means a later return.
c_index <- function(time, event, predicted) {
  ok <- !is.na(time) & !is.na(event) & !is.na(predicted)
  time <- as.numeric(time[ok])
  event <- as.logical(event[ok])
  predicted <- as.numeric(predicted[ok])
  n <- length(time)
  if (n < 3) {
    return(NA_real_)
  }
  concordant <- 0
  comparable <- 0
  for (i in seq_len(n - 1)) {
    for (j in seq.int(i + 1, n)) {
      if (time[i] == time[j]) {
        next
      }
      earlier <- if (time[i] < time[j]) i else j
      later <- if (earlier == i) j else i
      # A pair is comparable when the earlier spell has an observed return
      # before the later one's censoring time.
      if (!event[earlier]) {
        next
      }
      comparable <- comparable + 1
      if (predicted[earlier] < predicted[later]) {
        concordant <- concordant + 1
      } else if (predicted[earlier] == predicted[later]) {
        concordant <- concordant + 0.5
      }
    }
  }
  if (comparable == 0) NA_real_ else concordant / comparable
}

# Weighted trailed mean, used for rolling Brier and MAE: each day contributes
# its own mean weighted by the number of scored rows.
rolling_weighted <- function(date, value, n, window_days) {
  vapply(
    date,
    function(day) {
      keep <- date > day - window_days & date <= day
      value <- value[keep]
      n <- n[keep]
      ok <- !is.na(value) & !is.na(n) & n > 0
      if (!any(ok)) NA_real_ else sum(value[ok] * n[ok]) / sum(n[ok])
    },
    numeric(1)
  )
}
