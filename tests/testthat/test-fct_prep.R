test_that("trend metric choices use codes as values and labels as names", {
  choices <- trend_metric_choices()
  expect_equal(unname(choices), names(TREND_METRICS))
  expect_equal(names(choices), unname(TREND_METRICS))
  expect_true(all(unname(choices) %in% names(TREND_METRICS)))
})

test_that("player stat choices map readable labels to baked columns", {
  choices <- player_stat_choices()
  expect_equal(unname(choices), names(PLAYER_STAT_LABELS))
  expect_equal(names(choices), unname(PLAYER_STAT_LABELS))
})

test_that("player_stats_table filters and shapes a fixture", {
  toy <- tibble::tibble(
    game_date = as.Date(c("2026-11-18", "2026-11-18")),
    horizon_days = c(0L, 0L),
    team_abbreviation = c("BOS", "LAL"),
    opponent = c("NYK", "GSW"),
    home = c(TRUE, FALSE),
    player_name = c("Player A", "Player B"),
    p_play = c(0.9, 0.8),
    minutes_if_plays = c(32, 28),
    expected_usage = c(0.21, 0.18),
    pred_pts = c(20, 30),
    pred_reb = c(5, 7),
    pred_ast = c(4, 6),
    pred_fg3m = c(2, 3),
    pred_stl = c(1, 1),
    pred_blk = c(0, 1),
    pred_tov = c(2, 3),
    pred_fga = c(14, 20),
    pred_fta = c(4, 6),
    pred_pf = c(2, 3)
  )

  out <- player_stats_table(
    toy,
    game_date = "2026-11-18",
    team = "BOS",
    player = "A",
    sort_stat = "pred_pts"
  )
  expect_equal(nrow(out), 1)
  expect_equal(out$matchup, "vs NYK")
  expect_equal(out$team, "BOS")
  expect_equal(out$player, "Player A")

  all <- player_stats_table(toy, sort_stat = "pred_pts")
  expect_equal(all$player, c("Player B", "Player A"))
})

test_that("game_day_table shapes fixtures and their availability source", {
  toy <- tibble::tibble(
    game_date = as.Date(c("2026-11-18", "2026-11-18")),
    team_abbreviation = c("BOS", "LAL"),
    opponent = c("NYK", "GSW"),
    home = c(TRUE, FALSE),
    player_name = c("Player A", "Player B"),
    position = c("G", "F"),
    depth_rank = c(1, 2),
    report_status = c("Questionable", NA_character_),
    p_play = c(0.7, 0.4),
    p_play_source = c("report", "injury"),
    minutes_if_plays = c(32, 28),
    expected_minutes = c(22.4, 11.2),
    expected_usage = c(0.21, 0.14)
  )

  out <- game_day_table(toy)

  expect_equal(out$matchup, c("vs NYK", "@ GSW"))
  expect_equal(out$source, c("Injury report", "Injury curve"))
  expect_equal(out$player, c("Player A", "Player B"))
  expect_equal(out$report, c("Questionable", NA_character_))
})
