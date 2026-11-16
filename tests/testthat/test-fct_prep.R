test_that("trend metric choices use codes as values and labels as names", {
  choices <- trend_metric_choices()
  expect_equal(unname(choices), names(TREND_METRICS))
  expect_equal(names(choices), unname(TREND_METRICS))
  expect_true(all(unname(choices) %in% names(TREND_METRICS)))
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
