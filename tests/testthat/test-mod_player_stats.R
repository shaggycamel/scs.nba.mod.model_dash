test_that("the player stats module renders for a baked slate", {
  skip_if_not(
    file.exists(test_path("..", "..", "data", "player_stats.rda")),
    "data not baked"
  )
  dates <- sort(unique(player_stats$game_date))
  upcoming <- dates[dates >= Sys.Date()]
  initial <- if (length(upcoming) > 0) min(upcoming) else max(dates)

  testServer(mod_player_stats_server, {
    session$setInputs(
      game_date = format(initial, "%Y-%m-%d"),
      team = "all",
      player = "",
      sort_stat = "pred_pts"
    )
    expect_true(nzchar(output$table))
  })
})
