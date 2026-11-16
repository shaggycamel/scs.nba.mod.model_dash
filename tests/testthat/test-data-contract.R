test_that("baked frames satisfy the dashboard contract", {
  skip_if_not(file.exists(test_path("..", "..", "data", "mdl_prob.rda")), "data not baked")

  expect_s3_class(mdl_prob, "tbl_df")
  expect_true(all(
    c(
      "model_id",
      "unit_id",
      "as_of",
      "horizon",
      "p",
      "y",
      "scoreable",
      "scope",
      "days_bucket",
      "version",
      "pred_return_date"
    ) %in%
      names(mdl_prob)
  ))
  expect_true(all(c("onset", "nowcast") %in% mdl_prob$scope))
  expect_false(any(mdl_prob$scoreable & is.na(mdl_prob$y)))
  expect_false(any(is.na(mdl_prob$y[mdl_prob$scoreable])))

  expect_s3_class(mdl_point, "tbl_df")
  expect_true(all(c("measure", "predicted", "actual", "scoreable", "scope") %in% names(mdl_point)))
  expect_false(any(mdl_point$scoreable & is.na(mdl_point$actual)))

  expect_s3_class(mdl_unit, "tbl_df")
  expect_true(all(c("unit_id", "duration_days", "pred_days", "returned", "onset_eligible") %in% names(mdl_unit)))

  expect_true(all(mdl_meta$cutoff <= Sys.Date()))
  expect_true(all(mdl_meta$missing_days >= 0))
})

test_that("onset rows are the first forecast for each spell", {
  skip_if_not(file.exists(test_path("..", "..", "data", "mdl_prob.rda")), "data not baked")

  onset <- dplyr::filter(mdl_prob, .data$scope == "onset")
  expect_equal(dplyr::n_distinct(onset$unit_id), nrow(onset) / dplyr::n_distinct(onset$horizon))

  first <- mdl_prob |>
    dplyr::filter(.data$scope == "nowcast") |>
    dplyr::group_by(.data$unit_id) |>
    dplyr::summarise(first = min(.data$as_of), .groups = "drop")
  check <- dplyr::left_join(onset, first, by = "unit_id")
  expect_true(all(check$as_of == check$first))
})

test_that("the injury table fields are baked", {
  skip_if_not(file.exists(test_path("..", "..", "data", "mdl_unit.rda")), "data not baked")
  expect_true(all(c("ailment_class", "injury_start_date", "body_region") %in% names(mdl_unit)))
  expect_gt(sum(!is.na(mdl_unit$ailment_class)), 0)
})

test_that("the game day frame is baked", {
  skip_if_not(file.exists(test_path("..", "..", "data", "game_day.rda")), "data not baked")
  expect_s3_class(game_day, "tbl_df")
  expect_true(all(
    c("game_date", "team_abbreviation", "player_name", "p_play", "p_play_source") %in%
      names(game_day)
  ))
  expect_false(any(is.na(game_day$p_play)))
  expect_true(all(game_day$p_play_source %in% names(PLAY_SOURCE_LABELS)))
})

test_that("ailment and body part labels are readable", {
  expect_equal(ailment_label("rupture_tear"), "Rupture/tear")
  expect_equal(ailment_label("sprain"), "Sprain")
  expect_equal(ailment_label(NA_character_), "Not reported")
  expect_equal(body_part_label("knee"), "Knee")
  expect_equal(body_part_label(NA_character_), "Unknown")
})

test_that("the accuracy trend is a daily series that honours slices", {
  skip_if_not(file.exists(test_path("..", "..", "data", "mdl_prob.rda")), "data not baked")

  full <- trend_table("brier_7d")
  expect_true(all(c("as_of", "value", "n", "rolled") %in% names(full)))
  expect_gt(nrow(full), 0)

  sliced <- trend_table("brier_7d", ailment = "illness")
  expect_lt(sum(sliced$n), sum(full$n))

  expect_gt(nrow(trend_table("mae_games")), 0)
})
