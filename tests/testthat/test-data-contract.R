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

test_that("horizons that cannot be scored are not offered", {
  skip_if_not(file.exists(test_path("..", "..", "data", "mdl_prob.rda")), "data not baked")
  offered <- model_horizons("injury_duration")
  expect_true(all(unname(offered) %in% mdl_prob$horizon[mdl_prob$scoreable]))
})

test_that("the injury table fields are baked", {
  skip_if_not(file.exists(test_path("..", "..", "data", "mdl_unit.rda")), "data not baked")
  expect_true(all(c("injury_reason", "injury_start_date", "body_region") %in% names(mdl_unit)))
  expect_gt(sum(!is.na(mdl_unit$injury_reason)), 0)
})

test_that("injury text is cleaned for display", {
  expect_equal(clean_reason("Injury/Illness - Left Ankle Sprain"), "Left Ankle Sprain")
  expect_true(is.na(clean_reason("-")))
  expect_true(is.na(clean_reason(NA_character_)))
  expect_equal(body_part_label("knee"), "Knee")
  expect_equal(body_part_label(NA_character_), "Unknown")
})
