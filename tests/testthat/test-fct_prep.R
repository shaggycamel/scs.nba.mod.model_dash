test_that("trend metric choices use codes as values and labels as names", {
  choices <- trend_metric_choices()
  expect_equal(unname(choices), names(TREND_METRICS))
  expect_equal(names(choices), unname(TREND_METRICS))
  expect_true(all(unname(choices) %in% names(TREND_METRICS)))
})
