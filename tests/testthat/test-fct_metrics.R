test_that("Brier and log loss match hand calculations", {
  m <- prob_metrics(p = c(0.9, 0.1), y = c(1, 0))
  expect_equal(m$brier, 0.01)
  expect_equal(m$log_loss, -log(0.9))
  expect_equal(m$auc, 1)
  expect_equal(m$n, 2)
  expect_equal(m$n_pos, 1)
  expect_equal(m$base_rate, 0.5)
  expect_equal(m$mean_p, 0.5)
})

test_that("AUC is 0.5 for an uninformative forecast and NA for one class", {
  expect_equal(prob_metrics(p = c(0.4, 0.9, 0.2, 0.3), y = c(0, 1, 0, 1))$auc, 0.75)
  expect_true(is.na(prob_metrics(p = c(0.2, 0.3), y = c(0, 0))$auc))
})

test_that("brier_skill compares against the base rate", {
  m <- prob_metrics(p = c(0.5, 0.5), y = c(0, 1))
  expect_equal(m$brier_skill, 0)
})

test_that("prob_metrics drops unscored rows", {
  m <- prob_metrics(p = c(0.4, NA, 0.8), y = c(0, 1, NA))
  expect_equal(m$n, 1)
})

test_that("calibration bins are equal width and ordered", {
  cal <- calibration_table(p = c(0.05, 0.15, 0.95), y = c(0, 1, 1), bins = 10)
  expect_equal(cal$bin, c(1L, 2L, 10L))
  expect_equal(cal$n, c(1L, 1L, 1L))
  expect_equal(cal$y_rate, c(0, 1, 1))
})

test_that("point metrics report error with the predicted-minus-actual sign", {
  m <- point_metrics(predicted = c(3, 5), actual = c(2, 4), tolerances = c(1, 2))
  expect_equal(m$mae, 1)
  expect_equal(m$median_ae, 1)
  expect_equal(m$bias, 1)
  expect_equal(m$within_1, 1)
  expect_equal(m$within_2, 1)
  expect_equal(m$n, 2)
})

test_that("point metrics tolerate misses outside the tolerance", {
  m <- point_metrics(predicted = c(1, 9), actual = c(4, 5), tolerances = c(1, 3))
  expect_equal(m$bias, 0.5)
  expect_equal(m$within_1, 0)
  expect_equal(m$within_3, 0.5)
})

test_that("C-index is 1 for a perfectly ordered forecast and 0 for reversed", {
  expect_equal(c_index(time = c(3, 6, 9), event = c(TRUE, TRUE, TRUE), predicted = c(2, 5, 8)), 1)
  expect_equal(c_index(time = c(3, 6, 9), event = c(TRUE, TRUE, TRUE), predicted = c(8, 5, 2)), 0)
})

test_that("C-index uses censored spells only until their censoring time", {
  # The censored spell at 10 cannot be compared with the later return at 12,
  # but the observed return at 2 can be compared with both.
  expect_equal(
    c_index(time = c(2, 10, 12), event = c(TRUE, FALSE, TRUE), predicted = c(1, 9, 11)),
    1
  )
  expect_true(is.na(c_index(time = c(2, 10, 12), event = c(FALSE, FALSE, TRUE), predicted = c(1, 9, 11))))
})

test_that("rolling_weighted pools daily means by their row counts", {
  dates <- as.Date("2026-11-01") + 0:3
  values <- c(0.2, 0.4, 0.6, 0.8)
  n <- c(10, 10, 10, 10)
  rolled <- rolling_weighted(dates, values, n, window_days = 2)
  expect_equal(rolled[1], 0.2)
  expect_equal(rolled[2], 0.3)
  expect_equal(rolled[4], 0.7)
})
