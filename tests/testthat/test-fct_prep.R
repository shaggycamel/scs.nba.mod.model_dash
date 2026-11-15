test_that("filter helpers do not shadow their column names", {
  d <- tibble::tibble(
    scope = c("onset", "nowcast", "nowcast"),
    horizon = c("7d", "7d", "14d"),
    version = c("2026-11-01", "2026-11-01", "2026-11-10"),
    as_of = as.Date("2026-11-01") + 0:2,
    value = 1:3
  )

  expect_equal(nrow(filter_scope(d, "onset")), 1)
  expect_equal(nrow(filter_scope(d, "both")), 3)
  expect_equal(nrow(filter_horizon(d, "7d")), 2)
  expect_equal(nrow(filter_version(d, "2026-11-10")), 1)
  expect_equal(nrow(filter_version(d, "all")), 3)
  expect_equal(nrow(filter_window(d, as.Date(c("2026-11-02", "2026-11-03")))), 2)
})

test_that("version choices include an all option", {
  choices <- version_choices(c("2026-11-01", "2026-11-10"))
  expect_equal(unname(choices), c("all", "2026-11-01", "2026-11-10"))
})
