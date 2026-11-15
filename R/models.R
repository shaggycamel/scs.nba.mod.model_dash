# Registered models ---------------------------------------------------------
# Adding a model means an adapter in data-raw/ plus a row here. The overview
# reads this registry; the app only renders pages for models it finds.

model_registry <- function() {
  tibble::tibble(
    model_id = "injury_duration",
    label = "Injury Duration",
    description = "Return-to-play forecasts for injured players",
    family = "survival",
    source = "mod.injury_daily_forecast_vw"
  )
}

model_row <- function(model_id) {
  dplyr::filter(model_registry(), .data$model_id == model_id)
}
