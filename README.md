# `{scs.nba.mod.model_dash}`

Accuracy monitoring and per-game forecasts for the NBA predictive models. A
`{golem}` Shiny app, baked at build time from the published model views and
deployed to a Hugging Face Space, in the same style as the league dashboard.

The first registered model is the injury duration model
(`mod.injury_daily_forecast_vw`). The app is built around a small model
contract so the next model is a data adapter plus one registry row.

## What it reports

- **Overview** — a scorecard, one row per registered model: Brier (next game and
  7 days) for onset and nowcast, AUC, games MAE, C-index, resolved/censored.
- **Game day** — one row per player per unplayed fixture: the availability
  probability, whether it came from the injury report, the injury-duration
  curve or the hierarchy's state-only model, and the expected minutes and usage.
- **Player stats** — one row per player and fixture from the published player
  stat snapshot: P(play), minutes, expected usage and the 13 fitted stat means
  plus points and rebounds. Filter by date, team and player; sort by any stat.
  Values are conditional on playing, so multiply by P(play) for an ex-ante
  expected value.
- **Injuries** — a tabset:
  - **Injury log** — one row per injury spell with forecast date, team, player,
    ailment class, body part, date of injury, predicted return and the actual
    return once the spell resolves.
  - **Accuracy trend** — daily Brier or MAE with a 28-day weighted mean,
    filterable by scope (nowcast/onset), ailment class and body part.

Two scopes are reported throughout:

- **Onset** — the first forecast for each spell.
- **Nowcast** — every daily forecast.

## Layout

- `R/` — app, modules, metric functions and the model registry (`models.R`).
- `data-raw/` — bake scripts. One script per model, writing normalised frames,
  plus `02_game_day.R` for the day-of fixture table and `03_player_stats.R` for
  the player stat projections.
- `data/` — baked `.rda` files (git-ignored, produced at deploy time).
- `docker/`, `deploy/` — image build and HF deploy pipeline
  (`deploy/cron.sh` is the single entry point).
- `tests/testthat/` — metric maths, filter behaviour and the data contract.

## Run locally

```r
renv::restore()
devtools::load_all(".")
modeldash::run_app()
```

The app reads only baked data, so bake first:

```bash
NBA_DB_SECTION=jetson-read Rscript data-raw/_generate_all.R
```

Baking is read-only and uses `~/.config/scs_hub_credentials.ini`.

## Deploy

`deploy/cron.sh` bakes, builds `shaggycamel/scs.nba.mod.model_dash`, pushes it
and commits a digest-pinned Dockerfile to the `shaggycamel/model-dash` Space.
Always run `DRY_RUN=1` first, then `BUILD_ONLY=1` to verify the build without
touching Docker Hub or the Space.

Scheduled from the NUC after the daily model run:

```cron
15 23 * * * /home/oli/shaggy_camel_sports/nba/mod/model_dash/deploy/cron.sh daily \
  >> /home/oli/shaggy_camel_sports/nba/mod/model_dash/deploy/cron.log 2>&1
```

## Adding a model

1. Add a script under `data-raw/` that writes `<model>_unit`, `<model>_prob`,
   `<model>_point`, `<model>_coverage`, `<model>_censor` and `<model>_meta`
   (see `01_injury_forecast.R`; `99_combine.R` binds them into the `mdl_*`
   frames the app reads).
2. Add a row to `model_registry()` in `R/models.R`.
3. Add a page in `app_ui()` if the model needs a panel that is not generic.

Metric definitions stay in `R/fct_metrics.R`; do not re-implement them per
model.
