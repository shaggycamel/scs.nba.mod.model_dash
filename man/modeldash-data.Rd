\name{mdl_prob}
\alias{mdl_prob}
\alias{mdl_point}
\alias{mdl_unit}
\alias{mdl_coverage}
\alias{mdl_censor}
\alias{mdl_meta}
\alias{injury_prob}
\alias{injury_point}
\alias{injury_unit}
\alias{injury_coverage}
\alias{injury_censor}
\alias{injury_meta}
\alias{game_day}
\docType{data}
\title{Baked model accuracy frames}
\description{
Normalised frames produced by \code{data-raw/} at build time. Each model writes
\code{<model>_*} objects; \code{data-raw/99_combine.R} binds them into the
\code{mdl_*} frames the application reads. The app never connects to a database.
}
\usage{
mdl_prob
mdl_point
mdl_unit
mdl_coverage
mdl_censor
mdl_meta
injury_prob
injury_point
injury_unit
injury_coverage
injury_censor
injury_meta
game_day
}
\format{
\code{mdl_prob} is one row per model, unit, forecast date, horizon and scope
with columns \code{model_id}, \code{unit_id}, \code{as_of}, \code{horizon},
\code{p}, \code{y}, \code{scoreable} and slice columns.

\code{mdl_point} is one row per model, unit, forecast date, measure and scope
with \code{predicted}, \code{actual} and \code{scoreable}.

\code{mdl_unit} is one row per model unit (an injury spell for the injury
model), holding the dimensions and outcome fields.

\code{mdl_coverage} is one row per model and forecast date.

\code{mdl_censor} counts censoring reasons per model.

\code{mdl_meta} is one row per model with bake metadata and data-quality
figures.

The \code{injury_*} objects are the same frames before the combine step.

\code{game_day} is one row per player and unplayed fixture from
\code{mod.game_day_vw}, holding availability, its source and expected usage.
}
\keyword{datasets}
