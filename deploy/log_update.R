# Write one best-effort util.update_log row for a deploy stage.
# Run inside the base image with credentials mounted:
#   Rscript deploy/log_update.R
# Environment: LOG_TASK, LOG_SUCCESS ("1"/"0"), LOG_ERROR, LOG_KICKOFF,
# LOG_SOURCE, LOG_CADENCE, NBA_UPDATE_LOG_SECTION.

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
})

source("R/fct_db.R")

section <- Sys.getenv("NBA_UPDATE_LOG_SECTION", "jetson-write")
kickoff <- Sys.getenv("LOG_KICKOFF", "")
job_kickoff <- if (nzchar(kickoff)) {
  as.POSIXct(strptime(kickoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
} else {
  Sys.time()
}
row <- data.frame(
  source = Sys.getenv("LOG_SOURCE", "scs.nba.mod.model_dash"),
  job_kickoff = job_kickoff,
  cadence = Sys.getenv("LOG_CADENCE", "all"),
  task = Sys.getenv("LOG_TASK", "dashboard:model_dash"),
  task_time = as.POSIXct(format(Sys.time(), tz = "UTC"), tz = "UTC"),
  success = identical(Sys.getenv("LOG_SUCCESS"), "1"),
  error_message = if (nzchar(Sys.getenv("LOG_ERROR"))) Sys.getenv("LOG_ERROR") else NA_character_,
  row_count = NA_integer_
)

tryCatch(
  {
    con <- db_connect(section)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbWriteTable(con, DBI::Id(schema = "util", table = "update_log"), row, append = TRUE)
  },
  error = function(e) {
    warning("could not write util.update_log row: ", conditionMessage(e), call. = FALSE)
  }
)
