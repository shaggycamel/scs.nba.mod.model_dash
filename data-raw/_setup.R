# Shared generation setup ---------------------------------------------------

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(here)
  library(purrr)
  library(glue)
  library(tibble)
  library(dplyr)
  library(stringr)
  library(tidyr)
})

source(here("R", "fct_metrics.R"))
source(here("R", "fct_db.R"))

db_con <- db_connect(Sys.getenv("NBA_DB_SECTION", "jetson-read"))

save_data <- function(name, value) {
  env <- new.env(parent = emptyenv())
  assign(name, value, envir = env)
  save(list = name, file = here("data", paste0(name, ".rda")), envir = env, compress = "bzip2")
}
