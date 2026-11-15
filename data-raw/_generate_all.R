# Bake every registered model's published view into package data.

source(here::here("data-raw", "_setup.R"))

files <- sort(list.files(here::here("data-raw"), pattern = "^[0-9]{2}_", full.names = TRUE))
for (file in files) {
  source(file)
}

DBI::dbDisconnect(db_con)
