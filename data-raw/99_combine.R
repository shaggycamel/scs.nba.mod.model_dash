# Combine per-model bakes ---------------------------------------------------
# Every model script writes <model>_unit, <model>_prob, <model>_point,
# <model>_coverage, <model>_censor and <model>_meta. This script binds them into
# the frames the app reads, so a new model only has to write those files.

model_frames <- function(suffix) {
  pattern <- paste0("_", suffix, "\\.rda$")
  files <- list.files(here("data"), pattern = pattern, full.names = TRUE)
  files <- files[!grepl("^mdl_", basename(files))]
  if (length(files) == 0) {
    return(tibble::tibble())
  }
  purrr::map_dfr(files, function(file) {
    env <- new.env(parent = emptyenv())
    load(file, envir = env)
    tibble::as_tibble(get(ls(env)[1], envir = env))
  })
}

save_data("mdl_unit", model_frames("unit"))
save_data("mdl_prob", model_frames("prob"))
save_data("mdl_point", model_frames("point"))
save_data("mdl_coverage", model_frames("coverage"))
save_data("mdl_censor", model_frames("censor"))
save_data("mdl_meta", model_frames("meta"))
