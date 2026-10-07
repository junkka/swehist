#' build_data.R — Pipeline runner
#'
#' (data-raw/intermediate/, data-raw/model/out/) and never the installed package data, so a
#' step can be re-run on its own once its inputs exist. Run from the package root. Each step
#' reads only the previous steps' files

dir.create("data-raw/intermediate", showWarnings = FALSE)
dir.create("data-raw/model/out", showWarnings = FALSE, recursive = TRUE)

t_total <- proc.time()

steps <- c("step1_extract", "step2_snap")
model <- c("m1_units", "m2_atoms", "m1b_codes", "m3_evidence", "m3s_sfgt",
           "m3a_scb_municipalities", "m3c_courts", "m3d_deaneries", "m3b_resolve", "m4_derive",
           "m4b_matching", "m5_events", "m6_quality")

# Every step runs in its own R process. They share names for their working objects (`rec`,
# `atoms`, `m1`), and a step must never see a variable another step left behind: that is how a
# step silently uses stale data. Rscript also keeps the memory of the geometry steps separate.
run <- function(path){
  message("\n########## ", basename(path))
  st <- system2("Rscript", shQuote(path))
  if (st != 0) stop(basename(path), " failed (status ", st, ")", call. = FALSE)
}
for (step in steps) run(file.path("data-raw", paste0(step, ".R")))
for (step in model) run(file.path("data-raw", "model", paste0(step, ".R")))

elapsed <- (proc.time() - t_total)["elapsed"]
message(sprintf("\nTotal pipeline: %.0f s (%.1f min)", elapsed, elapsed / 60))
message("Install it with: Rscript data-raw/model/m7_install.R")
