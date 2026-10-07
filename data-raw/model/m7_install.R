## m7: Install the build into the package
##   Rscript data-raw/model/m7_install.R
## Copies the datasets from data-raw/model/out/pkg/data into data/ and writes R/sysdata.rda from
## this build's coverage table. Nothing else in the pipeline touches data/, so a build can be
## checked with SWEHIST_ROOT=data-raw/model/out/pkg before it replaces the installed data.
##
## The datasets a build must produce are listed below; the run stops if one is missing, so a
## half-finished build cannot be installed.

source("data-raw/build_helpers.R")
t0 <- timer_start("m7: install the build into data/")
pkg_data <- "data-raw/model/out/pkg/data"
stopifnot(dir.exists(pkg_data))

REQUIRED <- c("boundaries", "hierarchy", "relations", "sweden", "parish_meta", "parish_link",
              "parish_registry", "unit_variants", "hist_town", "quality_table", "known_issues",
              "events", "parish_codes")
have <- sub("\\.rda$", "", list.files(pkg_data, "\\.rda$"))
missing <- setdiff(REQUIRED, have)
if (length(missing))
  stop("the build has no ", paste(missing, collapse = ", "), call. = FALSE)

# The datasets 1.1.1 shipped are tibble-backed (class "sf" "tbl_df" "tbl" "data.frame" for the
# spatial ones), and code that expects a tibble sees the difference: unit_history() returned a
# plain data.frame from a build whose boundaries were not.
as_shipped <- function(x){
  if (inherits(x, "sf")) { class(x) <- c("sf", "tbl_df", "tbl", "data.frame"); x }
  else if (is.data.frame(x)) dplyr::as_tibble(x) else x
}
for (nm in REQUIRED) {
  e <- new.env()
  load(file.path(pkg_data, paste0(nm, ".rda")), envir = e)
  obj <- as_shipped(get(nm, envir = e))
  assign(nm, obj)
  # xz for the shipped data: it is what the package carries
  save(list = nm, file = file.path("data", paste0(nm, ".rda")), compress = "xz")
  message(sprintf("  %-18s %s", nm,
                  if (is.data.frame(obj)) sprintf("%d rows", nrow(obj)) else class(obj)[1]))
}

cov_f <- "data-raw/model/out/coverage.csv"
if (file.exists(cov_f)) {
  .coverage <- readr::read_csv(cov_f, show_col_types = FALSE)
  save(.coverage, file = "R/sysdata.rda", compress = "xz")
  message("  R/sysdata.rda: coverage for ", dplyr::n_distinct(.coverage$type_id), " types")
} else {
  warning("no out/coverage.csv: R/sysdata.rda keeps the previous build's coverage")
}
timer_end(t0)
message("  Installed. Run the tests against the package now.")
