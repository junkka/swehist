## Step 2: Set coordinate precision to close sub-meter gaps
## Input:  data-raw/intermediate/step1_valid.rda
## Output: data-raw/intermediate/step2_snapped.rda
##
## NOTE: st_snap(x, x, tolerance) is O(n^2) — unusable for 3000+ features.
##       st_set_precision(geom, 1) rounds vertices to 1m grid: O(n), same effect.

source("data-raw/build_helpers.R")
load("data-raw/intermediate/step1_valid.rda")

t0 <- timer_start("Step 2: Set precision (1m) + re-validate")

n_before <- nrow(sf_filtered)
message("  Features: ", n_before)

st_geometry(sf_filtered) <- st_set_precision(st_geometry(sf_filtered), 1)
st_geometry(sf_filtered) <- st_make_valid(st_geometry(sf_filtered))

# Remove any empty geometries created by precision rounding
empty_check <- st_is_empty(sf_filtered)
if (any(empty_check)) {
  message("  Removing ", sum(empty_check), " empty geometries after precision set")
  sf_filtered <- sf_filtered[!empty_check, ]
}

snapped <- sf_filtered
snapped$id <- seq_len(nrow(snapped))

save(snapped, raw_crs, file = "data-raw/intermediate/step2_snapped.rda")
timer_end(t0)

# ---- Validation ----
message("  --- Validations ---")
n_lost <- n_before - nrow(snapped)
check(n_lost < 50, sprintf("lost < 50 features during precision set (lost %d)", n_lost))
check(all(st_is_valid(snapped)), "all geometries valid")
check(!any(st_is_empty(snapped)), "no empty geometries")
check(identical(st_crs(snapped), raw_crs), "CRS preserved")
check(all(c("topo_id", "namn", "typ", "vtidstart", "vtidslut", "id") %in% names(snapped)),
      "all required columns present")
