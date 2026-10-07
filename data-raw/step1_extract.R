## Step 1: Extract raw data, filter to all 11 admin types, make valid
## Output: data-raw/intermediate/step1_valid.rda

source("data-raw/build_helpers.R")
t0 <- timer_start("Step 1: Extract and filter raw data")

tmpdir <- tempdir()
untar("data-raw/data/histmaps_raw.tar.gz", exdir = tmpdir)

sf_raw <- st_read(dsn = file.path(tmpdir, "histmaps_raw"), layer = "histmaps_raw",
                  quiet = TRUE)
message("  Raw features: ", nrow(sf_raw))

# The shapefile's .prj describes RT90 2.5 gon V (Bessel, TM lon_0 15.808277,
# k 1, x_0 1500000) but without a datum, so PROJ would transform it with no
# datum shift (~200 m off). Label it EPSG:3021, which carries the RT90 datum.
# Coordinates are unchanged; m4_derive.R transforms to SWEREF99 TM (EPSG:3006).
prj <- st_crs(sf_raw)$proj4string
stopifnot(grepl("+proj=tmerc", prj, fixed = TRUE), grepl("+lon_0=15.808277", prj, fixed = TRUE),
          grepl("+x_0=1500000", prj, fixed = TRUE), grepl("+ellps=bessel", prj, fixed = TRUE))
sf_raw <- suppressWarnings(st_set_crs(st_set_crs(sf_raw, NA), 3021))
raw_crs <- st_crs(sf_raw)

sf_filtered <- sf_raw %>%
  filter(typ %in% ALL_SWEDISH_TYPES) %>%
  mutate(
    vtidstart = ifelse(vtidstart < 1600, 1600L, as.integer(vtidstart)),
    vtidslut  = ifelse(vtidslut == 9999, 1990L, as.integer(vtidslut))
  ) %>%
  filter(vtidslut >= vtidstart)
message("  After filter: ", nrow(sf_filtered))

# Fix degenerate geometries with 2-point LinearRings
# (GEOS can't process these at all — remove degenerate rings at coordinate level)
fix_degenerate_rings <- function(g){
  coords_list <- g[[1]]
  fixed_polys <- list()
  for (poly in coords_list) {
    fixed_rings <- list()
    for (ring in poly) {
      if (nrow(ring) >= 4) fixed_rings[[length(fixed_rings) + 1]] <- ring
    }
    if (length(fixed_rings) >= 1) fixed_polys[[length(fixed_polys) + 1]] <- fixed_rings
  }
  if (length(fixed_polys) == 0) return(NULL)
  st_multipolygon(fixed_polys)
}

# Make geometries valid per-geometry
message("  Making geometries valid...")
geom <- st_geometry(sf_filtered)
n_geos <- 0L; n_ring_fix <- 0L

for (i in seq_along(geom)) {
  v <- tryCatch(st_is_valid(geom[i]), error = function(e) FALSE)
  if (isTRUE(v)) next

  # Try GEOS st_make_valid
  ok <- tryCatch({
    g <- st_make_valid(geom[i])
    if (!st_is_empty(g)) { geom[i] <- g; n_geos <- n_geos + 1L; TRUE }
    else FALSE
  }, error = function(e) FALSE)
  if (ok) next

  # Fix degenerate rings then retry GEOS
  g_fixed <- fix_degenerate_rings(geom[i])
  if (!is.null(g_fixed)) {
    sfc_fixed <- st_sfc(g_fixed, crs = st_crs(geom))
    ok2 <- tryCatch({
      gv <- st_make_valid(sfc_fixed)
      if (!st_is_empty(gv)) { geom[i] <- gv; n_ring_fix <- n_ring_fix + 1L; TRUE }
      else FALSE
    }, error = function(e) FALSE)
  }
}

st_geometry(sf_filtered) <- geom
message(sprintf("  Fixed: %d with GEOS, %d with ring fix + GEOS", n_geos, n_ring_fix))

# Remove any remaining empty geometries
empty_check <- st_is_empty(sf_filtered)
if (any(empty_check)) {
  message("  Removing ", sum(empty_check), " empty geometries")
  sf_filtered <- sf_filtered[!empty_check, ]
}

save(sf_filtered, raw_crs, file = "data-raw/intermediate/step1_valid.rda")
timer_end(t0)

# ---- Validation ----
message("  --- Validations ---")
check(nrow(sf_filtered) > 10000, "feature count > 10000")
check(all(sf_filtered$typ %in% ALL_SWEDISH_TYPES), "only known admin types")
check(min(sf_filtered$vtidstart) >= 1600, "min start year >= 1600")
check(max(sf_filtered$vtidslut) <= 1990, "max end year <= 1990")
check(!any(st_is_empty(sf_filtered)), "no empty geometries")
check(all(st_is_valid(sf_filtered)), "all geometries valid")

for (typ in ALL_SWEDISH_TYPES) {
  n <- sum(sf_filtered$typ == typ)
  message(sprintf("  %s: %d", typ, n))
}
