## Shared helpers for build pipeline

library(sf)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)
library(tibble)

sf_use_s2(FALSE)

# ---------------------------------------------------------------------------
# Timing
# ---------------------------------------------------------------------------
timer_start <- function(label){
  message(sprintf("\n=== %s ===", label))
  proc.time()
}

timer_end <- function(t0){
  elapsed <- (proc.time() - t0)["elapsed"]
  message(sprintf("  [%.1f s]", elapsed))
  invisible(elapsed)
}

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------
check <- function(condition, msg){
  if (!condition) stop("VALIDATION FAILED: ", msg, call. = FALSE)
  message("  OK: ", msg)
}

check_equal <- function(a, b, msg){
  check(identical(a, b), sprintf("%s (got %s, expected %s)", msg, a, b))
}

# ---------------------------------------------------------------------------
# Connected component IDs (union-find; same as R/misc.R create_block)
# ---------------------------------------------------------------------------
create_block <- function(x, y){
  x <- as.character(x); y <- as.character(y)
  x[is.na(x)] <- paste0("\001na_x", which(is.na(x)))
  y[is.na(y)] <- paste0("\001na_y", which(is.na(y)))
  all_ids <- unique(c(x, y))
  parent <- seq_along(all_ids)
  find <- function(i){
    while (parent[i] != i) { parent[i] <<- parent[parent[i]]; i <- parent[i] }
    i
  }
  xi <- match(x, all_ids); yi <- match(y, all_ids)
  for (k in seq_along(xi)) {
    a <- find(xi[k]); b <- find(yi[k])
    if (a != b) parent[max(a, b)] <- min(a, b)
  }
  roots <- vapply(xi, find, integer(1))
  match(roots, unique(roots))
}

# ---------------------------------------------------------------------------
# Counting errors that a step deliberately tolerates (reported, not hidden)
# ---------------------------------------------------------------------------
.tolerated <- new.env()
tolerate <- function(key, expr, fallback){
  tryCatch(expr, error = function(e){
    .tolerated[[key]] <- c(.tolerated[[key]], conditionMessage(e))
    fallback
  })
}
report_tolerated <- function(max_allowed = Inf){
  keys <- ls(.tolerated)
  if (length(keys) == 0) { message("  Tolerated errors: none"); return(invisible(0L)) }
  n <- 0L
  for (k in keys) {
    msgs <- .tolerated[[k]]
    n <- n + length(msgs)
    message(sprintf("  Tolerated errors in %s: %d (first: %s)", k, length(msgs), msgs[1]))
  }
  check(n <= max_allowed, sprintf("tolerated errors <= %s (got %d)", max_allowed, n))
  invisible(n)
}

# ---------------------------------------------------------------------------
# Frozen geom_ids
# ---------------------------------------------------------------------------
# geom_id is the key users store, so it must survive rebuilds. The lookup
# (data-raw/geom_id_lookup.csv) records every id ever issued. A feature keeps
# an id when (type_id, topo_id, start, end) match exactly; otherwise it takes
# the id of the only unclaimed version of the same (type_id, topo_id). All
# remaining features get new ids above the highest id ever issued. Retired ids
# are never reused.
GEOM_ID_LOOKUP <- "data-raw/geom_id_lookup.csv"

read_geom_id_lookup <- function(){
  readr::read_csv(GEOM_ID_LOOKUP, col_types = "iccccii", na = "")
}

assign_frozen_ids <- function(meta, lookup = read_geom_id_lookup(),
                              taken = integer(0)){
  # meta: data frame with type_id, topo_id, start, end (one row per feature)
  n <- nrow(meta)
  id <- rep(NA_integer_, n)
  lk <- lookup[!lookup$geom_id %in% taken, ]
  # A unit with no Riksarkivet topo_id -- the municipalities that exist only in SCB's lists and the
  # courts that exist only in the statskalender -- had the key "municipality NA 1863 1951", which
  # three of them shared, so match() could not tell them apart and issued each a fresh id on every
  # build: six ids burned per build, and Larbro landskommun had collected seven of them. Where
  # there is no topo_id the ref_code identifies the unit, and failing that its name.
  id_of <- function(d) dplyr::coalesce(as.character(d$topo_id), d$ref_code, d$name)
  key_m <- paste(meta$type_id, id_of(meta), meta$start, meta$end)
  key_l <- paste(lk$type_id, id_of(lk), lk$start, lk$end)
  hit <- match(key_m, key_l)
  hit[duplicated(hit) & !is.na(hit)] <- NA
  id[!is.na(hit)] <- lk$geom_id[hit[!is.na(hit)]]
  used <- c(taken, id[!is.na(id)])

  # Same unit, changed period: unique unclaimed version of the same topo_id
  todo <- which(is.na(id))
  if (length(todo)) {
    lk2 <- lk[!lk$geom_id %in% used, ]
    tk_l <- paste(lk2$type_id, id_of(lk2))
    tk_m <- paste(meta$type_id, id_of(meta))
    for (i in todo) {
      cand <- which(tk_l == tk_m[i])
      same_m <- sum(tk_m[todo] == tk_m[i])
      if (length(cand) == 1 && same_m == 1 && !lk2$geom_id[cand] %in% used) {
        id[i] <- lk2$geom_id[cand]
        used <- c(used, id[i])
      }
    }
  }

  todo <- which(is.na(id))
  if (length(todo)) {
    next_id <- max(c(lookup$geom_id, taken, id), na.rm = TRUE) + 1L
    id[todo] <- seq(next_id, length.out = length(todo))
  }
  id
}

# Write the lookup back: ids in use get their current unit, name and period
# (a unit keeps its id when its period is corrected, and the next build must
# find it by exact match); new ids are added; retired ids are never removed.
update_geom_id_lookup <- function(meta_with_ids){
  lk <- read_geom_id_lookup()
  cols <- c("geom_id", "type_id", "topo_id", "name", "ref_code", "start", "end")
  cur <- tibble::as_tibble(meta_with_ids[, cols])
  cur$start <- as.integer(cur$start); cur$end <- as.integer(cur$end)
  n_new <- sum(!cur$geom_id %in% lk$geom_id)
  old_rows <- lk[lk$geom_id %in% cur$geom_id, ]
  m <- cur[match(old_rows$geom_id, cur$geom_id), ]
  n_upd <- sum(old_rows$start != m$start | old_rows$end != m$end | old_rows$name != m$name, na.rm = TRUE)
  out <- dplyr::bind_rows(lk[!lk$geom_id %in% cur$geom_id, ], cur) %>% dplyr::arrange(geom_id)
  readr::write_csv(out, GEOM_ID_LOOKUP, na = "")
  message(sprintf("  geom_id lookup: %d new ids, %d ids with an updated period or name, %d retired kept",
                  n_new, n_upd, sum(!lk$geom_id %in% cur$geom_id)))
  invisible(n_new)
}

# ---------------------------------------------------------------------------
# Hole filling that keeps enclaves
# ---------------------------------------------------------------------------
# Fill interior rings smaller than `threshold` m2, except holes that contain
# another feature of the same type that coexists in time (a town inside a
# rural unit, an enclave parish). Digitisation gaps are filled; real enclaves
# stay open.
fill_small_holes <- function(x, threshold, by_type = "type_id",
                             start = "start", end = "end"){
  g <- st_geometry(x)
  pts <- suppressWarnings(st_point_on_surface(g))
  n_filled <- 0L; n_kept <- 0L
  for (i in seq_along(g)) {
    geom <- g[[i]]
    if (!inherits(geom, "MULTIPOLYGON")) next
    has_holes <- any(vapply(unclass(geom), length, integer(1)) > 1)
    if (!has_holes) next
    others <- which(x[[by_type]] == x[[by_type]][i] & seq_along(g) != i &
                    x[[start]] <= x[[end]][i] & x[[end]] >= x[[start]][i])
    polys <- unclass(geom)
    for (k in seq_along(polys)) {
      rings <- polys[[k]]
      if (length(rings) < 2) next
      keep <- rep(TRUE, length(rings))
      for (r in 2:length(rings)) {
        hole <- st_polygon(list(rings[[r]]))
        if (as.numeric(st_area(hole)) >= threshold) next
        inside <- if (length(others)) {
          any(lengths(st_intersects(pts[others], st_sfc(hole, crs = st_crs(g)))) > 0)
        } else FALSE
        if (inside) { n_kept <- n_kept + 1L } else { keep[r] <- FALSE; n_filled <- n_filled + 1L }
      }
      polys[[k]] <- rings[keep]
    }
    g[[i]] <- st_multipolygon(polys)
  }
  st_geometry(x) <- g
  message(sprintf("  Holes < %.0f km2: %d filled, %d kept (contain another unit)",
                  threshold / 1e6, n_filled, n_kept))
  x
}

# ---------------------------------------------------------------------------
# Rebuild parent geometries from their children
# ---------------------------------------------------------------------------
# links: one row per (parent_key, child_geom_id) with eff_start/eff_end, the
# period in which the child belongs to the parent. For each parent the years
# are cut wherever the set of children changes; each stable period becomes
# one feature whose geometry is the union of the children.
# child_sf: sf with geom_id (the child geometries).
reconstruct_from_children <- function(links, child_sf, hole_threshold = 1e8){
  out <- list()
  for (pk in unique(links$parent_key)) {
    df <- links[links$parent_key == pk, ]
    s_start <- min(df$eff_start); s_end <- max(df$eff_end)
    br <- sort(unique(c(s_start, df$eff_start, df$eff_end + 1L)))
    br <- br[br >= s_start & br <= s_end + 1L]
    br <- sort(unique(c(br, s_end + 1L)))
    per <- tibble::tibble(yr_start = br[-length(br)], yr_end = br[-1] - 1L)
    per$key <- vapply(per$yr_start, function(y){
      a <- df$child_geom_id[df$eff_start <= y & df$eff_end >= y]
      paste(sort(unique(a)), collapse = ",")
    }, character(1))
    r <- rle(per$key)
    e <- cumsum(r$lengths); b <- c(1L, e[-length(e)] + 1L)
    blk <- tibble::tibble(start = per$yr_start[b], end = per$yr_end[e], key = r$values)
    blk <- blk[nchar(blk$key) > 0, ]
    if (nrow(blk) == 0) next
    geoms <- lapply(blk$key, function(k){
      ids <- as.integer(strsplit(k, ",")[[1]])
      sub <- child_sf[child_sf$geom_id %in% ids, ]
      u <- st_union(st_geometry(sub))
      u <- tryCatch(smoothr::fill_holes(u, threshold = hole_threshold), error = function(e) u)
      st_cast(u, "MULTIPOLYGON")[[1]]
    })
    blk$n_children <- lengths(strsplit(blk$key, ","))
    blk$parent_key <- pk
    out[[length(out) + 1]] <- st_sf(blk[, c("parent_key", "start", "end", "n_children", "key")],
                                    geometry = st_sfc(geoms, crs = st_crs(child_sf)))
  }
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}

# ---------------------------------------------------------------------------
# Convert sparse intersect list to tibble of pairs
# ---------------------------------------------------------------------------
intersect_to_tibble <- function(intersect_list){
  ego <- rep(seq_along(intersect_list), lengths(intersect_list))
  alter <- unlist(intersect_list)
  tibble(ego = ego, alter = alter)
}

# ---------------------------------------------------------------------------
# Administrative type constants
# ---------------------------------------------------------------------------
ALL_SWEDISH_TYPES <- c(
  "Kyrksocken", "Län", "Kommun / stad", "Pastorat", "Kontrakt",
  "Stift", "Härad / stad / skeppslag", "Domsaga / rådhusrätt",
  "Tingsrätt", "Hovrätt", "Fögderi / stad", "Militär indelning"
)

TYPE_MAP <- c(
  "Kyrksocken" = "Parish", "Län" = "County",
  "Kommun / stad" = "Municipality", "Pastorat" = "Pastorship",
  "Kontrakt" = "Contract", "Stift" = "Diocese",
  "Härad / stad / skeppslag" = "Hundred",
  "Domsaga / rådhusrätt" = "Magistrates Court",
  "Tingsrätt" = "District Court", "Hovrätt" = "Court of Appeal",
  "Fögderi / stad" = "Bailiwick",
  "Militär indelning" = "Regiment"
)

TYPE_ID_MAP <- c(
  "Kyrksocken" = "parish", "Län" = "county",
  "Kommun / stad" = "municipality", "Pastorat" = "pastorship",
  "Kontrakt" = "contract", "Stift" = "diocese",
  "Härad / stad / skeppslag" = "hundred",
  "Domsaga / rådhusrätt" = "magistrates_court",
  "Tingsrätt" = "district_court", "Hovrätt" = "court_of_appeal",
  "Fögderi / stad" = "bailiwick",
  "Militär indelning" = "regiment"
)

ALL_TYPE_IDS <- unname(TYPE_ID_MAP)

# The package's national outline (`sweden`): the union of all parish polygons,
# 1600-1990, with holes under 10 km2 filled. Those holes are gaps between
# parish polygons that every other type covers; larger holes are lakes that no
# parish includes (Vänern, Vättern, Hjälmaren, Storsjön, Siljan, Orsasjön
# 56 km2, lakes near Arjeplog).
parish_territory <- function(g){
  u <- sf::st_union(sf::st_geometry(g[g$type_id == "parish", ]))
  smoothr::fill_holes(u, threshold = 1e7)
}

## close_unclaimed_holes() was here: the rule is live and correct as the opt-in
## fill_unclaimed_holes() in R/get_boundaries.R. The build copy was dead code that still
## ran, corrupted the geometry type, and carried a docstring arguing it was safe while the
## reason it was reverted (it breaks containment BETWEEN types) lived 400 lines away in
## another file. Deleted rather than kept as a trap.

