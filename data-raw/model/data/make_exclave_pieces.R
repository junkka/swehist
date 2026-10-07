#' make_exclave_pieces.R — extract the 80 missing exclaves as geometry (one-off
#'
#' 26 September 2026) Lantmäteriet's distrikt (in force 2016, drawn on the parishes of
#' 1999-12-31) have 128 detached parts of at least 0.25 km2.
#'
#' Usage:  Rscript data-raw/model/data/make_exclave_pieces.R (run from the package root)
#' Output: data-raw/model/data/exclave_pieces.gpkg (tracked; read by m2_atoms.R)
#' Input (not in the repository; the review notes accompanying the paper):
#' followup/geometry/comp_geoms.rds  — 1990 parish and distrikt geometry by component
#' followup/geometry/exclaves_distrikt_not_swehist.csv — the 80 cases

suppressPackageStartupMessages({library(sf); library(dplyr); library(readr)})
notes <- Sys.getenv("SWEHIST_NOTES", "../swehist-tests-2026-09-25")
geo <- file.path(notes, "followup", "geometry")
stopifnot(dir.exists(geo))
G <- readRDS(file.path(geo, "comp_geoms.rds"))
x <- read_csv(file.path(geo, "exclaves_distrikt_not_swehist.csv"), show_col_types = FALSE)

# The detached parts of each distrikt component, as 10_exclaves.R found them
parts <- function(d, idc){
  y <- d %>% select(all_of(idc)) %>% st_cast("MULTIPOLYGON") %>% st_cast("POLYGON", warn = FALSE)
  y$a <- as.numeric(st_area(y)) / 1e6
  y %>% group_by(across(all_of(idc))) %>% mutate(rank = rank(-a, ties.method = "first")) %>%
    ungroup() %>% filter(rank > 1, a >= 0.25)
}
dp <- parts(G$ds, "comp") %>% mutate(km2 = round(a, 2))
p <- dp %>% select(comp, km2) %>%           # sf on the left, so the geometry is kept
  inner_join(x %>% rename(km2 = a), by = c("comp", "km2"))
stopifnot(nrow(p) == nrow(x))

# Owner: the swehist parish of the same component. Host: taken from the detection run.
ps <- st_drop_geometry(G$ps) %>% select(comp, n_s, names, codes90)
p <- p %>% left_join(ps, by = "comp") %>%
  left_join(ps %>% transmute(host_comp = comp, host_names = names), by = "host_comp")
one <- function(s) vapply(strsplit(s, "; ", fixed = TRUE), `[`, character(1), 1)
p$owner_name <- one(p$names); p$owner_code90 <- one(gsub(";", ",", p$codes90))
p$ambiguous <- p$n_s > 1
amb <- which(p$ambiguous)
for (i in amb) {                      # several parishes in the component: the nearest one owns it
  cands <- G$ps[G$ps$comp == p$comp[i], ]
  nm <- strsplit(cands$names, "; ", fixed = TRUE)[[1]]
  cd <- strsplit(cands$codes90, ",", fixed = TRUE)[[1]]
  d <- st_distance(st_geometry(p)[i], st_cast(st_geometry(cands), "POLYGON", warn = FALSE))
  message(sprintf("  ambiguous owner: %s (%.2f km2) -> %s", p$distrikt[i], p$km2[i], nm[1]))
  p$owner_name[i] <- nm[1]; p$owner_code90[i] <- cd[1]
}
p$owner_code90 <- sub(",.*", "", p$owner_code90)

# A point inside the owner's own 1990 territory (its largest part), so the build can find the
# owner unit by geometry rather than by name: the exclave is added to whichever parish covers
# this point at 1990, and taken from whichever parishes cover the piece itself.
main_point <- function(cmp){
  g <- st_geometry(G$ps)[match(cmp, G$ps$comp)]
  pp <- suppressWarnings(st_cast(st_cast(g, "MULTIPOLYGON"), "POLYGON"))
  pp <- pp[which.max(as.numeric(st_area(pp)))]
  suppressWarnings(st_point_on_surface(pp))
}
op <- do.call(c, lapply(p$comp, main_point))
oc <- st_coordinates(op)

out <- p %>% transmute(piece_id = sprintf("X%03d", row_number()), distrikt, owner_name,
                       owner_code90, host_name = host_names, km2, ambiguous,
                       comp, host_comp, owner_x = oc[, 1], owner_y = oc[, 2])
message(sprintf("Pieces: %d, %.0f km2 (largest %s %.1f km2)", nrow(out), sum(out$km2),
                out$distrikt[which.max(out$km2)], max(out$km2)))
f <- "data-raw/model/data/exclave_pieces.gpkg"
if (file.exists(f)) file.remove(f)
st_write(out, f, quiet = TRUE)
write_csv(st_drop_geometry(out) %>% select(-owner_x, -owner_y),
          "data-raw/model/data/exclave_pieces.csv")
message("Written ", f, " and exclave_pieces.csv")
