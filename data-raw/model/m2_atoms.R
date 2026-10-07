#' m2: Atoms — parish territory periods
#'
#' An atom is one parish unit's territory for a period: one source parish record (records of
#' one topo_id with the same polygon were already merged in m1).
#'
#' Input:  data-raw/model/out/m1.rds
#' Output: data-raw/model/out/m2.rds: list(atoms, tiling, coverage)

source("data-raw/build_helpers.R")
source("data-raw/model/model_helpers.R")
m1 <- readRDS("data-raw/model/out/m1.rds")
t0 <- timer_start("m2: atoms")

atoms <- m1$records %>% filter(type_id == "parish") %>%
  arrange(unit_id, start) %>%
  transmute(atom_id = sprintf("A%05d", row_number()), unit_id, topo_id, name, start, end,
            start_date = year_start(start), end_date = year_end(end), part = FALSE)
atoms$km2 <- as.numeric(st_area(atoms)) / 1e6
message(sprintf("  Atoms: %d for %d parish units", nrow(atoms), n_distinct(atoms$unit_id)))
message(sprintf("  Area: median %.1f km2, smallest %.4f km2 (%s)",
                median(atoms$km2), min(atoms$km2), atoms$name[which.min(atoms$km2)]))

# Atoms of one unit overlapping in time (source duplicates): report, keep
dup <- st_drop_geometry(atoms) %>% inner_join(st_drop_geometry(atoms), by = "unit_id",
                                              relationship = "many-to-many") %>%
  filter(atom_id.x < atom_id.y, start.x <= end.y, start.y <= end.x)
message(sprintf("  Units whose atoms overlap in time: %d", n_distinct(dup$unit_id)))

# Same-period overlaps between atoms of different units
ii <- st_intersects(atoms)
pr <- do.call(rbind, lapply(seq_along(ii), function(i){
  j <- ii[[i]][ii[[i]] > i]; if (length(j)) cbind(i, j) }))
pr <- pr[atoms$start[pr[, 1]] <= atoms$end[pr[, 2]] & atoms$start[pr[, 2]] <= atoms$end[pr[, 1]] &
           atoms$unit_id[pr[, 1]] != atoms$unit_id[pr[, 2]], , drop = FALSE]
message(sprintf("  Candidate pairs (touching, coexisting, different units): %d", nrow(pr)))
g <- st_geometry(atoms)
ov_km2 <- vapply(seq_len(nrow(pr)), function(k){
  x <- tryCatch(st_area(st_intersection(g[pr[k, 1]], g[pr[k, 2]])), error = function(e) 0)
  if (length(x)) as.numeric(sum(x)) / 1e6 else 0
}, numeric(1))
tiling <- tibble(a = atoms$atom_id[pr[, 1]], b = atoms$atom_id[pr[, 2]],
                 a_name = atoms$name[pr[, 1]], b_name = atoms$name[pr[, 2]],
                 from = pmax(atoms$start[pr[, 1]], atoms$start[pr[, 2]]),
                 to = pmin(atoms$end[pr[, 1]], atoms$end[pr[, 2]]), km2 = ov_km2) %>%
  filter(km2 > 1) %>% arrange(desc(km2))
message(sprintf("  Coexisting atom pairs overlapping > 1 km2: %d", nrow(tiling)))
if (nrow(tiling)) print(as.data.frame(head(tiling, 10)))

# Parish territory covered per sample year (the national outline is the union of all atoms)
coverage <- bind_rows(lapply(c(1650, 1700, 1750, 1800, 1850, 1880, 1900, 1930, 1950, 1970, 1990),
  function(y){
    a <- atoms[atoms$start <= y & atoms$end >= y, ]
    tibble(year = y, atoms = nrow(a), km2 = sum(a$km2))
  }))
print(as.data.frame(coverage))
timer_end(t0)

message("  --- m2 checks ---")
check(all(atoms$km2 > 0.05), sprintf("no empty atoms (smallest %.4f km2: %s)",
                                     min(atoms$km2), atoms$name[which.min(atoms$km2)]))
check(all(!st_is_empty(atoms)), "no empty geometries")
check(nrow(tiling) < 50, sprintf("fewer than 50 coexisting atom pairs overlap > 1 km2 (got %d)",
                                 nrow(tiling)))
saveRDS(list(atoms = atoms, tiling = tiling, coverage = coverage), "data-raw/model/out/m2.rds")
message("  Saved data-raw/model/out/m2.rds")
