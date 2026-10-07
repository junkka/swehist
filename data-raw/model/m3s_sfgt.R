#' m3s: SFGT membership evidence, keyed by pid
#'
#' SFGT rows are keyed by `pid`, not by name: parish_name is NA in 78% of sfgt_pastorat and 62%
#' of sfgt_lan, so joining on the name (as a first draft of m3 did) loses most of the data. pid
#' -> parish unit comes from m1b's unit_codes.
#'
#' Input:  data-raw/model/out/m1.rds, out/m2.rds, out/m1b.rds (pid codes per parish unit)
#' data-raw/sfgt/sfgt_{pastorat,lan}.rda
#' Output: data-raw/model/out/m3s.rds: list(evidence, report)

source("data-raw/build_helpers.R")
source("data-raw/model/model_helpers.R")
m1 <- readRDS("data-raw/model/out/m1.rds")
m2 <- readRDS("data-raw/model/out/m2.rds")
if (!file.exists("data-raw/model/out/m1b.rds"))
  stop("m3s needs out/m1b.rds (pid codes); run m1b_codes.R first")
m1b <- readRDS("data-raw/model/out/m1b.rds")
if (!file.exists("data-raw/model/out/m3.rds"))
  stop("m3s needs out/m3.rds (containment, for the spatial check); run m3_evidence.R first")
m3 <- readRDS("data-raw/model/out/m3.rds")
t0 <- timer_start("m3s: SFGT evidence (by pid)")

# The share of a parish that lies inside a candidate parent, for the spatial check below
spatial_support <- function(ty){
  m3$containment %>% filter(parent_type == ty) %>%
    transmute(atom_id, parent_unit, cs = start, ce = end, share)
}
# Rank the candidates of one claim by how much of the parish lies in each, then by how long they
# coexist; drop a claim whose candidates hold none of it. `keyed` are the claim's key columns.
choose_spatially <- function(cand, sup, label){
  n0 <- n_distinct(paste(cand$atom_id, cand$sfgt_name, cand$s, cand$e))
  x <- cand %>%
    left_join(sup, by = c("atom_id", "parent_unit"), relationship = "many-to-many") %>%
    mutate(sp = ifelse(!is.na(share) & cs <= e & s <= ce, share, 0)) %>%
    group_by(atom_id, sfgt_name, s, e, parent_unit, ps, pe, unit_id) %>%
    summarise(sp = max(sp), .groups = "drop") %>%
    mutate(tov = pmin(e, coalesce(pe, e)) - pmax(s, coalesce(ps, s)))
  best <- x %>% group_by(atom_id, sfgt_name, s, e) %>%
    slice_max(order_by = sp + tov / 1e5, n = 1, with_ties = FALSE) %>% ungroup()
  drop <- best %>% filter(!is.na(parent_unit), sp <= 0)
  kept <- best %>% anti_join(drop, by = c("atom_id", "sfgt_name", "s", "e"))
  message(sprintf("  %s: %d claims, %d dropped with no spatial support (%.1f%%), %d named a unit the source lacks",
                  label, n0, nrow(drop), 100 * nrow(drop) / max(n0, 1),
                  sum(is.na(kept$parent_unit))))
  if (nrow(drop))
    print(as.data.frame(drop %>% count(sfgt_name, sort = TRUE) %>% head(6)), row.names = FALSE)
  kept
}

atoms <- m2$atoms
rec <- st_drop_geometry(m1$records)
pid_map <- m1b$unit_codes %>% filter(system == "pid") %>%
  transmute(pid = as.integer(code), unit_id,
            p_from = as.integer(format(start_date, "%Y")), p_to = as.integer(format(end_date, "%Y")))
message(sprintf("  pid -> unit: %d pids, %d parish units", n_distinct(pid_map$pid),
                n_distinct(pid_map$unit_id)))

# Atoms of the parish unit a pid points to, for the period the claim covers
atoms_of_pid <- function(claims){
  claims %>%
    inner_join(pid_map, by = "pid", relationship = "many-to-many") %>%
    inner_join(st_drop_geometry(atoms) %>% select(atom_id, unit_id, a_start = start, a_end = end),
               by = "unit_id", relationship = "many-to-many") %>%
    mutate(s = pmax(start, a_start, p_from), e = pmin(end, a_end, p_to)) %>%
    filter(s <= e)
}

yr <- function(x, default) ifelse(is.na(x), default, pmax(pmin(as.integer(x), 1990L), 1600L))

# ---- Pastorship ---------------------------------------------------------------------------
load("data-raw/sfgt/sfgt_pastorat.rda")
seat_of <- function(x) name_key_simple(sub("\\s+och\\s+.*$", "", sub("[,].*$", "", x)))
past_src <- rec %>% filter(type_id == "pastorship") %>%
  transmute(seat = name_key_simple(name), parent_unit = unit_id, ps = start, pe = end) %>%
  distinct()
past_claims <- sfgt_pastorat %>%
  transmute(pid = as.integer(pid), sfgt_name = pastorat_name, seat = seat_of(pastorat_name),
            start = yr(start_year, 1600L), end = yr(end_year, 1990L)) %>%
  filter(start <= end)
ev_past <- atoms_of_pid(past_claims) %>%
  left_join(past_src, by = "seat", relationship = "many-to-many") %>%
  mutate(ok = is.na(parent_unit) | (s <= pe & ps <= e)) %>% filter(ok) %>%
  choose_spatially(spatial_support("pastorship"), "pastorship") %>%
  transmute(atom_id, child_unit = unit_id, parent_unit, parent_type = "pastorship",
            parent_name = sfgt_name, start = s, end = e, source = "sfgt", dated = TRUE)
message(sprintf("  pastorship claims: %d for %d atoms (%.0f%% matched to a source pastorship)",
                nrow(ev_past), n_distinct(ev_past$atom_id), 100 * mean(!is.na(ev_past$parent_unit))))

# ---- County ------------------------------------------------------------------------------
load("data-raw/sfgt/sfgt_lan.rda")
county_src <- rec %>% filter(type_id == "county") %>%
  transmute(ckey = name_key_simple(name), parent_unit = unit_id, ps = start, pe = end) %>% distinct()
lan_claims <- sfgt_lan %>% filter(!partial) %>%
  transmute(pid = as.integer(pid), sfgt_name = county_name, ckey = name_key_simple(county_name),
            start = yr(start_year, 1600L), end = yr(end_year, 1990L)) %>%
  filter(start <= end)
# A county name is nearly unique, so only 6 of these claims had no spatial support, but the rule
# is the same: the candidate that holds the parish, and no claim guessed where none does.
ev_lan <- atoms_of_pid(lan_claims) %>%
  inner_join(county_src, by = "ckey", relationship = "many-to-many") %>%
  filter(s <= pe, ps <= e) %>%
  choose_spatially(spatial_support("county"), "county") %>%
  transmute(atom_id, child_unit = unit_id, parent_unit, parent_type = "county",
            parent_name = sfgt_name, start = s, end = e, source = "sfgt", dated = TRUE) %>%
  distinct()
message(sprintf("  county claims: %d for %d atoms", nrow(ev_lan), n_distinct(ev_lan$atom_id)))

# ---- Partial county transfers: parish parts, for m3b --------------------------------------
parts <- sfgt_lan %>% filter(partial) %>%
  transmute(pid = as.integer(pid), county_name, notes,
            start = yr(start_year, 1600L), end = yr(end_year, 1990L)) %>%
  inner_join(pid_map %>% select(pid, unit_id), by = "pid", relationship = "many-to-many")
message(sprintf("  partial county transfers (parish parts for m3b): %d", nrow(parts)))

evidence <- bind_rows(ev_past, ev_lan)
report <- list(pastorat_rows = nrow(sfgt_pastorat), lan_rows = nrow(sfgt_lan),
               pids_unmatched = setdiff(unique(c(sfgt_pastorat$pid, sfgt_lan$pid)), pid_map$pid))
message(sprintf("  SFGT pids with no parish unit: %d", length(report$pids_unmatched)))
timer_end(t0)

message("  --- m3s checks ---")
check(nrow(ev_past) > 6000, sprintf("pastorship claims > 6000 (got %d)", nrow(ev_past)))
check(all(evidence$start <= evidence$end), "every claim has start <= end")
saveRDS(list(evidence = evidence, parts = parts, report = report), "data-raw/model/out/m3s.rds")
message("  Saved data-raw/model/out/m3s.rds")
