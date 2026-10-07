#' m1: Units (identity) and their names
#'
#' A source record is one polygon of one topo_id for a period.
#'
#' Input:  data-raw/intermediate/step2_snapped.rda (source records, 1 m precision, EPSG:3021)
#' data-raw/model/corrections.csv (kinds drop_record, same_unit, separate_unit)
#' Output: data-raw/model/out/m1.rds: list(records, units, unit_names)

source("data-raw/build_helpers.R")
source("data-raw/model/model_helpers.R")
load("data-raw/intermediate/step2_snapped.rda")
t0 <- timer_start("m1: units and names")
corr <- read_corrections()

# ---- Records ----------------------------------------------------------------------------
rec <- snapped %>%
  transmute(topo_id, name = iconv(as.character(namn), "utf8", "utf8"), typ,
            type_id = unname(TYPE_ID_MAP[as.character(typ)]),
            start = as.integer(vtidstart), end = as.integer(vtidslut))
st_geometry(rec) <- polygons_only(st_geometry(snapped))       # GEOMETRYCOLLECTION -> polygons

drop <- corr %>% filter(kind == "drop_record")
rec <- rec[!paste(rec$topo_id, rec$start, rec$end) %in% paste(drop$topo_id, drop$start, drop$end), ]
message(sprintf("  Source records: %d (%d dropped as anachronisms)", nrow(rec), nrow(drop)))

# Records of one topo_id that follow each other (or overlap) with the same polygon are one
# record (the earlier pipeline's step 3a): symmetric difference < 1000 m2
rec <- merge_identical_versions(rec, tol_m2 = 1000)
rec$rec_id <- seq_len(nrow(rec))
message(sprintf("  Records after merging identical versions: %d", nrow(rec)))

# `period` corrections: the source dates a unit from its register entry rather than its
# existence. Uppsala, Linköping, Skara and Strängnäs are medieval but the source starts them at
# 1647/1615/1630, which left Norrland with no diocese before 1647. Stretch the unit's first and
# last record to the researched period. A correction may also NARROW a unit (S05/S06 move the
# Stockholm contracts to 1943); every record is clipped to the window and one wholly outside it is
# dropped and named, so a shrink can no longer strand territory in silence.
per <- corr %>% filter(kind == "period", !is.na(start) | !is.na(end))
if (nrow(per)) {
  n_adj <- 0L; miss <- character(); dropped <- character()
  for (i in seq_len(nrow(per))) {
    # a topo_id identifies the unit exactly; a bare name does not ("Domprosteriet" is a contract
    # in every diocese), so a name is only used when it is unique within the type
    k <- if (!is.na(per$topo_id[i])) which(rec$topo_id == per$topo_id[i]) else {
      kk <- which(rec$type_id == per$type_id[i] & rec$name == per$name[i])
      if (n_distinct(rec$topo_id[kk]) > 1) {
        miss <- c(miss, sprintf("%s ('%s' is not unique)", per$id[i], per$name[i])); integer(0)
      } else kk
    }
    if (!length(k)) { miss <- c(miss, per$id[i]); next }
    # Clip EVERY record of the unit to the window, not just the outermost one. Moving only the
    # first and last record left interior records outside it: S05 and S06 ask Domprosteriet and
    # Stockholms norra kontrakt to start in 1943, and because each has an interior 1906-1906
    # record the unit still started in 1906 and twelve Stockholm parishes were given a kontrakt
    # that did not exist for 1906-1942. The correction reported success either way.
    lo <- if (is.na(per$start[i])) -Inf else per$start[i]
    hi <- if (is.na(per$end[i]))    Inf else per$end[i]
    before <- rec[k, c("start", "end")]
    rec$start[k] <- pmax(rec$start[k], lo)
    rec$end[k]   <- pmin(rec$end[k],   hi)
    # the outermost surviving record still carries the researched date, so the unit's own period
    # is exactly the window the correction asked for
    alive <- k[rec$start[k] <= rec$end[k]]
    if (length(alive)) {
      if (!is.na(per$start[i])) rec$start[alive[which.min(rec$start[alive])]] <- per$start[i]
      if (!is.na(per$end[i]))   rec$end[alive[which.max(rec$end[alive])]]     <- per$end[i]
    }
    n_adj <- n_adj + sum(rec$start[k] != before$start | rec$end[k] != before$end)
    k2 <- k[rec$start[k] > rec$end[k]]          # a record wholly outside the window
    if (length(k2)) {
      dropped <- c(dropped, sprintf("%s: %s %d-%d", per$id[i], rec$name[k2[1]],
                                    before$start[match(k2[1], k)], before$end[match(k2[1], k)]))
      rec <- rec[-k2, ]
    }
  }
  message(sprintf("  period corrections: %d records clipped, %d rows not applied%s", n_adj,
                  length(unique(miss)),
                  if (length(miss)) paste0(" (", paste(unique(miss), collapse = ", "), ")") else ""))
  if (length(dropped))
    message(sprintf("    records dropped as wholly outside their window: %d (%s)",
                    length(dropped), paste(dropped, collapse = "; ")))
  # the correction must have taken: a unit's records must now span exactly the window asked for
  for (i in seq_len(nrow(per))) {
    k <- if (!is.na(per$topo_id[i])) which(rec$topo_id == per$topo_id[i]) else integer(0)
    if (!length(k)) next
    if (!is.na(per$start[i]) && min(rec$start[k]) != per$start[i])
      stop(sprintf("period correction %s did not take: %s starts %d, asked %d", per$id[i],
                   per$name[i], min(rec$start[k]), per$start[i]), call. = FALSE)
    if (!is.na(per$end[i]) && max(rec$end[k]) != per$end[i])
      stop(sprintf("period correction %s did not take: %s ends %d, asked %d", per$id[i],
                   per$name[i], max(rec$end[k]), per$end[i]), call. = FALSE)
  }
}

# ---- Identity ---------------------------------------------------------------------------
# Renames: topo_id A's last record ends in y, topo_id B's first record starts in y + 1,
# same type, same territory
first_last <- st_drop_geometry(rec) %>% group_by(topo_id) %>%
  summarise(first_rec = rec_id[which.min(start)], last_rec = rec_id[which.max(end)],
            t_start = min(start), t_end = max(end), .groups = "drop")
ends   <- rec[rec$rec_id %in% first_last$last_rec, ]
starts <- rec[rec$rec_id %in% first_last$first_rec, ]
cand <- st_drop_geometry(ends) %>% select(a = rec_id, a_topo = topo_id, type_id, y = end) %>%
  inner_join(st_drop_geometry(starts) %>% transmute(b = rec_id, b_topo = topo_id, type_id, y = start - 1L),
             by = c("type_id", "y")) %>%
  filter(a_topo != b_topo)
sep <- corr$topo_id[corr$kind == "separate_unit"]
cand <- cand %>% filter(!a_topo %in% sep, !b_topo %in% sep)
cand$iou <- pair_iou(rec, cand$a, cand$b)
renames <- cand %>% filter(iou >= 0.95)
message(sprintf("  Renames found (same type and territory, next year): %d", nrow(renames)))

same <- corr %>% filter(kind == "same_unit") %>% select(a_topo = topo_id, b_topo = value)
links <- bind_rows(renames %>% select(a_topo, b_topo), same)
topos <- unique(rec$topo_id)
grp <- create_block(c(topos, links$a_topo), c(topos, links$b_topo))[seq_along(topos)]
rec$unit_n <- grp[match(rec$topo_id, topos)]

# Stable unit ids: type prefix + rank by first start, then name, within the type
u <- st_drop_geometry(rec) %>% group_by(unit_n, type_id) %>%
  summarise(start = min(start), end = max(end), first_name = name[which.min(start)],
            name = name[which.max(end)], topo_ids = paste(sort(unique(topo_id)), collapse = ";"),
            n_records = n(), .groups = "drop") %>%
  arrange(type_id, start, first_name) %>%
  group_by(type_id) %>% mutate(unit_id = sprintf("%s:%05d", TYPE_PREFIX[type_id], row_number())) %>%
  ungroup()
rec$unit_id <- u$unit_id[match(rec$unit_n, u$unit_n)]
units <- u %>% transmute(unit_id, type_id, name, start_date = year_start(start),
                         end_date = year_end(end), topo_ids, n_records)

# ---- Names -------------------------------------------------------------------------------
# Official names from the source records (one per run of records with the same name), and
# the names of merged topo_ids as aliases for their period
unit_names <- st_drop_geometry(rec) %>% arrange(unit_id, start) %>%
  group_by(unit_id) %>% mutate(run = cumsum(name != lag(name, default = ""))) %>%
  group_by(unit_id, run, name) %>%
  summarise(start = min(start), end = max(end), .groups = "drop") %>%
  transmute(unit_id, name, start_date = year_start(start), end_date = year_end(end),
            precision = "year", kind = "official", source = "Riksarkivet topografi")
sm <- corr %>% filter(kind == "same_unit") %>%
  transmute(unit_id = rec$unit_id[match(topo_id, rec$topo_id)], alias_topo = value)
alias_names <- st_drop_geometry(rec) %>% filter(topo_id %in% sm$alias_topo) %>%
  group_by(unit_id, name) %>% summarise(start = min(start), end = max(end), .groups = "drop") %>%
  transmute(unit_id, name, start_date = year_start(start), end_date = year_end(end),
            precision = "year", kind = "alias", source = "Riksarkivet topografi (second record)")
unit_names <- bind_rows(unit_names %>% anti_join(alias_names, by = c("unit_id", "name")), alias_names)

# ---- Units the source does not have at all -------------------------------------------------
# `identity` rows with value "no_source_unit" name a unit that exists in the sources but has no
# Riksarkivet record (Djursholms stad, Noraskog, Folkare härad, Stockholms stads konsistorium).
# It gets a unit id and a name here and its geometry in m4, as the union of the parishes that
# `membership` corrections give it - exactly how every higher unit is built.
# The membership rows refer to such a unit by the key in its identity row's `topo_id`
# ("CORR:<slug>"), so that key is kept in `topo_ids` and resolves like any other.
syn <- corr %>% filter(kind == "identity", value == "no_source_unit", !is.na(name),
                       type_id != "parish") %>%
  transmute(type_id, name, corr_key = topo_id,
            start = coalesce(start, 1600L), end = coalesce(end, 1990L)) %>%
  distinct(type_id, name, .keep_all = TRUE)
if (nrow(syn)) {
  syn <- syn %>% group_by(type_id) %>%
    mutate(unit_id = sprintf("%s:X%04d", TYPE_PREFIX[type_id], row_number())) %>% ungroup()
  units <- bind_rows(units, syn %>% transmute(unit_id, type_id, name,
                                              start_date = year_start(start),
                                              end_date = year_end(end),
                                              topo_ids = corr_key, n_records = 0L))
  unit_names <- bind_rows(unit_names, syn %>%
    transmute(unit_id, name, start_date = year_start(start), end_date = year_end(end),
              precision = "year", kind = "official", source = "corrections (no source record)"))
  message(sprintf("  units created from corrections (no source record): %d", nrow(syn)))
  print(as.data.frame(syn %>% count(type_id)), row.names = FALSE)
}

# `name` rows keyed by a source topo_id: dated official names the source does not carry
# (Mariestads superintendentia 1600-1646 -> Karlstads superintendentia 1647-1771 -> Karlstads
# stift 1772-), so a version is named for the period it covers.
cn <- corr %>% filter(kind == "name", !is.na(name), !is.na(topo_id), !startsWith(topo_id, "SCB:"))
if (nrow(cn)) {
  t2u <- st_drop_geometry(rec) %>% distinct(topo_id, unit_id) %>%
    bind_rows(if (exists("syn")) syn %>% transmute(topo_id = corr_key, unit_id) else NULL)
  cnu <- cn %>% inner_join(t2u, by = "topo_id")
  if (nrow(cnu)) {
    unit_names <- bind_rows(unit_names,
      cnu %>% transmute(unit_id, name, start_date = year_start(coalesce(start, 1600L)),
                        end_date = year_end(coalesce(end, 1990L)), precision = "year",
                        kind = "official", source = "corrections"))
    message(sprintf("  dated names from corrections: %d rows on %d units", nrow(cnu),
                    n_distinct(cnu$unit_id)))
  }
}

records <- rec %>% select(rec_id, unit_id, topo_id, type_id, typ, name, start, end)
timer_end(t0)

# ---- Checks ------------------------------------------------------------------------------
message("  --- m1 checks ---")
check(!anyNA(records$unit_id), "every record has a unit")
check(!anyNA(units$name), "every unit has a name")
check(!anyNA(records$type_id), "every record has a known type")
check(all(!st_is_empty(records)), "no empty record geometry")
check(all(as.character(st_geometry_type(records)) == "MULTIPOLYGON"), "all records MULTIPOLYGON")
ov <- st_drop_geometry(records) %>% inner_join(st_drop_geometry(records), by = "unit_id",
                                               relationship = "many-to-many") %>%
  filter(rec_id.x < rec_id.y, start.x <= end.y, start.y <= end.x)
message(sprintf("  Units with overlapping records (alias pairs or source duplicates): %d",
                n_distinct(ov$unit_id)))
for (tid in sort(unique(units$type_id)))
  message(sprintf("  %-18s %5d units from %5d records", tid, sum(units$type_id == tid),
                  sum(records$type_id == tid)))
saveRDS(list(records = records, units = units, unit_names = unit_names, renames = renames),
        "data-raw/model/out/m1.rds")
message("  Saved data-raw/model/out/m1.rds")
