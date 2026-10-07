#' m5: Events — what happened to a unit and when
#'
#' `relations` says only that territory passed from one polygon to another.
#'
#' Input:  data-raw/model/out/{m1,m1b,m2,m3b,m4}.rds, out/m3a.rds (SCB changes),
#' data-raw/sfgt/sfgt_indelning.rda
#' Output: data-raw/model/out/m5.rds: list(events, report)

source("data-raw/build_helpers.R")
source("data-raw/model/model_helpers.R")
m1 <- readRDS("data-raw/model/out/m1.rds"); m1b <- readRDS("data-raw/model/out/m1b.rds")
m2 <- readRDS("data-raw/model/out/m2.rds"); m3b <- readRDS("data-raw/model/out/m3b.rds")
m3a <- if (file.exists("data-raw/model/out/m3a.rds")) readRDS("data-raw/model/out/m3a.rds") else NULL
t0 <- timer_start("m5: events")
units <- m1$units
yr <- function(d) as.integer(format(as.Date(d), "%Y"))
ev <- function(...) tibble(...)

# ---- Created and abolished ------------------------------------------------------------------
life <- ev(unit_id = units$unit_id, type_id = units$type_id,
           start = yr(units$start_date), end = yr(units$end_date))
created <- life %>% filter(start > 1600) %>%
  transmute(date = year_start(start), unit_id, type_id, event = "created",
            other_unit = NA_character_, detail = NA_character_, source = "unit period")
abolished <- life %>% filter(end < 1990) %>%
  transmute(date = year_end(end), unit_id, type_id, event = "abolished",
            other_unit = NA_character_, detail = NA_character_, source = "unit period")

# ---- Renamed --------------------------------------------------------------------------------
# Consecutive official names of one unit
nm <- bind_rows(
  m1$unit_names %>% filter(kind == "official") %>%
    transmute(unit_id, name, start = yr(start_date), end = yr(end_date), source = "source"),
  m1b$unit_names_extra %>% filter(kind == "official") %>%
    transmute(unit_id, name, start = yr(start_date), end = yr(end_date), source = "research")) %>%
  arrange(unit_id, start) %>% group_by(unit_id) %>%
  mutate(prev = lag(name), prev_end = lag(end)) %>% ungroup()
renamed <- nm %>% filter(!is.na(prev), prev != name, start == prev_end + 1L) %>%
  left_join(units %>% select(unit_id, type_id), by = "unit_id") %>%
  transmute(date = year_start(start), unit_id, type_id, event = "renamed",
            other_unit = NA_character_, detail = paste0(prev, " -> ", name), source = source)

# ---- Recoded --------------------------------------------------------------------------------
# only the unit's own codes: a code added for lookup (a kyrkobokföringsdistrikt's SCB code, a code
# carried from the earlier build's registry, a correction, a chapel's DDB code) is not a recoding
cd <- m1b$unit_codes %>% filter(system %in% c("forkod", "dedik", "nadkod"),
                                !grepl("kyrkobokföringsdistrikt\\)|carried by pid|^correction:|matched by name within",
                                       source)) %>%
  transmute(unit_id, system, code, start = yr(start_date), end = yr(end_date)) %>%
  filter(!is.na(start)) %>% arrange(unit_id, system, start) %>%
  group_by(unit_id, system) %>% mutate(prev = lag(code), prev_end = lag(end)) %>% ungroup()
recoded <- cd %>% filter(!is.na(prev), prev != code, start == prev_end + 1L) %>%
  left_join(units %>% select(unit_id, type_id), by = "unit_id") %>%
  transmute(date = year_start(start), unit_id, type_id, event = "recoded",
            other_unit = NA_character_,
            detail = paste0(system, ": ", prev, " -> ", code), source = "codes")

# ---- Parent changed --------------------------------------------------------------------------
pc <- m3b$membership %>% arrange(child_unit, parent_type, start) %>%
  group_by(child_unit, parent_type) %>%
  mutate(prev = lag(parent_unit), prev_end = lag(end)) %>% ungroup() %>%
  filter(!is.na(prev), prev != parent_unit, start == prev_end + 1L)
parent_changed <- pc %>%
  transmute(date = year_start(start), unit_id = child_unit, type_id = "parish",
            event = "parent_changed", other_unit = parent_unit,
            detail = paste0(parent_type, ": ", unname(setNames(units$name, units$unit_id)[prev]),
                            " -> ", unname(setNames(units$name, units$unit_id)[parent_unit])),
            source = source)

# ---- Merged, split, part transferred ----------------------------------------------------------
# SFGT indelning is the dated source for parishes; SCB's change list for 1970-90.
load("data-raw/sfgt/sfgt_indelning.rda")
pid_map <- m1b$unit_codes %>% filter(system == "pid") %>%
  transmute(pid = as.integer(code), unit_id)
sfgt_ev <- sfgt_indelning %>%
  transmute(pid = as.integer(pid), year = as.integer(year), event_type, other_parish, notes) %>%
  filter(!is.na(year), year >= 1600, year <= 1990) %>%
  inner_join(pid_map, by = "pid") %>%
  mutate(event = case_when(event_type %in% c("merged_into", "incorporated") ~ "merged_into",
                           event_type %in% c("split_from", "split_off", "formed") ~ "split_from",
                           event_type == "transferred" ~ "part_transferred",
                           event_type == "renamed" ~ "renamed",
                           TRUE ~ event_type)) %>%
  transmute(date = year_start(year), unit_id, type_id = "parish", event,
            other_unit = NA_character_, detail = coalesce(other_parish, notes), source = "sfgt")
scb_ev <- if (!is.null(m3a) && !is.null(m3a$report$events)) {
  m3a$report$events %>% transmute(date = as.Date(date), unit_id = NA_character_,
                                  type_id = "municipality", event = "merged_into",
                                  other_unit = NA_character_, detail = detail, source = "scb")
} else NULL

# Parish parts: a documented transfer of part of a parish (the sources name farms, not lines)
parts_ev <- m3b$parts %>%
  transmute(date = year_start(start), unit_id, type_id = "parish", event = "part_transferred",
            other_unit = NA_character_,
            detail = paste0(parent_type, ": ", coalesce(parent_name, ""), " ",
                            substr(coalesce(note, ""), 1, 120)), source = source)

events <- bind_rows(created, abolished, renamed, recoded, parent_changed, sfgt_ev, scb_ev,
                    parts_ev) %>%
  filter(!is.na(date)) %>% arrange(date, unit_id, event) %>% distinct()
print(events %>% count(event, source) %>% arrange(event, desc(n)) %>% as.data.frame(), row.names = FALSE)
# Drop events for units that never reach the shipped data. 17 units acquired no member parish, so
# they have no polygon and no hierarchy row, and their 32 created/abolished events were a reference
# a user could not follow: events joins to boundaries by unit_id. The units stay in the model's own
# tables; only the dangling events go.
shipped <- unique(readRDS("data-raw/model/out/m4.rds")$boundaries$unit_id)
orphan <- !is.na(events$unit_id) & !events$unit_id %in% shipped
if (any(orphan)) {
  message(sprintf("  events: %d rows for %d units with no polygon dropped",
                  sum(orphan), n_distinct(events$unit_id[orphan])))
  events <- events[!orphan, ]
}
message(sprintf("  events: %d for %d units", nrow(events), n_distinct(events$unit_id)))
timer_end(t0)

message("  --- m5 checks ---")
check(nrow(events) > 5000, sprintf("events > 5000 (got %d)", nrow(events)))
check(all(events$event %in% c("created", "abolished", "merged_into", "split_from",
                              "part_transferred", "renamed", "recoded", "parent_changed",
                              "became_annex", "became_kapell", "dissolved", "other")),
      "known event kinds only")
check(sum(events$event == "renamed") > 100,
      sprintf("renames recorded (got %d)", sum(events$event == "renamed")))
pkg_data <- "data-raw/model/out/pkg/data"
if (dir.exists(pkg_data)) {
  save(events, file = file.path(pkg_data, "events.rda"), compress = "gzip")
  message("  Saved events.rda into the package tree")
}
saveRDS(list(events = events), "data-raw/model/out/m5.rds")
message("  Saved data-raw/model/out/m5.rds")
