#' m3a: Municipalities 1952-1990 from SCB
#'
#' and the county of every parish from the SCB codes Why SCB: the source register has 449
#' municipalities in 1971 against SCB's 464 (it gives one unit to two of them: its 1974 unit is
#' used from 1971), keeps the 1963-69 mergers running to 1970, and misses the temporary mergers
#' of 1974-82 (FOLLOWUP.md C4).
#'
#' Input:  data-raw/sources/scb_codes.csv (prepare_scb.R writes it; the repo carries a copy):
#' one row per
#' parish code and validity interval, with the county (digits 1-2) and municipality
#' (digits 1-4) the code belongs to, and code_1990 (the code the parish had by 1991,
#' following recodings) as a stable parish key
#' data-raw/validation/external/cache/scb/scb_panel.csv  (report only)
#' /home/rstudiojunkka/rproj/Samuel/reports/data/muncipals_2011.csv (SCB's municipality
#' register: code, name, registered, deregistered; holds only codes valid in 2011)
#' data-raw/data/Tbl_topografi.csv, Tbl_topografi_rel.csv (the source's own
#' municipality -> parish links, dated)
#' data-raw/model/out/m1.rds (units, records: the source's units and their topo_ids)
#' Output: data-raw/model/out/m3a.rds:
#' mun_units, mun_names, mun_evidence, county_evidence, report

suppressMessages({library(dplyr); library(tidyr); library(readr)})
source("data-raw/build_helpers.R")          # timer_start/timer_end, check()
source("data-raw/model/model_helpers.R")    # year_start/year_end, TYPE_PREFIX

t0 <- timer_start("m3a: SCB municipalities 1952-1990")
scb_dir  <- "data-raw/validation/external/cache/scb"   # scb_panel.csv, for the report only
mun_list <- source_file("muncipals_2011.csv")
D0 <- as.Date("1952-01-01"); D1 <- as.Date("1991-01-01")   # window [D0, D1)

# County codes as SCB used them in 1952-1990 (the 1997/1998 mergers are outside the window).
COUNTY <- c("01" = "Stockholms län", "03" = "Uppsala län", "04" = "Södermanlands län",
            "05" = "Östergötlands län", "06" = "Jönköpings län", "07" = "Kronobergs län",
            "08" = "Kalmar län", "09" = "Gotlands län", "10" = "Blekinge län",
            "11" = "Kristianstads län", "12" = "Malmöhus län", "13" = "Hallands län",
            "14" = "Göteborgs och Bohus län", "15" = "Älvsborgs län", "16" = "Skaraborgs län",
            "17" = "Värmlands län", "18" = "Örebro län", "19" = "Västmanlands län",
            "20" = "Kopparbergs län", "21" = "Gävleborgs län", "22" = "Västernorrlands län",
            "23" = "Jämtlands län", "24" = "Västerbottens län", "25" = "Norrbottens län")

# Names folded for comparison: lower case, no type words, no diacritics. A stads- or
# landsförsamling keeps a marker, so the two parishes of a town do not fold together. The
# districts of a parish (Söderåkra norra kbfd) fold to the parish.
name_key <- function(x){
  x <- tolower(ifelse(is.na(x), "", x))
  x <- gsub("\\([^)]*\\)", "", x)
  x <- gsub("s:t ", "sankt ", x, fixed = TRUE); x <- gsub("s:ta ", "sankta ", x, fixed = TRUE)
  district <- grepl("kbfd|kyrkoområde|kapellag", x)
  x <- gsub(",? del$|\\bkbfd\\b|\\bkyrkoområde(t|n)?\\b|\\bkapellag(et)?\\b", "", x)
  x[district] <- sub("\\s+(norra|södra|östra|västra|övre|nedre|yttre|inre|gamla|nya)\\s*$", "",
                     x[district])
  x <- gsub("\\blands(kommuns?|församling(en)?|förs\\.?)", "\001L", x)
  x <- gsub("\\bstads(församling(en)?|förs\\.?)", "\001S", x)
  mark <- ifelse(grepl("\001L", x), "_L", ifelse(grepl("\001S", x), "_S", ""))
  x <- gsub("(dom)?kyrko|församling(en)?|förs\\.?|kommunen|kommun|landskommun|stad(en)?|köping(en)?|municipalsamhälle",
            "", x)
  x <- gsub("[^a-zåäöéü]", "", x)
  x <- sub("s$", "", x)
  paste0(chartr("åäöéü", "aaoeu", x), mark)
}

# ---- 1. Membership intervals from the SCB codes -------------------------------------------
codes <- read_csv(source_file("scb_codes.csv"),
                  col_types = cols(.default = "c", from = "D", to = "D"))
iv <- codes %>%
  mutate(to = coalesce(to, D1)) %>%
  filter(from < D1, to > D0) %>%
  mutate(start_date = pmax(from, D0), end_date = pmin(to, D1) - 1,     # end inclusive
         partial  = grepl(",\\s*del$", name) | split %in% "TRUE",
         district = grepl("\\bkbfd\\b|kyrkoområde|kapellag", name),
         parish_key = code_1990) %>%
  filter(start_date <= end_date) %>%
  select(parish_key, code, scb_name = name, county, mun_code = municipality,
         start_date, end_date, partial, district)
message(sprintf("  SCB membership intervals 1952-1990: %d (%d parish keys, %d municipality codes)",
                nrow(iv), n_distinct(iv$parish_key), n_distinct(iv$mun_code)))

# Municipality code activity: the member intervals of a code, merged where they overlap or touch.
# A code with a gap (Vadstena 0584, in Motala 1974-79) gets one span per period of activity.
DATES <- sort(unique(c(iv$start_date, iv$end_date + 1)))   # the change dates, all 1 January
DATES <- DATES[DATES >= D0 & DATES < D1]
spans <- iv %>% arrange(mun_code, start_date) %>%
  group_by(mun_code) %>%
  mutate(span = cumsum(start_date > cummax(as.integer(lag(end_date, default = as.Date("1900-01-01")))) + 1)) %>%
  group_by(mun_code, span) %>%
  summarise(start_date = min(start_date), end_date = max(end_date), .groups = "drop") %>%
  arrange(mun_code, start_date)
spans$node <- seq_len(nrow(spans))
message(sprintf("  Municipality code spans: %d over %d codes (%d codes with a gap)",
                nrow(spans), n_distinct(spans$mun_code),
                sum(duplicated(spans$mun_code))))

# Each membership interval belongs to one span
iv_span <- iv %>%
  left_join(spans %>% select(mun_code, node, s = start_date, e = end_date),
            by = "mun_code", relationship = "many-to-many") %>%
  filter(start_date >= s, start_date <= e) %>% select(-s, -e)

# Parishes of each span at each change date (for the flows) and at 1 January of each year
mem <- iv_span %>% tidyr::crossing(d = DATES) %>% filter(d >= start_date, d <= end_date) %>%
  select(d, parish_key, mun_code, node, partial, district)

# ---- 2. Identity: chain the spans into units ----------------------------------------------
# At each change date, count how many parishes flow from one span to another.
events <- lapply(DATES[-1], function(d){
  prev_d <- DATES[match(d, DATES) - 1]
  a <- mem %>% filter(d == prev_d) %>% select(parish_key, from = node)
  b <- mem %>% filter(d == !!d) %>% select(parish_key, to = node)
  inner_join(a, b, by = "parish_key", relationship = "many-to-many") %>%
    count(from, to, name = "flow") %>% mutate(date = d)
}) %>% bind_rows()
size <- mem %>% count(node, d, name = "n") %>% group_by(node) %>%
  summarise(n_first = n[which.min(d)], n_last = n[which.max(d)], n_max = max(n), .groups = "drop")
sp <- spans %>% left_join(size, by = "node")

# continuation: B starts when A ends, A gives B most of B's parishes and most of its own
starts <- sp %>% filter(start_date > D0)
cont <- events %>% inner_join(starts %>% select(to = node, date = start_date, n_b = n_first),
                              by = c("to", "date")) %>%
  inner_join(sp %>% select(from = node, a_end = end_date, n_a = n_last), by = "from") %>%
  filter(a_end == date - 1) %>%                               # the predecessor ends here
  group_by(to) %>% slice_max(flow, n = 1, with_ties = FALSE) %>% ungroup() %>%
  filter(flow >= 0.5 * n_b, flow >= 0.9 * n_a)                # recoding, not a merger
message(sprintf("  Continuations across a code change (recodings): %d", nrow(cont)))

sp$unit_n <- create_block(c(sp$node, cont$from), c(sp$node, cont$to))[seq_len(nrow(sp))]
units_scb <- sp %>% group_by(unit_n) %>%
  # the code columns must be computed before start_date and end_date are summarised away
  summarise(codes = paste(unique(mun_code[order(start_date)]), collapse = ";"),
            code_first = mun_code[which.min(start_date)], code_last = mun_code[which.max(end_date)],
            start_date = min(start_date), end_date = max(end_date),
            n_spans = n(), n_parishes_max = max(n_max), .groups = "drop") %>%
  arrange(code_first, start_date) %>%
  mutate(mun_id = sprintf("KS:%04d", row_number()))
sp <- sp %>% left_join(units_scb %>% select(unit_n, mun_id), by = "unit_n")
message(sprintf("  SCB municipality units 1952-1990: %d", nrow(units_scb)))

# Events at the start and end of each unit
node_unit <- setNames(sp$mun_id, sp$node)
ev <- events %>% mutate(from_u = node_unit[as.character(from)], to_u = node_unit[as.character(to)]) %>%
  filter(from_u != to_u)
u_start <- units_scb$start_date; names(u_start) <- units_scb$mun_id
u_end   <- units_scb$end_date;   names(u_end)   <- units_scb$mun_id
start_ev <- ev %>% filter(date == u_start[to_u]) %>% group_by(to_u) %>%
  summarise(preds = paste(unique(from_u), collapse = ";"), n_pred = n_distinct(from_u),
            pred_flow = sum(flow), .groups = "drop")
end_ev <- ev %>% filter(date == u_end[from_u] + 1) %>% group_by(from_u) %>%
  slice_max(flow, n = 1, with_ties = FALSE) %>% ungroup() %>%
  select(from_u, into = to_u, into_flow = flow)

units_scb <- units_scb %>%
  left_join(start_ev, by = c("mun_id" = "to_u")) %>%
  left_join(end_ev, by = c("mun_id" = "from_u")) %>%
  mutate(
    start_event = case_when(
      start_date == D0                        ~ "panel_floor",     # 1952: floor of the lists
      # before 1971 the panel is still short, so a unit without a predecessor was only first
      # seen then (its parishes had no code before), not necessarily formed then
      is.na(n_pred) & start_date < as.Date("1971-01-01") ~ "first_seen",
      is.na(n_pred)                           ~ "no_predecessor",
      n_pred >= 2                             ~ "formed",          # bildar
      TRUE                                    ~ "formed_from_one"),
    start_precision = ifelse(start_event %in% c("panel_floor", "first_seen"), "not_before", "exact"),
    end_event = case_when(
      end_date >= D1 - 1                      ~ "beyond_window",
      !is.na(into)                            ~ "merged_into",     # uppgår i / bildar
      TRUE                                    ~ "last_seen"),
    end_precision = ifelse(end_event %in% c("beyond_window", "last_seen"), "not_after", "exact"),
    county_first = substr(code_first, 1, 2), county_last = substr(code_last, 1, 2),
    # a merger into a municipality that already existed is "uppgår i"; into one that starts the
    # same day is "bildar"
    end_kind = case_when(end_event != "merged_into" ~ NA_character_,
                         u_start[into] <= end_date  ~ "uppgår i",
                         TRUE                       ~ "bildar"))

# ---- 3. The source's own municipalities, for the link --------------------------------------
topo <- read_csv("data-raw/data/Tbl_topografi.csv", col_types = cols(.default = "c"))
rel  <- read_csv("data-raw/data/Tbl_topografi_rel.csv", col_types = cols(.default = "c"))
m1   <- readRDS("data-raw/model/out/m1.rds")
rec  <- sf::st_drop_geometry(m1$records); m1_units <- m1$units; m1_names <- m1$unit_names
rm(m1); invisible(gc(FALSE))
topo2unit <- setNames(rec$unit_id, rec$topo_id)

# ref_code of a parish is "SE/" + its SCB code as of the register's snapshot + a 3-digit serial,
# so substr(4, 9) is the same key as code_1990 for all but 133 keys (parishes lumped or gone
# before the snapshot; listed in report$unmatched_parish_keys).
par_topo <- topo %>% filter(typ == "Kyrksocken") %>%
  transmute(topo_id, parish_key = substr(referenskod, 4, 9), serial = substr(referenskod, 10, 12),
            pname = namn, unit_id = unname(topo2unit[topo_id])) %>%
  left_join(rec %>% group_by(topo_id) %>% summarise(ps = min(start), pe = max(end), .groups = "drop"),
            by = "topo_id")
mun_topo <- topo %>% filter(typ == "Kommun / stad") %>%
  transmute(topo_id, ref_code = referenskod, mname = namn, ms = as.integer(vtidstart),
            me = as.integer(vtidslut), unit_id = unname(topo2unit[topo_id]))
links <- rel %>% filter(Ordning == "Överordnad") %>%
  transmute(mtopo = kalla_id, ptopo = dest_id, s = as.integer(Start), e = as.integer(Slut)) %>%
  inner_join(mun_topo %>% select(mtopo = topo_id, m1_unit = unit_id, mname), by = "mtopo") %>%
  inner_join(par_topo %>% distinct(ptopo = topo_id, parish_key, pname), by = "ptopo") %>%
  filter(!is.na(m1_unit))
message(sprintf("  Source municipality -> parish links: %d (%d municipality units)",
                nrow(links), n_distinct(links$m1_unit)))

# Parish keys the register codes otherwise: a parish split into kyrkobokföringsdistrikt is
# listed by SCB as its districts (Norsjö kbfd, Söderåkra norra kbfd), and the register gives
# such a parish a NAD code (SE/083471000) instead of an SCB one. Those keys are matched to the
# register by name within the county; the register county is taken from its ref_code, so a
# parish that changed county is matched on its later county.
reg_par <- par_topo %>% mutate(county = substr(parish_key, 1, 2), nkey = name_key(pname)) %>%
  distinct(parish_key, county, nkey)
scb_par <- iv %>% distinct(parish_key, county, scb_name) %>%
  group_by(parish_key) %>% slice_head(n = 1) %>% ungroup() %>%
  mutate(nkey = name_key(scb_name))
by_name <- scb_par %>% filter(!parish_key %in% reg_par$parish_key) %>%
  inner_join(reg_par %>% rename(reg_key = parish_key), by = c("county", "nkey"),
             relationship = "many-to-many") %>%
  group_by(parish_key) %>% filter(n_distinct(reg_key) == 1) %>%
  summarise(reg_key = first(reg_key), .groups = "drop")
key_map <- scb_par %>%
  transmute(parish_key,
            match_key = case_when(parish_key %in% reg_par$parish_key ~ parish_key,
                                  TRUE ~ by_name$reg_key[match(parish_key, by_name$parish_key)]),
            key_source = case_when(parish_key %in% reg_par$parish_key ~ "code",
                                   !is.na(match_key) ~ "name_county", TRUE ~ NA_character_))
message(sprintf("  Parish keys matched to the register: %d by code, %d by name, %d unmatched",
                sum(key_map$key_source == "code", na.rm = TRUE),
                sum(key_map$key_source == "name_county", na.rm = TRUE),
                sum(is.na(key_map$match_key))))
iv <- iv %>% left_join(key_map, by = "parish_key")
iv_span <- iv_span %>% left_join(key_map, by = "parish_key")

# ---- 4. Match every SCB municipality-year to a source municipality unit --------------------
YEARS <- 1952:1990
scb_year <- iv_span %>% left_join(sp %>% select(node, mun_id), by = "node") %>%
  tidyr::crossing(year = YEARS) %>%
  filter(as.Date(sprintf("%d-01-01", year)) >= start_date,
         as.Date(sprintf("%d-01-01", year)) <= end_date) %>%
  distinct(year, mun_id, parish_key, match_key)
src_year <- bind_rows(lapply(YEARS, function(y)
  links %>% filter(s <= y, e >= y) %>% distinct(m1_unit, parish_key) %>% mutate(year = y))) %>%
  rename(match_key = parish_key)
shared <- scb_year %>% filter(!is.na(match_key)) %>% distinct(year, mun_id, match_key) %>%
  inner_join(src_year, by = c("year", "match_key"), relationship = "many-to-many") %>%
  count(year, mun_id, m1_unit, name = "shared")
sizes_a <- scb_year %>% filter(!is.na(match_key)) %>% distinct(year, mun_id, match_key) %>%
  count(year, mun_id, name = "n_scb")
sizes_b <- src_year %>% count(year, m1_unit, name = "n_src")
jac <- shared %>% left_join(sizes_a, by = c("year", "mun_id")) %>%
  left_join(sizes_b, by = c("year", "m1_unit")) %>%
  mutate(jaccard = shared / (n_scb + n_src - shared))
# A municipality is the source's unit when (nearly) all the parishes SCB gives it are that
# unit's parishes. The other direction is not required: before 1967 SCB shows only a part of
# each municipality's parishes, so the source unit usually has more.
best <- jac %>% group_by(mun_id, m1_unit) %>%
  summarise(years = n(), shared_tot = sum(shared), n_scb_tot = sum(n_scb),
            jac_max = max(jaccard), cov_scb = sum(shared) / sum(n_scb),
            cov_src = sum(shared) / sum(n_src), .groups = "drop") %>%
  group_by(mun_id) %>% slice_max(order_by = cov_scb * shared_tot, n = 1, with_ties = FALSE) %>%
  ungroup()
m1_mun <- m1_units %>% filter(type_id == "municipality") %>%
  select(m1_unit = unit_id, m1_name_raw = name, m1_start = start_date, m1_end = end_date) %>%
  left_join(mun_topo %>% filter(!is.na(unit_id)) %>%
              group_by(m1_unit = unit_id) %>%
              summarise(m1_county = substr(ref_code[1], 4, 5), .groups = "drop"), by = "m1_unit")
units_scb <- units_scb %>% left_join(best, by = "mun_id") %>% left_join(m1_mun, by = "m1_unit") %>%
  mutate(status = ifelse(!is.na(m1_unit) & cov_scb >= 0.5, "in_source", "new_missing_in_source"),
         m1_unit = ifelse(status == "new_missing_in_source", NA_character_, m1_unit),
         m1_name_raw = ifelse(is.na(m1_unit), NA_character_, m1_name_raw),
         # the source writes a unit's period into some names ("Dorotea kommun 1863-1973")
         m1_name = sub("\\s+-?\\d{4}(\\s*-\\s*\\d{0,4})?$", "", m1_name_raw))
message(sprintf("  Linked to a source unit: %d of %d (%d missing in the source)",
                sum(units_scb$status == "in_source"), nrow(units_scb),
                sum(units_scb$status == "new_missing_in_source")))

# ---- 5. Names -------------------------------------------------------------------------------
# 1. SCB's own municipality register, for the codes it holds (valid in 2011)
scb_mun_names <- read_csv(mun_list, col_types = cols(.default = "c")) %>%
  setNames(c("county", "mun_code", "name", "from", "to")) %>%
  mutate(from = as.Date(from), to = as.Date(to))
by_code <- units_scb %>% select(mun_id, code_last, start_date, end_date) %>%
  inner_join(scb_mun_names %>% select(code_last = mun_code, scb_reg_name = name, from, to),
             by = "code_last", relationship = "many-to-many") %>%
  filter(from <= end_date, is.na(to) | to > start_date) %>%
  group_by(mun_id) %>% slice_min(from, n = 1, with_ties = FALSE) %>% ungroup() %>%
  select(mun_id, scb_reg_name)
# 2. the source's names, one per year the municipality matches a source unit, so that a
#    municipality that was a köping and then a kommun keeps both names with their periods.
#    A municipality with neither name keeps name NA: a parish name is not a municipality name
#    (municipality 0185 held only "Danderyd, del" and was Djursholms stad), so it is left for
#    research instead (report$units_without_name).
year_match <- jac %>% mutate(cov = shared / n_scb) %>% filter(cov >= 0.5) %>%
  group_by(mun_id, year) %>% slice_max(cov * shared, n = 1, with_ties = FALSE) %>%
  # where the source has one unit for two municipalities (its 1974 unit used from 1971, or a
  # 1963-69 merger it lacks), only the municipality with the most of that unit's parishes may
  # take its name
  group_by(year, m1_unit) %>% mutate(shared_unit = n() > 1,
                                     wins = shared == max(shared) & !duplicated(shared)) %>%
  ungroup() %>%
  arrange(mun_id, year) %>% group_by(mun_id) %>%
  mutate(run = cumsum(m1_unit != lag(m1_unit, default = "") | year != lag(year, default = 0L) + 1L)) %>%
  group_by(mun_id, run, m1_unit) %>%
  summarise(y_from = min(year), y_to = max(year), shared_unit = any(shared_unit),
            wins = any(wins), .groups = "drop")
src_names <- year_match %>% filter(wins) %>%
  left_join(units_scb %>% select(mun_id, u_from = start_date, u_to = end_date), by = "mun_id") %>%
  inner_join(m1_names %>% select(m1_unit = unit_id, name, n_start = start_date, n_end = end_date),
             by = "m1_unit", relationship = "many-to-many") %>%
  mutate(start_date = pmax(n_start, year_start(y_from), u_from),
         end_date = pmin(n_end, year_end(y_to), u_to),
         name = sub("\\s+-?\\d{4}(\\s*-\\s*\\d{0,4})?$", "", name)) %>%   # "Dorotea kommun 1863-1973"
  filter(start_date <= end_date) %>%
  group_by(mun_id, name) %>%
  summarise(start_date = min(start_date), end_date = max(end_date), .groups = "drop")
last_name <- src_names %>% group_by(mun_id) %>% slice_max(end_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>% select(mun_id, src_last_name = name)
# whether the source has a unit of its own for this municipality, or shares one with another
share_flag <- year_match %>% group_by(mun_id) %>%
  summarise(has_own_source_unit = any(wins), shares_source_unit = any(shared_unit),
            .groups = "drop")
# every source unit the municipality matches, with the years, e.g. "K:00123 1952-1970;K:00456 1971-1990"
m1_by_year <- year_match %>% arrange(mun_id, y_from) %>% group_by(mun_id) %>%
  summarise(m1_by_year = paste(sprintf("%s %d-%d", m1_unit, y_from, y_to), collapse = ";"),
            .groups = "drop")
units_scb <- units_scb %>% left_join(by_code, by = "mun_id") %>% left_join(last_name, by = "mun_id") %>%
  left_join(m1_by_year, by = "mun_id") %>% left_join(share_flag, by = "mun_id") %>%
  mutate(src_last_name = ifelse(status == "in_source", src_last_name, NA_character_),
         # SCB's register is the authority on the name where the two differ (the source keeps
         # the rural commune's name: Borlänge = "Stora Tuna kommun", Boden = "Överluleå kommun")
         conflict = !is.na(src_last_name) & !is.na(scb_reg_name) &
           name_key(src_last_name) != name_key(scb_reg_name),
         name = case_when(conflict ~ paste(scb_reg_name, "kommun"),
                          !is.na(src_last_name) ~ src_last_name,
                          !is.na(scb_reg_name) ~ paste(scb_reg_name, "kommun"),
                          TRUE ~ NA_character_),
         name_source = case_when(conflict | (is.na(src_last_name) & !is.na(scb_reg_name)) ~
                                   "scb_municipality_register",
                                 !is.na(src_last_name) ~ "source_register", TRUE ~ NA_character_),
         kind = case_when(grepl(" stad$", name) ~ "stad", grepl(" köping$", name) ~ "köping",
                          grepl(" landskommun$", name) ~ "landskommun",
                          grepl(" kommun$", name) ~ "kommun", TRUE ~ NA_character_))
message(sprintf("  Names: %d from the source, %d from SCB's register, %d without a name",
                sum(units_scb$name_source == "source_register", na.rm = TRUE),
                sum(units_scb$name_source == "scb_municipality_register", na.rm = TRUE),
                sum(is.na(units_scb$name))))
# Where both names exist they are an independent check on the parish-set match
name_conflicts <- units_scb %>% filter(conflict) %>%
  select(mun_id, codes, start_date, end_date, src_last_name, scb_reg_name, cov_scb, jac_max)
message(sprintf("  Source name against SCB's municipality register: %d of %d differ",
                nrow(name_conflicts),
                sum(!is.na(units_scb$src_last_name) & !is.na(units_scb$scb_reg_name))))

# A unit re-established later under the same code (Vadstena 1980, Essunga 1983)
reest <- units_scb %>% filter(start_event %in% c("formed", "formed_from_one")) %>%
  select(mun_id, code_first, start_date, name) %>%
  inner_join(units_scb %>% select(prev_id = mun_id, code_first, prev_end = end_date,
                                  prev_name = name), by = "code_first") %>%
  filter(prev_end < start_date, name_key(name) == name_key(prev_name)) %>%
  select(mun_id, reestablished_of = prev_id)
units_scb <- units_scb %>% left_join(reest, by = "mun_id")

# Municipalities whose only SCB members are districts (kbfd) or parts of parishes: their
# membership, and so their match, rests on a parish they share with another municipality
evidence_kind <- iv_span %>% left_join(sp %>% select(node, mun_id), by = "node") %>%
  group_by(mun_id) %>%
  summarise(evidence_kind = ifelse(all(partial | district), "districts_only", "parishes"),
            .groups = "drop")
# Parishes SCB divides between two municipalities in a year (parts and districts). Where every
# member of a municipality is such a share, its match to a source unit rests on a parish it
# does not hold alone, so the link is uncertain (0185 held only "Danderyd, del" and is not
# Danderyds köping but Djursholms stad, which the source lacks).
split_parishes <- scb_year %>% filter(!is.na(match_key)) %>%
  distinct(year, match_key, mun_id) %>% count(year, match_key, name = "municipalities") %>%
  filter(municipalities > 1)
uncertain <- scb_year %>% filter(!is.na(match_key)) %>%
  semi_join(split_parishes, by = c("year", "match_key")) %>%
  distinct(mun_id) %>% mutate(shares_every_parish = TRUE)

mun_units <- units_scb %>% left_join(evidence_kind, by = "mun_id") %>%
  left_join(uncertain, by = "mun_id") %>%
  mutate(start_event = ifelse(!is.na(reestablished_of), "reestablished", start_event),
         status = case_when(
           status == "in_source" & evidence_kind == "districts_only" &
             shares_every_parish %in% TRUE                       ~ "in_source_uncertain",
           status == "in_source" & !has_own_source_unit %in% TRUE ~ "shares_a_source_unit",
           TRUE                                                   ~ status)) %>%
  transmute(mun_id, name, name_source, kind, county_code = county_last,
            county_name = unname(COUNTY[county_last]), county_first, codes, code_first, code_last,
            start_date, end_date, start_event, start_precision, end_event, end_kind, end_precision,
            merged_into = into, predecessors = preds, n_parishes_max, n_spans, evidence_kind,
            m1_unit_id = m1_unit, m1_units_by_year = m1_by_year, m1_name, m1_name_raw,
            m1_start, m1_end,
            scb_register_name = scb_reg_name, match_jaccard = round(jac_max, 3),
            match_coverage_scb = round(cov_scb, 3), match_coverage_source = round(cov_src, 3),
            match_years = years, status, shares_source_unit, reestablished_of)

# mun_names: the source's names over their periods, and SCB's register name
mun_names <- bind_rows(
  src_names %>% semi_join(mun_units %>% filter(status == "in_source"), by = "mun_id") %>%
    mutate(kind = "official", source = "Riksarkivet topografi"),
  mun_units %>% filter(!is.na(scb_register_name)) %>%
    transmute(mun_id, name = scb_register_name, start_date, end_date, kind = "official",
              source = "SCB kommunregister")) %>%
  distinct(mun_id, name, start_date, end_date, .keep_all = TRUE) %>%
  left_join(mun_units %>% select(mun_id, scb_register_name), by = "mun_id") %>%
  # the source's name for a period can be the name of a municipality that had already been
  # merged away (the source runs Dörby kommun to 1970, although SCB moved its parishes to
  # Kalmar in 1965), so a source name that disagrees with SCB's is marked
  mutate(note = ifelse(source == "Riksarkivet topografi" & !is.na(scb_register_name) &
                         name_key(name) != name_key(scb_register_name),
                       "disagrees with SCB's name for this municipality", NA_character_)) %>%
  select(-scb_register_name) %>% arrange(mun_id, start_date)

# ---- 6. Evidence: parish -> municipality, parish -> county ---------------------------------
# The source's parish unit for each SCB parish key, at the middle of the interval
pv <- par_topo %>% filter(!is.na(unit_id)) %>%
  transmute(parish_key, m1_parish_unit = unit_id, serial, ps, pe)
pick_parish <- function(key, y){
  d <- tibble(i = seq_along(key), parish_key = key_map$match_key[match(key, key_map$parish_key)],
              y = y) %>%
    left_join(pv, by = "parish_key", relationship = "many-to-many")
  ok <- d %>% filter(!is.na(m1_parish_unit), ps <= y, pe >= y) %>%
    group_by(i) %>% slice_min(serial, n = 1, with_ties = FALSE) %>% ungroup()
  any_v <- d %>% filter(!is.na(m1_parish_unit)) %>%
    group_by(i) %>% slice_min(serial, n = 1, with_ties = FALSE) %>% ungroup()
  out <- rep(NA_character_, length(key)); how <- rep(NA_character_, length(key))
  out[any_v$i] <- any_v$m1_parish_unit; how[any_v$i] <- "key_any_version"
  out[ok$i] <- ok$m1_parish_unit;       how[ok$i] <- "key_at_date"
  list(unit = out, how = how)
}
mun_evidence <- iv_span %>% left_join(sp %>% select(node, mun_id), by = "node") %>%
  select(parish_key, register_key = match_key, key_source, scb_code = code, scb_name,
         county_code = county, mun_code, mun_id, start_date, end_date, partial, district)
mid_year <- as.integer(format(mun_evidence$start_date +
                                (mun_evidence$end_date - mun_evidence$start_date) / 2, "%Y"))
pp <- pick_parish(mun_evidence$parish_key, mid_year)
mun_evidence <- mun_evidence %>%
  mutate(m1_parish_unit = pp$unit, parish_match = pp$how,
         mun_name = mun_units$name[match(mun_id, mun_units$mun_id)], source = "scb") %>%
  arrange(parish_key, start_date)

county_evidence <- iv %>%
  transmute(parish_key, scb_code = code, scb_name, county_code = county,
            county_name = unname(COUNTY[county]), mun_code, start_date, end_date,
            partial, district,
            # Stockholm city was its own county (överståthållarskapet) until 1968; SCB codes it 01
            note = ifelse(mun_code == "0180" & start_date < as.Date("1968-01-01"),
                          "Stockholms stad (överståthållarämbetet) until 1968", NA_character_),
            source = "scb")
cp <- pick_parish(county_evidence$parish_key,
                  as.integer(format(county_evidence$start_date +
                                      (county_evidence$end_date - county_evidence$start_date) / 2, "%Y")))
# the source abbreviates ("Göteborgs o Bohus län") and uses the 1997 name for Kopparbergs län
m1_county <- m1_units %>% filter(type_id == "county") %>%
  transmute(m1_county_unit = unit_id, cname = name,
            key = name_key(gsub(" o ", " och ", name)), cs = start_date, ce = end_date) %>%
  bind_rows(m1_units %>% filter(type_id == "county", name == "Dalarnas län") %>%
              transmute(m1_county_unit = unit_id, cname = name,
                        key = name_key("Kopparbergs län"), cs = start_date, ce = end_date))
county_evidence <- county_evidence %>%
  mutate(m1_parish_unit = cp$unit, key = name_key(county_name)) %>%
  left_join(m1_county, by = "key", relationship = "many-to-many") %>%
  filter(is.na(m1_county_unit) | (cs <= end_date & ce >= start_date)) %>%
  group_by(parish_key, scb_code, start_date) %>% slice_head(n = 1) %>% ungroup() %>%
  select(parish_key, m1_parish_unit, scb_code, scb_name, county_code, county_name,
         m1_county_unit, mun_code, start_date, end_date, partial, district, note, source) %>%
  arrange(parish_key, start_date)
message(sprintf("  Evidence rows: %d municipality, %d county; parish linked to a source unit: %.1f%%",
                nrow(mun_evidence), nrow(county_evidence),
                100 * mean(!is.na(mun_evidence$m1_parish_unit))))

# ---- 7. Report -------------------------------------------------------------------------------
# The codes valid on 1 January of each year. prepare_scb.R writes this out as scb_panel.csv, but
# it is 5 MB of pure derivation, so the repo does not carry it and it is rebuilt here.
panel_f <- source_file("scb_panel.csv")
panel <- if (!is.na(panel_f)) read_csv(panel_f, col_types = cols(.default = "c", year = "i")) else
  bind_rows(lapply(YEARS, function(y){
    d <- as.Date(sprintf("%d-01-01", y))
    codes %>% filter(from <= d, is.na(to) | to > d) %>% mutate(year = y)
  }))
src_mun_year <- bind_rows(lapply(YEARS, function(y)
  tibble(year = y, m1_unit = unique(links$m1_unit[links$s <= y & links$e >= y]))))
src_par_year <- bind_rows(lapply(YEARS, function(y)
  tibble(year = y, n_src_par = n_distinct(links$parish_key[links$s <= y & links$e >= y]))))
# Municipality-years for which the source has no unit: the municipality is missing, or the
# source gives its unit the wrong period (the 1971-73 units replaced by the 1974 ones)
matched_years <- year_match %>% rowwise() %>%
  mutate(year = list(y_from:y_to)) %>% ungroup() %>% tidyr::unnest(year) %>%
  distinct(mun_id, year)
mun_years <- scb_year %>% distinct(mun_id, year)
gap_years <- mun_years %>% anti_join(matched_years, by = c("mun_id", "year")) %>%
  left_join(mun_units %>% select(mun_id, name, codes, county_name, start_date, end_date, status),
            by = "mun_id") %>% arrange(year, codes)
# and municipality-years where the source has one unit for two SCB municipalities: the source
# keeps the 1974 unit from 1971 (FOLLOWUP C4), or a 1963-69 merger is missing
lumped_years <- matched_years %>%
  inner_join(year_match %>% rowwise() %>% mutate(year = list(y_from:y_to)) %>% ungroup() %>%
               tidyr::unnest(year) %>% distinct(mun_id, year, m1_unit),
             by = c("mun_id", "year")) %>%
  group_by(year, m1_unit) %>% filter(n_distinct(mun_id) > 1) %>%
  summarise(municipalities = paste(sort(unique(mun_id)), collapse = ";"), .groups = "drop")
lumped_named <- lumped_years %>%
  tidyr::separate_longer_delim(municipalities, ";") %>%
  rename(mun_id = municipalities) %>%
  left_join(mun_units %>% select(mun_id, name, codes, county_name), by = "mun_id") %>%
  group_by(year, m1_unit) %>%
  summarise(municipalities = paste(sprintf("%s (%s)", coalesce(name, mun_id), codes),
                                   collapse = "; "),
            source_name = m1_units$name[match(m1_unit[1], m1_units$unit_id)], .groups = "drop") %>%
  arrange(year, m1_unit)
gap_by_year <- mun_years %>% count(year, name = "scb_municipalities") %>%
  left_join(gap_years %>% count(year, name = "without_a_source_unit"), by = "year") %>%
  left_join(lumped_years %>% count(year, name = "source_units_for_two_municipalities"),
            by = "year") %>%
  mutate(across(c(without_a_source_unit, source_units_for_two_municipalities),
                ~ coalesce(.x, 0L)))

coverage <- tibble(year = YEARS) %>%
  left_join(panel %>% count(year, name = "scb_codes"), by = "year") %>%
  left_join(scb_year %>% count(year, name = "scb_parish_keys"), by = "year") %>%
  left_join(src_par_year, by = "year") %>%
  left_join(scb_year %>% distinct(year, mun_id) %>% count(year, name = "scb_municipalities"),
            by = "year") %>%
  left_join(src_mun_year %>% count(year, name = "source_municipalities"), by = "year") %>%
  left_join(gap_by_year %>% select(year, without_a_source_unit,
                                   source_units_for_two_municipalities), by = "year") %>%
  mutate(parish_ratio_scb_to_source = round(scb_parish_keys / n_src_par, 3))
published <- c("1971" = 464, "1974" = 278, "1980" = 279, "1990" = 284)
mun_counts <- coverage %>% filter(year %in% c(1952, 1960, 1966, 1967, 1970, 1971, 1974, 1977,
                                              1980, 1983, 1990)) %>%
  select(year, scb_municipalities, source_municipalities) %>%
  mutate(published = unname(published[as.character(year)]))

unmatched_units <- mun_units %>% filter(status == "new_missing_in_source") %>%
  select(mun_id, name, name_source, county_name, codes, start_date, end_date, start_event,
         n_parishes_max, match_jaccard, match_coverage_scb)
units_without_name <- mun_units %>% filter(is.na(name)) %>%
  left_join(mun_evidence %>% group_by(mun_id) %>%
              summarise(parishes = paste(unique(scb_name), collapse = "; "), .groups = "drop"),
            by = "mun_id") %>%
  select(mun_id, codes, county_name, start_date, end_date, status, parishes)
unmatched_keys <- iv %>% distinct(parish_key, scb_name, county, match_key) %>%
  filter(is.na(match_key)) %>%
  mutate(county_name = unname(COUNTY[county])) %>% arrange(county, parish_key) %>%
  select(-match_key)

# Problems in the source's own 1863-1951 municipalities that SCB cannot fix (not fixed here)
src_1863 <- mun_topo %>% filter(!is.na(unit_id), ms <= 1951, me >= 1863)
par_names <- links %>% filter(s <= 1951) %>% distinct(m1_unit, pname) %>%
  mutate(pkey = name_key(pname))
town_named <- src_1863 %>%
  transmute(m1_unit = unit_id, mname, start = ms, end = me, mkey = name_key(mname)) %>%
  distinct() %>%
  left_join(par_names %>% group_by(m1_unit) %>%
              summarise(parishes = paste(sort(unique(pname)), collapse = "; "),
                        keys = list(unique(pkey)), .groups = "drop"), by = "m1_unit") %>%
  rowwise() %>%
  mutate(name_in_parishes = !is.null(keys) && any(keys == mkey | startsWith(keys, mkey) |
                                                    startsWith(mkey, keys))) %>%
  ungroup() %>%
  filter(!name_in_parishes, start <= 1863, grepl(" stad$| köping$", mname)) %>%
  select(m1_unit, mname, start, end, parishes) %>% arrange(mname)
# parishes with no municipality in the source, 1863-1951
par_active <- rec %>% filter(type_id == "parish") %>%
  group_by(topo_id) %>% summarise(ps = min(start), pe = max(end), .groups = "drop") %>%
  inner_join(par_topo %>% distinct(topo_id, pname, parish_key), by = "topo_id")
no_mun <- par_active %>% filter(ps <= 1930, pe >= 1900) %>%
  anti_join(links %>% filter(s <= 1930, e >= 1900) %>% distinct(parish_key), by = "parish_key") %>%
  select(topo_id, pname, parish_key, ps, pe) %>% arrange(pname)
named_missing <- c("Noraskog", "Värmlandsnäs", "Våmhus", "Kinnarumma", "Kumla landskommun",
                   "Ronneby landskommun", "Lärbro", "Larv")
named_check <- tibble(name = named_missing) %>%
  rowwise() %>%
  mutate(in_source = paste(mun_topo$mname[grepl(sub(" landskommun", "", name), mun_topo$mname)],
                           collapse = "; ")) %>% ungroup()

notes <- c(
  "SCB parish codes are a dated municipality and county membership table: digits 1-2 are the county, 1-4 the municipality, and a parish is recoded whenever either changes. Every change date in 1952-1990 is 1 January.",
  "1967-1990 is complete: the municipality counts are 464 (1971), 278 (1974), 279 (1980) and 284 (1990), the published numbers.",
  "1952-1966 is a subset: the SCB lists hold only codes still valid in 1967 or later, so 1,624 of about 2,500 parishes in 1952 and 476 of about 1,037 municipalities. A municipality first seen then may be older, so start_precision is 'not_before'. Mergers before 1967 are visible only through the new codes issued at them (162 codes registered 1953-1966), and the municipality a parish left is usually not in the lists at all.",
  "1967 and 1968 are also short (795 and 843 municipalities against 847-848 in 1969-1970): the parishes recoded in 1968-69 have no earlier code in the lists. Every start before 1971 without a predecessor is therefore 'first_seen', not a formation.",
  "A municipality's end date is exact when its parishes move to another municipality (merged_into); 'beyond_window' means it was still there at 1 Jan 1991.",
  "Identity: a code that stays active is one unit; a code that replaces another which gives it most of its parishes and (nearly) all of its own is the same unit (a recoding); otherwise the new code is a new unit and its predecessors merged into it.",
  "SCB gave the municipalities of the 1971 reform their codes already in 1967-70, so at 1971 and 1974 codes only disappear: every merger comes out as 'uppgår i' and there is no 'bildar' event. Where the reform legally formed a new kommun, this model keeps the block's central municipality as the same unit. A DECISION for the coordinator: accept SCB's continuity, or treat 1971 and 1974 as formations.",
  "Vadstena (0584), Essunga (1603), Österåker (0117), Salem (0128), Bjurholm (2403), Malå (2418) and Dorotea (2425) end in 1974 and start again in 1980 or 1983 with the same code and name; under the rule those are new units, marked with reestablished_of.",
  "Names: from the source unit the municipality matches in its last year, or, where the two differ, from SCB's municipality register (Borlänge, which the source calls Stora Tuna kommun; Boden, which it calls Överluleå kommun). A parish name is never used as a municipality name. mun_names keeps every name with its period, and marks a source name that disagrees with SCB's.",
  "Three municipalities have no name and no unit in the source: 0664 (1967-70, only Bodafors kbfd), 1564 (1967-70, only Skene kbfd) and 2405 (1967-70, only Vännäs landskommuns kbfd). Their names need research (SFS); the district names suggest Bodafors köping, Skene köping and Vännäs landskommun.",
  "Five more municipalities are linked to a source unit only through a parish they share with another municipality (status in_source_uncertain): 0185, 0663, 1164, 2103 and 2160. 0185 is not Danderyds kommun but the town municipality that held part of Danderyd parish (Djursholms stad), which the source lacks.",
  "17 parishes are divided between two municipalities in some year (report$parishes_split_between_municipalities), SCB's 'delad av kommungräns'; their evidence rows have partial or district TRUE. These are the parish parts the atom model needs.",
  "The county of a Stockholm city parish before 1968 is coded 01, although Stockholms stad was its own county (överståthållarämbetet) until then; the rows carry a note.",
  "Two municipalities change county: 0304 -> 0139 Upplands-Bro (Uppsala -> Stockholm) and 0189 -> 0382 Östhammar (Stockholm -> Uppsala), both in 1971.",
  "Algutsboda and Hälleberga are coded in Kalmar county from 1 Jan 1969, which is the correction FOLLOWUP C4 asks for. Their earlier codes (Kronobergs län) are not in the lists, so there is no SCB row before 1969.",
  "56 SCB parish keys have no counterpart in the source, by code or by name (Stockholm's Klara and Jakob, lumped in the register, Göteborgs Kristine and tyska, kbfd of parishes the source does not name); their evidence rows have no m1_parish_unit.",
  "12 municipalities have no name (report$units_without_name), each with its SCB parishes: the source has no unit of its own for them (Bodafors, Skene, Norrahammar, Oskarström, Vännäs landskommun, Tärendö, Örträsk, the part of Danderyd that was Djursholms stad, ...). They need a name from SFS.",
  "kind (stad / köping / landskommun / kommun) is read off the name, and the source calls almost every unit 'kommun', so it is 777 kommun, 52 landskommun, 8 köping and 6 stad. It is not a reliable classification.",
  "In 1971-73 the source gives one unit to two or more SCB municipalities in 12 cases (Motala, Kinda, Mönsterås, Gislaved, Borgholm, Mörbylånga, ...), which is the 464 against 449; in 1967-70 it does so in 14-16 cases (Laholm, Lycksele, Mora, Pajala, Strängnäs, ...), the missing 1963-69 mergers. report$source_unit_for_two_municipalities lists them by year.",
  "Not used: the death book, NAPP, Sveriges statskalender, ISOF and the gold sample.",
  "Not fixed here (1863-1951, SCB cannot reach it): report$pre1952_town_named lists 11 rural communes the source names after a town that came later (Tranås = Säby, Mölndal = Fässberg, Tierp = Tolfta, Lilla Edet = Fuxerna, Fagersta = Västanfors, Frövi = Näsby, Laxå = Ramundeboda, Mellerud = Holm/Järn, Finspång = Risinge, Tingsryd = Tingsås, and Söderköping = Sankt Laurentii, which is a real town). Bengtsfors köping (1863-1970) is the same case but escapes the test, because the source gives Ärtemark a second parish named Bengtsfors from 1905.",
  "Not fixed here: Noraskog, Värmlandsnäs, Våmhus and Kinnarumma are missing from the source altogether; Kumla and Ronneby landskommun (1952-62/66) are missing next to Kumla stad and Ronneby stad; Larv and Lärbro exist as municipalities but have no parish link in 1900-1930.")

split_by_municipality <- split_parishes %>%
  left_join(scb_year %>% filter(!is.na(match_key)) %>% distinct(year, match_key, mun_id) %>%
              left_join(mun_units %>% select(mun_id, mname = name), by = "mun_id") %>%
              group_by(year, match_key) %>%
              summarise(municipalities_named = paste(sort(unique(coalesce(mname, mun_id))),
                                                     collapse = "; "), .groups = "drop"),
            by = c("year", "match_key")) %>%
  left_join(iv %>% distinct(match_key, scb_name) %>% group_by(match_key) %>%
              slice_head(n = 1) %>% ungroup(), by = "match_key") %>%
  arrange(match_key, year)

report <- list(coverage = coverage, mun_counts = mun_counts,
               unmatched_units = unmatched_units, units_without_name = units_without_name,
               unmatched_parish_keys = unmatched_keys, name_conflicts = name_conflicts,
               parishes_split_between_municipalities = split_by_municipality,
               municipality_years_without_a_source_unit = gap_years,
               source_unit_for_two_municipalities = lumped_named,
               start_events = count(mun_units, start_event, start_precision),
               end_events = count(mun_units, end_event, end_kind),
               status = count(mun_units, status, name_source),
               pre1952_town_named = town_named, pre1952_parish_without_municipality = no_mun,
               pre1952_named_missing = named_check, notes = notes)

# ---- 8. Checks --------------------------------------------------------------------------------
message("  --- m3a checks ---")
check(!anyNA(mun_units$mun_id), "every municipality has an id")
check(all(mun_units$start_date <= mun_units$end_date), "municipality periods are ordered")
check(!anyNA(mun_evidence$mun_id), "every evidence row has a municipality")
check(all(mun_evidence$start_date <= mun_evidence$end_date), "evidence periods are ordered")
for (y in c(1971, 1974, 1980, 1990)) {
  d <- as.Date(sprintf("%d-01-01", y))
  n <- sum(mun_units$start_date <= d & mun_units$end_date >= d)
  check(n == published[as.character(y)],
        sprintf("%d: %d municipalities (SCB %d)", y, n, published[as.character(y)]))
}
# no parish in two municipalities at one date unless it is a part (", del") or a district
dup <- mun_evidence %>% filter(!partial, !district) %>%
  inner_join(mun_evidence %>% filter(!partial, !district),
             by = "parish_key", relationship = "many-to-many") %>%
  filter(scb_code.x < scb_code.y, start_date.x <= end_date.y, start_date.y <= end_date.x)
check(nrow(dup) == 0, sprintf("no whole parish in two municipalities at one date (%d)", nrow(dup)))
cdup <- county_evidence %>% filter(!partial, !district) %>%
  inner_join(county_evidence %>% filter(!partial, !district), by = "parish_key",
             relationship = "many-to-many") %>%
  filter(scb_code.x < scb_code.y, start_date.x <= end_date.y, start_date.y <= end_date.x,
         county_code.x != county_code.y)
check(nrow(cdup) == 0, sprintf("no whole parish in two counties at one date (%d)", nrow(cdup)))
check(!anyNA(county_evidence$m1_county_unit), "every county evidence row has a source county")
inside <- mun_evidence %>% left_join(mun_units %>% select(mun_id, u_s = start_date, u_e = end_date),
                                     by = "mun_id") %>%
  filter(start_date < u_s | end_date > u_e)
check(nrow(inside) == 0, sprintf("every evidence row is inside its municipality's period (%d)", nrow(inside)))
print(as.data.frame(mun_counts))
timer_end(t0)
saveRDS(list(mun_units = mun_units, mun_names = mun_names, mun_evidence = mun_evidence,
             county_evidence = county_evidence, report = report),
        "data-raw/model/out/m3a.rds")
message("  Saved data-raw/model/out/m3a.rds")
