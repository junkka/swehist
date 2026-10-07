#' m3d: Deaneries (kontrakt) and pastorat from Sveriges statskalender
#'
#' Kontrakt membership was the one level in the model that rested on geometry alone: the
#' register has no kontrakt -> parish links, so m3 had only containment, and `quality_table`
#' said 0% sourced.
#'
#' Input:  data-raw/model/out/{m1,m2}.rds, the statskalender page texts (cached, see
#' statskalender_parse.R), data-raw/model/data/statskalender_kyrkor_<year>.csv (tracked,
#' written by this script on the first run)
#' Output: data-raw/model/out/m3d.rds: list(kyrkor, evidence, chain, report)

source("data-raw/build_helpers.R")
source("data-raw/model/model_helpers.R")
stk_cache <- "data-raw/model/out/cache/statskalender"
STK_ALT   <- "data-raw/validation/external/cache/statskalender"   # already downloaded volumes
dir.create(stk_cache, showWarnings = FALSE, recursive = TRUE)
for (w in c("sonkal_1866", "statskal_1881", "statskal_1931"))    # reuse what is on disk
  if (!dir.exists(file.path(stk_cache, w)) && dir.exists(file.path(STK_ALT, w)))
    file.symlink(normalizePath(file.path(STK_ALT, w)), file.path(stk_cache, w))
source("data-raw/model/statskalender_parse.R")

m1 <- readRDS("data-raw/model/out/m1.rds")
m2 <- readRDS("data-raw/model/out/m2.rds")
t0 <- timer_start("m3d: deaneries from the statskalender")

ED <- tribble(
  ~year, ~work,           ~from, ~to,   ~parser,
  1866,  "sonkal/1866",    339,   376,  "1866",
  1881,  "statskal/1881",  299,   326,  "1881",
  1931,  "statskal/1931",  559,   624,  "1931")
DATA <- "data-raw/model/data"

# ---- 1. The printed lists, parsed once and tracked ------------------------------------------
read_edition <- function(i){
  y <- ED$year[i]
  f <- file.path(DATA, sprintf("statskalender_kyrkor_%d.csv", y))
  if (file.exists(f)) {
    x <- read_csv(f, show_col_types = FALSE)
    message(sprintf("  %d  %5d parish rows (cached)", y, nrow(x)))
    return(x)
  }
  k <- switch(ED$parser[i],
              "1866" = parse_kyrkor_1866(y, ED$work[i], ED$from[i], ED$to[i]),
              "1881" = parse_kyrkor_1881(y, ED$work[i], ED$from[i], ED$to[i]),
              "1931" = parse_kyrkor_1931(y, ED$work[i], ED$from[i], ED$to[i]))
  x <- stk_pastorat_parishes(k) %>%
    transmute(year, stift = stift_raw, kontrakt, pastorat, parish, is_mother,
              county_raw = if ("county_raw" %in% names(.)) county_raw else NA_character_,
              page, url)
  write_csv(x, f)
  message(sprintf("  %d  %5d parish rows parsed (%d kontrakt, %d stift)", y, nrow(x),
                  n_distinct(na.omit(x$kontrakt)), n_distinct(na.omit(x$stift))))
  x
}
kyrkor <- bind_rows(lapply(seq_len(nrow(ED)), read_edition))

# ---- 2. Dates: an edition speaks until the year before the next ------------------------------
yrs <- sort(unique(kyrkor$year))
ed_start <- setNames(c(1600L, yrs[-1]), yrs)                 # the first opens backwards
ed_end   <- setNames(c(yrs[-1] - 1L, 1990L), yrs)            # the last opens forwards
kyrkor <- kyrkor %>% mutate(start = unname(ed_start[as.character(year)]),
                            end   = unname(ed_end[as.character(year)]))

# ---- 3. Names to units ------------------------------------------------------------------------
# A parish is matched by name within its county where the volume prints one (1931), otherwise
# anywhere; a name that fits more than one parish unit in the same years is dropped, because the
# wrong Ekeby is worse than none (the fault m3s had, fixed on 27 September).
# The printed name and the unit's name differ in three ways: the unit carries its type word
# ("Närdinghundra" is printed, "Närdinghundra kontrakt" is the unit), the volume writes "och" where
# the register writes "&", and a parish unit has its county in brackets. The key removes all three.
# Uppsala's stift is an "ärkestift"; and a pastorat of several parishes is printed as their list
# while the register names it after its mother parish, so a pastorat is keyed on that parish.
nkey <- function(x){
  y <- tolower(trimws(gsub("\\s+", " ", x)))
  y <- gsub("\\s*\\([^)]*\\)", "", y)
  y <- gsub("&", " och ", y, fixed = TRUE)
  y <- gsub("[^a-z\u00e5\u00e4\u00f6 -]", " ", y)
  y <- gsub("\\b(o|och|med|samt)\\b", " ", y)
  y <- sub("\\s+(lands|stads|domkyrko)?f(\u00f6|o)rsamling(en)?$", "", y)
  y <- sub("\\s+(kontraktet|kontrakt|(\u00e4|a)rke-?stift|stiftet|stift|pastoratet|pastorat)$", "", y)
  y <- gsub("\\s*-\\s*", " ", y)
  # and the historical spellings, the genitive and the word order, so Elfkarleby meets Älvkarleby,
  # "Hjelmseryd" meets "Hjälmseryds församling" and "Södra Vadsbo" meets "Vadsbo södra kontrakt"
  # (fold_unit_key in model_helpers.R)
  fold_unit_key(trimws(gsub("\\s+", " ", y)))
}
units <- m1$units %>% mutate(ustart = as.integer(format(start_date, "%Y")),
                             uend = as.integer(format(end_date, "%Y")))
par_u <- units %>% filter(type_id == "parish") %>% transmute(unit_id, name, ustart, uend,
                                                            key = nkey(name))
con_u <- units %>% filter(type_id == "contract") %>% transmute(unit_id, name, ustart, uend,
                                                              key = nkey(name))
dio_u <- units %>% filter(type_id == "diocese") %>% transmute(unit_id, name, ustart, uend,
                                                              key = nkey(name))
pas_u <- units %>% filter(type_id == "pastorship") %>% transmute(unit_id, name, ustart, uend,
                                                                 key = nkey(name))

# the county of a parish unit, for the 1931 volume's county abbreviations
cmeta <- read.csv("data-raw/data/county_meta.csv") %>% transmute(cc = code, cname = name, letter)
par_county <- m1$units %>% filter(type_id == "parish") %>% transmute(unit_id, nm = name) %>%
  mutate(par_letter = str_match(nm, "\\(([A-Z]{1,2})-l(ä|a)n\\)")[, 2])

# the unique unit of that name whose life covers any of [s, e]; several candidates count as a miss,
# and the two kinds of miss are counted apart because they need different remedies
resolve <- function(keys, s, e, tbl, label){
  u <- character(length(keys)); n_amb <- 0L; n_none <- 0L
  for (i in seq_along(keys)) {
    hit <- tbl$unit_id[tbl$key == keys[i] & tbl$ustart <= e[i] & tbl$uend >= s[i]]
    if (length(hit) == 1) u[i] <- hit
    else { u[i] <- NA_character_
           if (length(hit) > 1) n_amb <- n_amb + 1L else n_none <- n_none + 1L }
  }
  message(sprintf("    %-9s matched %5d, ambiguous %4d, no unit of that name %4d",
                  label, sum(!is.na(u)), n_amb, n_none))
  u
}

k <- kyrkor %>% filter(!is.na(parish), nzchar(parish)) %>%
  group_by(year, kontrakt, pastorat) %>%
  mutate(mother = parish[which(is_mother)[1]]) %>% ungroup() %>%
  mutate(p_key = nkey(parish), k_key = nkey(kontrakt), s_key = nkey(stift),
         pa_key = nkey(coalesce(mother, parish)))
k$parish_unit   <- resolve(k$p_key,  k$start, k$end, par_u, "parish")
k$contract_unit <- resolve(k$k_key,  k$start, k$end, con_u, "kontrakt")
k$diocese_unit  <- resolve(k$s_key,  k$start, k$end, dio_u, "stift")
k$pastorat_unit <- resolve(k$pa_key, k$start, k$end, pas_u, "pastorat")
message(sprintf("  matched: parish %.0f%%, kontrakt %.0f%%, stift %.0f%%, pastorat %.0f%% of %d rows",
                100 * mean(!is.na(k$parish_unit)), 100 * mean(!is.na(k$contract_unit)),
                100 * mean(!is.na(k$diocese_unit)), 100 * mean(!is.na(k$pastorat_unit)), nrow(k)))

# ---- 4. Claims, atom by atom ------------------------------------------------------------------
atoms <- st_drop_geometry(m2$atoms) %>% select(atom_id, unit_id, a_start = start, a_end = end)
claim <- function(child_col, parent_col, parent_type){
  d <- k[!is.na(k[[child_col]]) & !is.na(k[[parent_col]]), ]
  if (!nrow(d)) return(NULL)
  d %>% transmute(child_unit = .data[[child_col]], parent_unit = .data[[parent_col]],
                  parent_type = parent_type, start, end) %>%
    inner_join(atoms, by = c("child_unit" = "unit_id"), relationship = "many-to-many") %>%
    mutate(s = pmax(start, a_start), e = pmin(end, a_end)) %>% filter(s <= e) %>%
    transmute(atom_id, child_unit, parent_unit, parent_type, start = s, end = e,
              source = "statskalender", dated = TRUE, share = NA_real_) %>%
    distinct()
}
evidence <- bind_rows(
  claim("parish_unit", "contract_unit", "contract"),
  claim("parish_unit", "pastorat_unit", "pastorship"),
  claim("parish_unit", "diocese_unit",  "diocese"))
print(as.data.frame(evidence %>% count(parent_type, name = "claims")), row.names = FALSE)

# ---- 5. Chain links between the higher units --------------------------------------------------
# kontrakt -> stift and pastorat -> kontrakt, for the parishes the name matching could not place:
# m3b can then reach a kontrakt through the pastorat the SFGT gives a parish.
chain <- bind_rows(
  k %>% filter(!is.na(pastorat_unit), !is.na(contract_unit)) %>%
    transmute(child_unit = pastorat_unit, parent_unit = contract_unit,
              parent_type = "contract", start, end),
  k %>% filter(!is.na(contract_unit), !is.na(diocese_unit)) %>%
    transmute(child_unit = contract_unit, parent_unit = diocese_unit,
              parent_type = "diocese", start, end)) %>%
  distinct() %>% mutate(src = "statskalender")
print(as.data.frame(chain %>% count(parent_type, name = "links")), row.names = FALSE)

report <- list(
  editions = kyrkor %>% group_by(year) %>%
    summarise(parish_rows = n(), kontrakt = n_distinct(na.omit(kontrakt)),
              pastorat = n_distinct(na.omit(pastorat)), stift = n_distinct(na.omit(stift)),
              .groups = "drop"),
  unmatched_kontrakt = k %>% filter(is.na(contract_unit), !is.na(kontrakt)) %>%
    count(year, kontrakt, sort = TRUE),
  unmatched_parish = k %>% filter(is.na(parish_unit)) %>% count(year, parish, sort = TRUE))
print(as.data.frame(report$editions), row.names = FALSE)
write_csv(report$unmatched_kontrakt, "data-raw/model/out/m3d_unmatched_kontrakt.csv")
write_csv(report$unmatched_parish, "data-raw/model/out/m3d_unmatched_parish.csv")
message(sprintf("  names with no unit: %d kontrakt, %d parish (out/m3d_unmatched_*.csv)",
                nrow(report$unmatched_kontrakt), nrow(report$unmatched_parish)))
timer_end(t0)

message("  --- m3d checks ---")
check(nrow(kyrkor) > 4000, sprintf("parish rows parsed (got %d)", nrow(kyrkor)))
check(sum(evidence$parent_type == "contract") > 2000,
      sprintf("kontrakt claims (got %d)", sum(evidence$parent_type == "contract")))
check(mean(!is.na(k$contract_unit)) > 0.65,
      sprintf("kontrakt names matched to a unit (%.0f%%)", 100 * mean(!is.na(k$contract_unit))))
check(all(evidence$start <= evidence$end), "every claim has start <= end")
saveRDS(list(kyrkor = kyrkor, evidence = evidence, chain = chain, report = report),
        "data-raw/model/out/m3d.rds")
message("  Saved data-raw/model/out/m3d.rds")
