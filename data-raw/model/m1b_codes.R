#' m1b: codes and dated names of parish units (and ref_code for every type)
#'
#' Model (DESIGN.md §2): identity is separate from the codes and names, each with its own
#' validity. A code is never taken from the unit a parish merged into: - ref_code / nadkod /
#' parse come from the unit's own source record (topo_id). Run from the package root: Rscript
#' data-raw/model/m1b_codes.R
#'
#' Input
#' data-raw/model/out/m1.rds                     m1 identity (records, units, unit_names)
#' data-raw/data/Tbl_topografi.csv               Riksarkivet referenskod per topo_id
#' data-raw/data/parish_medta.csv                SCB socken name per NAD code (own, per version)
#' data-raw/data/county_meta.csv                 county code -> letter
#' data-raw/pid_lookup.csv                       frozen pids (name, charid, forkod)
#' ../swe-parish/data/for_hist.rda               SFGT registry (names, namn history, county)
#' data-raw/sources/scb_codes.csv                SCB parish codes with validity
#' <rproj>/Samuel/reports/data/parish-changes-scb-1970-2018.csv   SCB renames (types 16, 30-33, 40)
#' ddb_catalogue.csv (not in the repo)          DDB dedik catalogue, see data-raw/sources/README.md
#' data-raw/sfgt/sfgt_indelning.rda              SFGT indelning events (renamed)
#' data/parish_registry.rda                      name_previous (fallback only)
#' Output  data-raw/model/out/m1b.rds = list(unit_codes, unit_names_extra, parish_link_compat,
#' match_report, new_pids, checks)
#' plus data-raw/model/out/pid_lookup_new.csv    (frozen lookup + the pids this step had to add)
#' plus data-raw/model/out/m1b_*.csv             match report and remaining conflicts

suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(sf)})
source("data-raw/model/model_helpers.R")
check <- function(condition, msg){
  if (!condition) stop("VALIDATION FAILED: ", msg, call. = FALSE)
  message("  OK: ", msg)
}
options(stringsAsFactors = FALSE)
rproj <- Sys.getenv("SWEHIST_RPROJ", "/home/rstudiojunkka/rproj")
say <- function(...) message(sprintf(...))
out_dir <- "data-raw/model/out"
t0 <- Sys.time()

DMIN <- as.Date("1600-01-01"); DMAX <- as.Date("1990-12-31")

# ── Name folding ─────────────────────────────────────────────────────────────
# Upper case, no parentheticals, no "församling" words, diacritics and w folded, letters only.
# "lands"/"stads" are KEPT: Hjo lands and Hjo stads are different parishes.
fold_base <- function(x){
  x <- toupper(as.character(x))
  x <- gsub("\\([^)]*\\)", " ", x)                       # "(M-län)", "(i Oxie härad)"
  x <- gsub(",\\s*DEL(\\s+AV)?\\b.*$", " ", x)              # "Skatelöv, del av ..." (needs the comma)
  x <- gsub("S:T ", "SANKT ", x, fixed = TRUE); x <- gsub("S:TA ", "SANKTA ", x, fixed = TRUE)
  # "församling" also as the tail of a word: bergsförsamling, stadsförs., landsförs.
  x <- gsub("(*UCP)FÖRSAMLING(EN|AR|S)?", " ", x, perl = TRUE)
  x <- gsub("(*UCP)FÖRS\\.", " ", x, perl = TRUE)
  x <- gsub("(*UCP)\\bFÖRS\\b", " ", x, perl = TRUE)
  x <- gsub("(*UCP)\\bKYRKOBOKFÖRINGSDISTRIKT\\b", " KBFD ", x, perl = TRUE)
  x <- chartr("ÅÄÖÉÜÈÆØÁÀ", "AAOEUEAOAA", x)
  x <- gsub("W", "V", x)
  x <- gsub("[^A-Z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}
fold_name <- function(x) gsub(" ", "", fold_base(x))
# Secondary key: the words of the name, without the joining words, each without its genitive s,
# sorted. "Domkyrkoförsamlingen i Göteborg" and "Göteborgs domkyrkoförsamling" give the same key,
# as do "Sankt Ilians" and "Sankt Ilian". It is only used where it is unique on both sides.
STOPW <- c("I", "OCH", "MED", "VID", "DEN", "DET", "DE", "AV")
fold_tokens <- function(x){
  vapply(strsplit(fold_base(x), " "), function(w){
    w <- setdiff(w, STOPW)
    w <- ifelse(nchar(w) >= 5 & endsWith(w, "S"), substr(w, 1, nchar(w) - 1), w)
    if (!length(w)) return("")
    paste(sort(unique(w)), collapse = "-")
  }, character(1))
}
# Non-territorial kinds: a parish unit and a registry entry must agree on this
nt_kind <- function(x){
  x <- tolower(x)
  case_when(grepl("kbfd|kyrkobokf", x) ~ "kbfd",
            grepl("garnison|regement|trängk|artilleri|livgarde|amiralitet", x) ~ "military",
            grepl("hospital|straff|fängelse|anstalt", x) ~ "institution",
            grepl("mosaisk|katolsk|metodist|rysk|finska|tyska förs", x) ~ "congregation",
            grepl("bruksförsamling|\\bbruks?\\b", x) ~ "bruk",
            grepl("slottsförsamling", x) ~ "slott",
            TRUE ~ "")
}

# ── 1. m1 identity ───────────────────────────────────────────────────────────
m1 <- readRDS(file.path(out_dir, "m1.rds"))
records <- st_drop_geometry(m1$records)
units <- m1$units
unames <- m1$unit_names
say("m1: %d units, %d records, %d source names", nrow(units), nrow(records), nrow(unames))

pu <- units %>% filter(type_id == "parish")
say("parish units: %d", nrow(pu))

# ── 2. ref_code, nadkod and parse from the source records (every type) ───────
topo <- read_csv("data-raw/data/Tbl_topografi.csv", show_col_types = FALSE) %>%
  select(topo_id, ref_code = referenskod)
rec_per <- records %>% group_by(unit_id, type_id, topo_id) %>%
  summarise(start = min(start), end = max(end), .groups = "drop") %>%
  left_join(topo, by = "topo_id")
# One source record has no row in Tbl_topografi (the Dalarnas län polygon, the second record
# of Kopparbergs län; correction I03): it gets no reference code.
say("source records without a Tbl_topografi row: %d (%s)", sum(is.na(rec_per$ref_code)),
    paste(rec_per$unit_id[is.na(rec_per$ref_code)], collapse = ", "))

code_ref <- rec_per %>% filter(!is.na(ref_code)) %>%
  transmute(unit_id, system = "ref_code", code = ref_code,
            start_date = year_start(start), end_date = year_end(end),
            precision = "year", source = "Riksarkivet topografi")

# NAD / PARSE codes: the 9 digits of the parish reference code. Suffix 000 = the record the
# 6-digit SCB code belongs to; a higher suffix is a parish that merged into it.
nad <- rec_per %>% filter(type_id == "parish", !is.na(ref_code)) %>%
  mutate(nad = sub("^SE/", "", ref_code), prefix = substr(nad, 1, 6), suffix = substr(nad, 7, 9),
         county_nad = as.integer(substr(nad, 1, 2)))
code_nad <- bind_rows(
  nad %>% transmute(unit_id, system = "nadkod", code = nad,
                    start_date = year_start(start), end_date = year_end(end),
                    precision = "year", source = "Riksarkivet topografi (NAD)"),
  nad %>% transmute(unit_id, system = "parse", code = nad,
                    start_date = year_start(start), end_date = year_end(end),
                    precision = "year", source = "Riksarkivet topografi (NAD) = NAPP PARSE"))

# Per unit: its own NAD codes, the county of its NAD group, whether it holds a suffix-000 record
pu_nad <- nad %>% group_by(unit_id) %>%
  summarise(nad_last = nad[which.max(end)], prefix_last = prefix[which.max(end)],
            own_prefix = any(suffix == "000"),
            prefixes = paste(sort(unique(prefix)), collapse = ";"),
            county_nad = county_nad[which.max(end)], .groups = "drop")
pu <- pu %>% left_join(pu_nad, by = "unit_id")
say("parish units with a suffix-000 record (own 6-digit SCB code): %d of %d",
    sum(pu$own_prefix), nrow(pu))

# ── 3. Names the unit is known by (for matching) ─────────────────────────────
medta <- read_csv("data-raw/data/parish_medta.csv", show_col_types = FALSE) %>%
  transmute(nad = sprintf("%09d", nadkod), name_scb = socken, from, tom)
nad_names <- nad %>% select(unit_id, nad, start, end) %>% left_join(medta, by = "nad") %>%
  filter(!is.na(name_scb))

unit_name_index <- bind_rows(
  unames %>% semi_join(pu, by = "unit_id") %>% transmute(unit_id, name, src = kind),
  nad_names %>% transmute(unit_id, name = name_scb, src = "scb_medta")) %>%
  mutate(key = fold_name(name), key2 = fold_tokens(name)) %>% filter(nzchar(key)) %>%
  distinct(unit_id, key, .keep_all = TRUE)
pu$key_last <- fold_name(pu$name)
pu$nt <- nt_kind(pu$name)

# ── 4. The frozen parish registry (SFGT) with its name history ───────────────
load(file.path("..", "swe-parish", "data", "for_hist.rda"))
pid_lookup <- read_csv("data-raw/pid_lookup.csv", col_types = "icci", na = "")
pid_key <- function(name, charid, forkod) paste(name, charid, as.integer(forkod), sep = "|")
frozen <- pid_lookup$pid[match(pid_key(for_hist$name, for_hist$charid, for_hist$forkod),
                               pid_key(pid_lookup$name, pid_lookup$charid, pid_lookup$forkod))]
if (anyDuplicated(frozen[!is.na(frozen)])) stop("duplicated frozen pids")
for_hist$pid <- frozen
# The saved for_hist has no `alias` column (the last step of swe-parish/parse_parish.R did not
# run): the alias-only entries are still rows of their own and have no frozen pid. They are
# name variants of another entry, reached through link1 -> charid (step7). When the target sits
# in another county, the alias belongs to the parish of the same name in the alias's own county.
alias_rows <- for_hist %>% filter(is.na(pid))
kept <- for_hist %>% filter(!is.na(pid))
if (!nrow(kept)) stop("no for_hist row matched the frozen pid lookup")
say("registry: %d entries with a frozen pid, %d alias-only entries", nrow(kept), nrow(alias_rows))
a_t <- alias_rows %>% select(a_name = name, link1, a_county = county) %>% filter(!is.na(link1)) %>%
  inner_join(kept %>% filter(!is.na(charid)) %>% select(charid, t_pid = pid, t_name = name,
                                                        t_county = county),
             by = c("link1" = "charid"), relationship = "many-to-many")
a_same <- a_t %>% filter(as.character(a_county) == as.character(t_county))
a_cross <- a_t %>% filter(!paste(a_name, link1) %in% paste(a_same$a_name, a_same$link1)) %>%
  inner_join(kept %>% select(t_name = name, a_county = county, c_pid = pid, c_forkod = forkod),
             by = c("t_name", "a_county"), relationship = "many-to-many") %>%
  group_by(a_name, link1) %>% filter(n() == 1 | c_forkod %% 100 != 0) %>% filter(n() == 1) %>%
  ungroup() %>% transmute(a_name, t_pid = c_pid)
aliases <- bind_rows(a_same %>% select(a_name, t_pid), a_cross) %>% distinct()
say("alias entries resolved to a pid: %d", nrow(aliases))
for_hist <- kept

reg <- for_hist %>%
  transmute(pid, name = ifelse(name == tolower(name),
                               paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name))), name),
            charid, forkod = as.numeric(forkod), county = as.integer(as.character(county)),
            namn, indelning) %>%
  left_join(aliases %>% group_by(t_pid) %>% summarise(alias = paste(sort(unique(a_name)), collapse = ", ")),
            by = c("pid" = "t_pid")) %>%
  mutate(county = ifelse(is.na(county) | county == 0, NA_integer_, county),
         forkod_real = !is.na(forkod) & forkod %% 100 != 0,
         nt = nt_kind(name))
say("registry: %d entries, %d with a real (non-placeholder) forkod", nrow(reg), sum(reg$forkod_real))

# ---- SFGT "namn": the dated name history --------------------------------------------------
# "-1885 Sallerup (i Oxie härad), 1885-04-17- Södra Sallerup." ->
#   Sallerup until 1885-04-16, Södra Sallerup from 1885-04-17.
# Only segments that carry a date, plus undated "förr/tidigare/äldre form X" as kind "former".
DATE_RE <- "\\d{4}(?:-\\d{2}(?:-\\d{2})?)?"
as_date_tok <- function(tok, end = FALSE){
  n <- nchar(tok)
  if (n == 4) return(if (end) year_end(tok) else year_start(tok))
  if (n == 7) {
    d <- as.Date(paste0(tok, "-01"))
    return(if (end) seq(d, by = "month", length.out = 2)[2] - 1 else d)
  }
  as.Date(tok)
}
tok_prec <- function(tok) if (nchar(tok) >= 10) "day" else "year"
BAD_NAME <- paste0("enligt|fastställ|ändra|stavning|beslut|jordebok|utbrut|uppgå|",
                   "\\bej\\b|kmt|kbr|\\bscb\\b|talet|omkring|troligen|\\d")
clean_name <- function(x){
  x <- gsub("\\([^)]*\\)", " ", x)
  x <- gsub("[.;:]+$", "", trimws(x))
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
# A capital inside a word with no space or hyphen around it is the SFGT key, not a name
# ("BarneAsaka" for Barne-Åsaka, "TrolleLjungby", "KrIstine"), and it became the registry's name
# for three parishes because it was their latest official name.
ok_name <- function(x) nzchar(x) && nchar(x) <= 60 && !grepl(BAD_NAME, x, ignore.case = TRUE) &&
  grepl("^[[:upper:]ÅÄÖ]", x) &&
  !grepl("(*UCP)\\p{Ll}\\p{Lu}", x, perl = TRUE)

parse_namn <- function(txt){
  empty <- tibble(name = character(), start_date = as.Date(character()),
                  end_date = as.Date(character()), precision = character(), kind = character())
  if (is.na(txt) || !nzchar(txt)) return(empty)
  s <- gsub("\\s+", " ", trimws(txt)); s <- sub("\\.$", "", s)
  out <- list()
  # "1902 ändrat/stavningsändrat ... från X till Y" (an official spelling change)
  m <- str_match(s, paste0("(", DATE_RE, ")[^,]*?ändra\\w*[^,]*? från ([^,]+?) till ([^,.]+)"))
  if (!is.na(m[1, 1])) {
    d <- as_date_tok(m[1, 2]); a <- clean_name(m[1, 3]); b <- clean_name(m[1, 4])
    if (ok_name(a)) out[[length(out) + 1]] <- tibble(name = a, start_date = as.Date(NA), end_date = d - 1,
                                                     precision = tok_prec(m[1, 2]), kind = "official")
    if (ok_name(b)) out[[length(out) + 1]] <- tibble(name = b, start_date = d, end_date = as.Date(NA),
                                                     precision = tok_prec(m[1, 2]), kind = "official")
  }
  segs <- trimws(strsplit(s, ",\\s*")[[1]])
  for (seg in segs) {
    r <- str_match(seg, paste0("^(", DATE_RE, ")-(", DATE_RE, ")\\s*(.+)$"))
    f <- str_match(seg, paste0("^(", DATE_RE, ")-\\s*(.+)$"))
    e <- str_match(seg, paste0("^-\\s*(", DATE_RE, ")\\s*(.+)$"))
    o <- str_match(seg, "^(?:förr|tidigare|tidigast|äldre form(?:er)?(?: bland andra)?|även|medeltiden|före \\d{4})\\s+(?:även\\s+)?(.+)$")
    if (!is.na(r[1, 1])) {
      nm <- clean_name(r[1, 4])
      if (ok_name(nm)) out[[length(out) + 1]] <-
        tibble(name = nm, start_date = as_date_tok(r[1, 2]), end_date = as_date_tok(r[1, 3], TRUE),
               precision = if (tok_prec(r[1, 2]) == "day") "day" else tok_prec(r[1, 3]), kind = "official")
    } else if (!is.na(f[1, 1])) {
      nm <- clean_name(f[1, 3])
      if (ok_name(nm)) out[[length(out) + 1]] <-
        tibble(name = nm, start_date = as_date_tok(f[1, 2]), end_date = as.Date(NA),
               precision = tok_prec(f[1, 2]), kind = "official")
    } else if (!is.na(e[1, 1])) {
      nm <- clean_name(e[1, 3])
      if (ok_name(nm)) out[[length(out) + 1]] <-
        tibble(name = nm, start_date = as.Date(NA), end_date = as_date_tok(e[1, 2], TRUE),
               precision = tok_prec(e[1, 2]), kind = "official")
    } else if (!is.na(o[1, 1])) {
      nm <- clean_name(o[1, 2])
      if (ok_name(nm)) out[[length(out) + 1]] <-
        tibble(name = nm, start_date = as.Date(NA), end_date = as.Date(NA),
               precision = "none", kind = "former")
    }
  }
  if (!length(out)) return(empty)
  x <- bind_rows(out)
  # chain the official segments: a segment without an end ends the day before the next starts
  off <- which(x$kind == "official")
  if (length(off) > 1) for (i in seq_len(length(off) - 1)) {
    a <- off[i]; b <- off[i + 1]
    if (!is.na(x$start_date[b]) && (is.na(x$end_date[a]) || x$end_date[a] >= x$start_date[b]))
      x$end_date[a] <- x$start_date[b] - 1
  }
  x
}
reg_names <- bind_rows(lapply(which(!is.na(reg$namn)), function(i){
  p <- parse_namn(reg$namn[i]); if (!nrow(p)) return(NULL); p$pid <- reg$pid[i]; p
}))
say("SFGT namn: %d entries parsed into %d dated/former names (%d entries with a name history)",
    sum(!is.na(reg$namn)), nrow(reg_names), n_distinct(reg_names$pid))

# ---- The registry's name index (for matching) ---------------------------------------------
reg_index <- bind_rows(
  reg %>% transmute(pid, name, src = "registry"),
  reg %>% filter(grepl(" eller ", name)) %>% tidyr::separate_rows(name, sep = " eller ") %>%
    transmute(pid, name, src = "registry_eller"),
  reg %>% filter(!is.na(alias)) %>% tidyr::separate_rows(alias, sep = ",\\s*") %>%
    transmute(pid, name = alias, src = "alias"),
  reg_names %>% transmute(pid, name, src = paste0("namn_", kind))) %>%
  filter(!is.na(name), nzchar(trimws(name))) %>%
  mutate(key = fold_name(name), key2 = fold_tokens(name)) %>% filter(nzchar(key)) %>%
  left_join(reg %>% select(pid, r_county = county, r_forkod = forkod, r_forkod_real = forkod_real,
                           r_nt = nt, r_name = name), by = "pid") %>%
  distinct(pid, key, .keep_all = TRUE)

# ── 5. Match parish units to frozen pids ─────────────────────────────────────
# Candidates by folded name; scored by county, code and which name matched. A pid may serve
# several units only when they do not coexist.
cand_raw <- bind_rows(
  unit_name_index %>% transmute(unit_id, k = key, u_src = src) %>%
    inner_join(reg_index %>% mutate(k = key), by = "k", relationship = "many-to-many") %>%
    mutate(key_kind = "k1"),
  unit_name_index %>% transmute(unit_id, k = key2, u_src = src) %>% filter(nzchar(k)) %>%
    inner_join(reg_index %>% mutate(k = key2), by = "k", relationship = "many-to-many") %>%
    mutate(key_kind = "k2") %>%
    group_by(k) %>% filter(n_distinct(unit_id) == 1, n_distinct(pid) == 1) %>% ungroup())
cand <- cand_raw %>%
  left_join(pu %>% select(unit_id, u_name = name, u_start = start_date, u_end = end_date,
                          county_nad, prefixes, own_prefix, key_last, nt), by = "unit_id") %>%
  filter(nt == r_nt) %>%
  mutate(code_hit = r_forkod_real & mapply(function(p, f)
           any(strsplit(p, ";")[[1]] == sprintf("%06d", as.integer(f))), prefixes, r_forkod),
         county_ok = !is.na(county_nad) & !is.na(r_county) & county_nad == r_county,
         county_unknown = is.na(r_county) | is.na(county_nad)) %>%
  group_by(key_kind, k) %>% mutate(unique_both = n_distinct(unit_id) == 1 & n_distinct(pid) == 1) %>%
  ungroup() %>%
  mutate(score = 4 * code_hit + 3 * county_ok + 1 * county_unknown + 2 * (key == key_last) +
           1 * (u_src == "official") + 1 * (src == "registry") + 1 * (key_kind == "k1")) %>%
  filter(county_ok | county_unknown | code_hit | unique_both)
say("pid candidates: %d pairs for %d of %d parish units",
    nrow(cand), n_distinct(cand$unit_id), nrow(pu))

# Greedy assignment, best score first
ord <- cand %>% arrange(desc(score), unit_id, pid)
assign_pid <- rep(NA_integer_, nrow(pu)); names(assign_pid) <- pu$unit_id
pid_units <- new.env(parent = emptyenv())
u_start <- setNames(pu$start_date, pu$unit_id); u_end <- setNames(pu$end_date, pu$unit_id)
coexists <- function(a, b) u_start[a] <= u_end[b] && u_start[b] <= u_end[a]
for (i in seq_len(nrow(ord))) {
  ui <- ord$unit_id[i]; p <- as.character(ord$pid[i])
  if (!is.na(assign_pid[[ui]])) next
  held <- if (exists(p, envir = pid_units)) get(p, envir = pid_units) else character()
  if (length(held) && any(vapply(held, function(h) coexists(ui, h), logical(1)))) next
  assign_pid[[ui]] <- ord$pid[i]
  assign(p, c(held, ui), envir = pid_units)
}
pu$pid <- unname(assign_pid[pu$unit_id])
matched_how <- ord %>% filter(paste(unit_id, pid) %in% paste(pu$unit_id, pu$pid)) %>%
  group_by(unit_id) %>% slice(1) %>% ungroup() %>%
  select(unit_id, pid, match_key = k, key_kind, match_name = name, match_src = src, u_src, score,
         code_hit, county_ok)
say("pids assigned from the frozen registry: %d of %d parish units", sum(!is.na(pu$pid)), nrow(pu))

# New pids for units the registry does not hold (appended only to the copy under out/)
new_pids <- pu %>% filter(is.na(pid)) %>%
  transmute(unit_id, name, start_date, end_date, county = county_nad, nad = nad_last)
if (nrow(new_pids)) {
  next_pid <- max(pid_lookup$pid) + seq_len(nrow(new_pids))
  new_pids$pid <- next_pid
  pu$pid[match(new_pids$unit_id, pu$unit_id)] <- new_pids$pid
}
say("new pids needed: %d", nrow(new_pids))

# The names the registry entry carried (SFGT namn) are names of the unit too: the code lists
# and the DDB catalogue often use the older name (DDB has Aringsås, not Alvesta; Västra
# Sallerup, not Eslöv), so they are added to the index before the codes are matched.
sfgt_unit_names <- reg_names %>% inner_join(pu %>% select(pid, unit_id, u_start = start_date,
                                                          u_end = end_date), by = "pid",
                                            relationship = "many-to-many") %>%
  filter(coalesce(start_date, u_start) <= u_end, u_start <= coalesce(end_date, u_end)) %>%
  transmute(unit_id, name, src = "sfgt_namn")
unit_name_index <- bind_rows(unit_name_index, sfgt_unit_names %>%
                               mutate(key = fold_name(name), key2 = fold_tokens(name))) %>%
  filter(nzchar(key)) %>% distinct(unit_id, key, .keep_all = TRUE)
say("name index for the code matching: %d names on %d units (%d from the SFGT name history)",
    nrow(unit_name_index), n_distinct(unit_name_index$unit_id), nrow(sfgt_unit_names))

# ── 6. forkod: the SCB code lists 1952-1990 ──────────────────────────────────
chg_f <- source_file("parish-changes-scb-1970-2018.csv")
chg <- if (!is.na(chg_f)) {
  read_csv(chg_f, col_types = cols(.default = "c")) %>%
    setNames(c("old", "old_name", "new", "new_name", "date", "type", "change")) %>%
    mutate(date = as.Date(date))
} else tibble(old = character(), old_name = character(), new = character(), new_name = character(),
              date = as.Date(character()), type = character(), change = character())
scb_f <- source_file("scb_codes.csv")
if (is.na(scb_f)) stop("scb_codes.csv not found. It is a build input: see data-raw/sources/README.md",
                       call. = FALSE)
scb <- read_csv(scb_f,
                col_types = cols(code = "c", from = "D", to = "D", name = "c", split = "l",
                                 county = "c", municipality = "c", code_1990 = "c")) %>%
  filter(from <= DMAX) %>%
  mutate(cc = as.integer(county), key = fold_name(name))
scb_m <- scb %>% mutate(key2 = fold_tokens(name)) %>%
  select(code, from, to, s_name = name, cc, key, key2)
cand_scb <- bind_rows(
  unit_name_index %>% transmute(unit_id, k = key) %>%
    inner_join(scb_m %>% mutate(k = key), by = "k", relationship = "many-to-many") %>%
    mutate(key_kind = "k1"),
  unit_name_index %>% transmute(unit_id, k = key2) %>% filter(nzchar(k)) %>%
    inner_join(scb_m %>% mutate(k = key2), by = "k", relationship = "many-to-many") %>%
    mutate(key_kind = "k2") %>%
    group_by(k) %>% filter(n_distinct(unit_id) == 1, n_distinct(code) == 1) %>% ungroup()) %>%
  distinct(unit_id, code, from, .keep_all = TRUE) %>%
  left_join(pu %>% select(unit_id, pid, u_name = name, u_start = start_date, u_end = end_date,
                          county_nad, prefixes, own_prefix), by = "unit_id") %>%
  mutate(from_c = pmax(from, u_start),
         to_c = pmin(if_else(is.na(to), DMAX, to - 1), u_end, DMAX)) %>%
  filter(from_c <= to_c) %>%
  mutate(code_hit = mapply(function(p, c) any(strsplit(p, ";")[[1]] == c), prefixes, code),
         county_ok = !is.na(county_nad) & cc == county_nad,
         ovl = as.numeric(to_c - from_c))
# One code interval may fit several units of the same name and county (successive versions):
# keep the unit whose own NAD prefix is that code, else the one whose period overlaps most.
pick_interval <- function(d) d %>% group_by(code, from) %>%
  filter(if (any(code_hit)) code_hit else TRUE) %>%
  filter(if (any(key_kind == "k1")) key_kind == "k1" else TRUE) %>%
  filter(ovl == max(ovl)) %>% slice(1) %>% ungroup()
m_scb1 <- pick_interval(cand_scb %>% filter(county_ok))
# Second pass, for the codes a parish had before it changed county: the SCB county is the
# county of that period, the NAD code carries the county of 1974+, so the two differ
# (Knutby 1968-70 in Stockholm county, from 1971 in Uppsala). Only where the name belongs to
# one unit and one code, and the unit holds no other code then.
clash2 <- cand_scb %>% filter(!county_ok) %>% inner_join(m_scb1 %>% select(unit_id, o_from = from_c,
                                                                          o_to = to_c),
                                                         by = "unit_id", relationship = "many-to-many") %>%
  filter(from_c <= o_to, o_from <= to_c) %>% distinct(unit_id, code, from)
m_scb2 <- cand_scb %>% filter(!county_ok, !paste(code, from) %in% paste(m_scb1$code, m_scb1$from)) %>%
  group_by(k) %>% filter(n_distinct(unit_id) == 1, n_distinct(code) == 1) %>% ungroup() %>%
  anti_join(clash2, by = c("unit_id", "code", "from")) %>% pick_interval() %>%
  mutate(county_changed = TRUE)
say("forkod: %d intervals matched by name and county, %d more for parishes that changed county",
    nrow(m_scb1), nrow(m_scb2))
m_scb <- bind_rows(m_scb1, m_scb2)
code_forkod <- m_scb %>%
  transmute(unit_id, system = "forkod", code, start_date = from_c, end_date = to_c,
            precision = "day", source = "SCB parish code lists 1952-1990")
say("forkod: %d dated SCB codes for %d parish units", nrow(code_forkod), n_distinct(code_forkod$unit_id))

# Units with no SCB code but their own suffix-000 record: the NAD prefix is their own code
# (undated). Units with a higher suffix never get the 6-digit prefix: it is the survivor's.
scb_all_codes <- unique(c(scb$code, chg$old, chg$new))
no_scb <- setdiff(pu$unit_id[pu$own_prefix], code_forkod$unit_id)
# The codes here are ONE VINTAGE: the register's forkod is the code a parish had last, in or after
# 1974. The 1930 census uses the scheme of its own day, in which the rural parish of a stad has
# parish part 00 and the stad itself 01 -- 158500 is Amals landsforsamling and 158501 is Amal -- and
# the register has neither: its code for Amals landsforsamling is 158501, the 1974 code of the unit
# that absorbed it. So 7 of the 1,337 census codes in the downstream regression no longer resolve
# (1.1.1 carried 72 such codes on territorial parishes, 6 of these 7 rightly, but others on two
# parishes at once: Eksjo lands and Eksjo stads both had 68600). The parishes are still found by
# name, at the same rate as before. The fix is the dated code history that DESIGN.md calls for, from
# SCB's own lists per year; deriving MMMM00 from the census instead would make the census a build
# input, and it is one of the instruments the build is measured with.
# A code whose parish part is 7x is the total of a parish with kyrkobokföringsdistrikt, and the
# rule was to keep it only when SCB's lists have it. But the model has no kbfd units of its own for
# it to clash with, and 28 parishes alive in 1967 or later have no other code at all: Piteå
# landsförsamling is one, and 258171 is the code a user of the 1930 census has for it (9 of the 27
# lookups the downstream regression lost). So a 7x total is kept when it is the unit's only code.
code_forkod_nad <- pu %>% filter(unit_id %in% no_scb) %>%
  mutate(n = as.numeric(prefix_last),
         kbfd_total = n %% 100 >= 70 & !prefix_last %in% scb_all_codes) %>%
  filter(n %% 100 != 0) %>%
  transmute(unit_id, system = "forkod", code = prefix_last, start_date, end_date,
            precision = "none",
            source = ifelse(kbfd_total,
              "Riksarkivet NAD reference code (the parish's kyrkobokföringsdistrikt total, undated)",
              "Riksarkivet NAD reference code (6 digits, undated)"))
say("forkod from the NAD prefix (no SCB code, own record): %d", nrow(code_forkod_nad))
say("parish units with no forkod at all: %d (existed only before the SCB lists, or merged away)",
    nrow(pu) - n_distinct(c(code_forkod$unit_id, code_forkod_nad$unit_id)))

# ---- The codes of one registry entry hold for all its versions ---------------------------
# A pid never sits on two coexisting units, so a code matched to one version of a pid is that
# registry entry's own code. Versions that matched nothing (their historical name is not the
# name the code list uses: "Alingsås stadsförsamling" for the code named "Alingsås") take it
# over for the part of their lifetime the code was valid, unless a unit of another pid holds
# the same code then.
pool_codes <- function(have, windows, note, one_per_unit = FALSE){
  u <- pu %>% select(unit_id, pid, u_start = start_date, u_end = end_date)
  held <- have %>% inner_join(u %>% select(unit_id, pid), by = "unit_id")
  add <- u %>% filter(!unit_id %in% have$unit_id) %>%
    inner_join(windows %>% inner_join(u %>% select(unit_id, pid), by = "unit_id") %>%
                 select(pid, code, w_start, w_end, src_unit = unit_id),
               by = "pid", relationship = "many-to-many") %>%
    filter(unit_id != src_unit) %>%
    mutate(start_date = pmax(w_start, u_start, DMIN), end_date = pmin(w_end, u_end, DMAX)) %>%
    filter(start_date <= end_date)
  if (!nrow(add)) return(add[0, c("unit_id", "code", "start_date", "end_date")])
  # no other pid's unit may hold the same code at the same time
  clash <- add %>% inner_join(held %>% select(o_unit = unit_id, o_pid = pid, code,
                                              o_start = start_date, o_end = end_date),
                              by = "code", relationship = "many-to-many") %>%
    filter(o_pid != pid, start_date <= o_end, o_start <= end_date) %>% distinct(unit_id, code)
  add <- add %>% anti_join(clash, by = c("unit_id", "code")) %>%
    left_join(u %>% select(src_unit = unit_id, s_start = u_start, s_end = u_end), by = "src_unit") %>%
    mutate(gap = pmax(0, pmin(as.numeric(s_start - u_end), as.numeric(u_start - s_end)))) %>%
    group_by(unit_id, code) %>% arrange(gap, start_date, .by_group = TRUE) %>% slice(1) %>% ungroup()
  if (one_per_unit) add <- add %>% group_by(unit_id) %>% arrange(gap, start_date, .by_group = TRUE) %>%
    slice(1) %>% ungroup()
  say("  %s: %d code periods carried to %d versions of the same pid", note, nrow(add),
      n_distinct(add$unit_id))
  # A version that ended before the code was ever valid gets it for its own lifetime, undated:
  # SCB's lists start in 1952, so "Alingsås församling -1618" shares its registry entry with
  # "Alingsås stadsförsamling 1619-1966" and its code, but no window can overlap it. The code
  # identifies the registry entry, not the period, and a user with a code and an early date has
  # nothing else to go on; the interval is the version's own life and the precision says "none".
  outside <- u %>% filter(!unit_id %in% c(have$unit_id, add$unit_id)) %>%
    inner_join(windows %>% inner_join(u %>% select(unit_id, pid), by = "unit_id") %>%
                 select(pid, code, src_unit = unit_id) %>% distinct(),
               by = "pid", relationship = "many-to-many") %>%
    filter(unit_id != src_unit) %>%
    transmute(unit_id, code, start_date = u_start, end_date = u_end, whole_life = TRUE)
  if (nrow(outside)) {
    clash2 <- outside %>% inner_join(held %>% select(o_pid = pid, code, o_start = start_date,
                                                     o_end = end_date),
                                     by = "code", relationship = "many-to-many") %>%
      inner_join(u %>% select(unit_id, pid), by = "unit_id") %>%
      filter(o_pid != pid, start_date <= o_end, o_start <= end_date) %>% distinct(unit_id, code)
    outside <- outside %>% anti_join(clash2, by = c("unit_id", "code")) %>%
      group_by(unit_id) %>% arrange(code, .by_group = TRUE) %>% slice(1) %>% ungroup()
    say("  %s: %d versions that ended before the code's own dates take it undated", note,
        nrow(outside))
    add <- bind_rows(add, outside)
  }
  if (!"whole_life" %in% names(add)) add$whole_life <- FALSE
  add %>% mutate(whole_life = coalesce(whole_life, FALSE)) %>%
    select(unit_id, code, start_date, end_date, whole_life)
}
fk_windows <- m_scb %>% mutate(w_start = from, w_end = if_else(is.na(to), DMAX, to - 1)) %>%
  select(unit_id, code, w_start, w_end)
fk_pool <- pool_codes(code_forkod, fk_windows, "forkod") %>%
  mutate(system = "forkod",
         precision = ifelse(whole_life, "none", "day"),
         source = ifelse(whole_life,
           "SCB parish code list (the registry entry's code, before the lists begin: undated)",
           "SCB parish code lists 1952-1990 (another version of the same registry entry)")) %>%
  select(-whole_life)
forkod_all <- bind_rows(code_forkod, code_forkod_nad, fk_pool)
# Codes added only so that match_units() finds the parish: they are not the parish's own codes, so
# they bring no names (unit_names_extra) and no "recoded" events (m5)
LOOKUP_ONLY <- "kyrkobokföringsdistrikt\\)|carried by pid|^correction:|matched by name within"
# SCB's lists also code a parish's kyrkobokföringsdistrikt ("Spånga kbfd" 018041 from 1976), which
# the model has no unit for. A user with that code means the parish it lies in, so the code goes to
# the parish of the same name in the same county, when exactly one fits and no unit holds the code
# then (user-path tests: Folkrörelsearkivet's 18041 for Spånga found nothing).
kb <- scb %>% filter(grepl("kbfd", name, ignore.case = TRUE)) %>%
  mutate(base = fold_name(sub("(?i)\\s*kbfd.*$", "", name, perl = TRUE))) %>%
  filter(!paste(code, from) %in% paste(m_scb$code, m_scb$from), nzchar(base)) %>%
  inner_join(unit_name_index %>% distinct(unit_id, base = key), by = "base", relationship = "many-to-many") %>%
  inner_join(pu %>% select(unit_id, county_nad, u_start = start_date, u_end = end_date), by = "unit_id") %>%
  filter(!is.na(county_nad), cc == county_nad) %>%
  mutate(start_date = pmax(from, u_start), end_date = pmin(if_else(is.na(to), DMAX, to - 1), u_end, DMAX)) %>%
  filter(start_date <= end_date) %>%
  group_by(code, from) %>% filter(n_distinct(unit_id) == 1) %>% ungroup()
held_fk <- forkod_all %>% select(o_unit = unit_id, code, o_s = start_date, o_e = end_date)
kb <- kb %>% left_join(held_fk, by = "code", relationship = "many-to-many") %>%
  group_by(unit_id, code, from) %>%
  filter(!any(!is.na(o_unit) & o_unit != unit_id & o_s <= end_date & start_date <= o_e)) %>%
  slice(1) %>% ungroup() %>%
  transmute(unit_id, system = "forkod", code, start_date, end_date, precision = "day",
            source = "SCB parish code lists 1952-1990 (the parish's kyrkobokföringsdistrikt)")
say("forkod: %d SCB codes of a kyrkobokföringsdistrikt given to its parish", nrow(kb))
forkod_all <- bind_rows(forkod_all, kb)
# Skatteverket's registry (SFGT) gives the rural parish of a town its code of the old scheme, with
# parish part 00: Vimmerby landsförsamling 088400, Åmåls landsförsamling 158500, Sölvesborgs
# landsförsamling 108300. The SCB lists of 1952- do not carry these, so the parishes had no forkod
# and the 1930 census codes for them found nothing. A 00 code is
# taken from the registry only where one registry entry carries it and that entry is a parish of
# its own (not a kbfd, garrison, ...): 38100 sits on seven entries and stays out. A part-00 code is
# nonetheless a placeholder at municipality level that the sources fill differently (048600 is
# Strängnäs landsförsamling here and Kärnbo in the 1930 census; 168300 Skövde landsförsamling,
# dissolved 1915, and Öm), so match_units() flags every match on one.
code_forkod_reg <- reg %>% filter(!is.na(forkod), forkod > 0, forkod %% 100 == 0, nt == "") %>%
  # one entry, or one landsförsamling among them (158500: Åmåls landsförsamling, and Hässelskog and
  # Hesselskog, two spellings of a name the registry files under the same code)
  group_by(forkod) %>%
  filter(n_distinct(pid) == 1 |
           (sum(grepl("lands", name, ignore.case = TRUE)) == 1 & grepl("lands", name, ignore.case = TRUE))) %>%
  ungroup() %>%
  inner_join(pu %>% select(unit_id, pid, start_date, end_date), by = "pid") %>%
  mutate(code = sprintf("%06d", as.integer(forkod))) %>%
  filter(!code %in% sprintf("%06d", as.integer(forkod_all$code))) %>%
  transmute(unit_id, system = "forkod", code, start_date, end_date, precision = "none",
            source = "Skatteverket parish registry (SFGT): code of the old scheme, parish part 00")
say("forkod: %d codes with parish part 00 from the registry (one entry each)", nrow(code_forkod_reg))
forkod_all <- bind_rows(forkod_all, code_forkod_reg)
# The registry carries one forkod per unit: the latest, but never a code ending in 00 while the
# unit has one of its own, because a 00 code is the municipality's total and the registry would be
# presenting it as the parish's own (b5_placeholder_codes). unit_codes keeps it either way, so a
# user with "38100" for Arnö still finds the parish (it cost 18 downstream matches when the code
# was dropped from the pool instead).
forkod_last <- forkod_all %>% group_by(unit_id) %>%
  mutate(own = !grepl("00$", code)) %>%
  arrange(desc(own), desc(end_date), .by_group = TRUE) %>% slice(1) %>% ungroup() %>%
  transmute(unit_id, forkod = as.numeric(code), forkod_src = precision)

# ── 7. dedik: re-keyed from the DDB catalogue ────────────────────────────────
# The DDB code catalogue is not redistributable, so it is not in the repo (data-raw/README.md
# says where to get it). Without it the dedik codes fall back to the registry, which is the 1990
# code only, and the build says so rather than passing it over.
ddb_f <- source_file("ddb_catalogue.csv")
if (is.na(ddb_f)) message("  NOTE: no DDB catalogue; dedik codes stay undated (see data-raw/README.md)")
ddb <- (if (is.na(ddb_f)) tibble(dedik = integer(), ddb_name_raw = character(), level = character(),
                                 ddb_type = character(), scb74 = numeric(), county = character())
        else read_csv(ddb_f, col_types = cols(.default = "c"))) %>%
  mutate(dedik = as.integer(dedik), scb74 = as.numeric(scb74),
         cc = suppressWarnings(as.integer(county)), key = fold_name(ddb_name_raw)) %>%
  filter(level %in% c("LF", "SF"), ddb_type == "0", nzchar(key))
ddb_m <- ddb %>% mutate(key2 = fold_tokens(ddb_name_raw)) %>%
  select(dedik, ddb_name, key, key2, cc, scb74)
cand_ddb <- bind_rows(
  unit_name_index %>% transmute(unit_id, k = key) %>%
    inner_join(ddb_m %>% mutate(k = key), by = "k", relationship = "many-to-many") %>%
    mutate(key_kind = "k1"),
  unit_name_index %>% transmute(unit_id, k = key2) %>% filter(nzchar(k)) %>%
    inner_join(ddb_m %>% mutate(k = key2), by = "k", relationship = "many-to-many") %>%
    mutate(key_kind = "k2") %>%
    group_by(k) %>% filter(n_distinct(unit_id) == 1, n_distinct(dedik) == 1) %>% ungroup()) %>%
  distinct(unit_id, dedik, .keep_all = TRUE) %>%
  left_join(pu %>% select(unit_id, pid, u_name = name, u_start = start_date, u_end = end_date,
                          county_nad, prefixes), by = "unit_id") %>%
  mutate(county_ok = !is.na(county_nad) & !is.na(cc) & cc == county_nad)
# county_ok first; a name that belongs to one unit and one dedik is taken even when the DDB
# county (from SCB74, i.e. 1974) differs from the NAD county
m_ddb <- bind_rows(cand_ddb %>% filter(county_ok),
                   cand_ddb %>% filter(!county_ok) %>% group_by(k) %>%
                     filter(n_distinct(unit_id) == 1, n_distinct(dedik) == 1) %>% ungroup() %>%
                     filter(!unit_id %in% cand_ddb$unit_id[cand_ddb$county_ok])) %>%
  mutate(code_hit = mapply(function(p, s) !is.na(s) && s > 0 &&
                             any(strsplit(p, ";")[[1]] == sprintf("%06d", as.integer(s))),
                           prefixes, scb74),
         trail0 = nchar(dedik) - nchar(sub("0+$", "", as.character(dedik))))
ded_u <- m_ddb %>% group_by(unit_id) %>% filter(if (any(county_ok)) county_ok else TRUE) %>% ungroup()
# A unit keeps EVERY catalogue entry its names match, not one. DDB gives a parish several codes
# over time (Byske 82983 and, under its earlier name, 82990; Vikingstad 56820 "med Rakeryd" and
# 56822), and an extract uses the one valid at its date: keeping one lost POPLINK's Byske, Lunds
# domkyrkoförsamling and 70 more. Where some entries' SCB74 is
# the unit's own code, the others are not its own. The primary code (parish_link$dedik) is the
# entry named like the unit's own last name, then the one with most trailing zeros.
ded_u <- ded_u %>% group_by(unit_id) %>%
  filter(if (any(code_hit)) code_hit else TRUE) %>%
  distinct(unit_id, dedik, .keep_all = TRUE) %>% ungroup() %>%
  left_join(pu %>% select(unit_id, key_last), by = "unit_id") %>%
  mutate(own_name = fold_name(ddb_name) == key_last) %>%
  group_by(unit_id) %>% arrange(desc(own_name), desc(trail0), dedik, .by_group = TRUE) %>%
  mutate(primary = row_number() == 1) %>% ungroup() %>% select(-key_last, -own_name)
# One dedik: it must not land on two units that coexist; prefer the unit the DDB name spells
# (GRÄNNA STADS is Gränna stadsförsamling 1652-1962, not the Gränna församling around it that
# spans 1600-1990), then the SCB74 hit, then the longer life
ded_u <- ded_u %>% mutate(life = as.numeric(u_end - u_start)) %>%
  left_join(unit_name_index %>% distinct(unit_id, key) %>% mutate(spelled = TRUE),
            by = c("unit_id", "key")) %>%
  mutate(spelled = coalesce(spelled, FALSE),
         own = fold_name(u_name) == key) %>%     # its own (source) name, not one it inherited
  arrange(dedik, desc(own), desc(spelled), desc(code_hit), desc(life))
keep <- rep(TRUE, nrow(ded_u))
for (d in unique(ded_u$dedik[duplicated(ded_u$dedik)])) {
  idx <- which(ded_u$dedik == d)
  for (j in seq_along(idx)[-1]) {
    i <- idx[j]; prev <- idx[seq_len(j - 1)][keep[idx[seq_len(j - 1)]]]
    if (any(ded_u$u_start[i] <= ded_u$u_end[prev] & ded_u$u_start[prev] <= ded_u$u_end[i]))
      keep[i] <- FALSE
  }
}
dropped_dedik <- ded_u[!keep, ]
ded_u <- ded_u[keep, ]
ded_u <- ded_u %>% group_by(unit_id) %>% mutate(primary = primary | !any(primary) & row_number() == 1) %>%
  ungroup()

# Second pass, for the catalogue's ordinary parishes that no name reached: the catalogue spells
# them its own way ("SANKT KATARINA, STOCKHOLM", "LINKÖPINGS DOMKYRKFÖRS."), and these were the
# largest losses (Stockholm's city parishes, Linköping, Lund, Västerås). Candidates are the units
# of the code's SCB74 group (the 1974 parish it lies in: the unit's own NAD prefix or forkod), else
# of its county; a candidate fits when all words of one of its names are words of the DDB name, or
# the two folded names are within 15% of each other. Taken only where exactly one unit fits.
ded_left <- ddb %>% filter(!dedik %in% ded_u$dedik, !dedik %in% dropped_dedik$dedik) %>%
  # "DOMKYRKOFÖRS", "DOMKYRKFÖRS.": the catalogue's abbreviation of församling, glued to the word
  mutate(ddb_name_raw = gsub("(*UCP)(F\u00d6RS)(\\.|\\b)", " ", ddb_name_raw, perl = TRUE),
         toks = strsplit(fold_tokens(ddb_name_raw), "-"), s74 = sprintf("%06d", as.integer(scb74)))
unit_codes6 <- bind_rows(nad %>% transmute(unit_id, c6 = prefix),
                         forkod_all %>% transmute(unit_id, c6 = sprintf("%06d", as.integer(code)))) %>%
  distinct()
fits <- function(u_names, d_raw, d_toks) {
  any(vapply(u_names, function(n) {
    ut <- strsplit(fold_tokens(n), "-")[[1]]
    (length(ut) && nzchar(ut[1]) && all(ut %in% d_toks)) ||
      utils::adist(fold_name(n), fold_name(d_raw)) <= 0.15 * max(nchar(fold_name(n)), 1)
  }, logical(1)))
}
names_of <- split(unit_name_index$name, unit_name_index$unit_id)
ded_grp <- bind_rows(lapply(seq_len(nrow(ded_left)), function(i) {
  d <- ded_left[i, ]
  g <- unique(unit_codes6$unit_id[unit_codes6$c6 == d$s74])
  how <- "SCB74 group"
  if (!length(g) || is.na(d$scb74) || d$scb74 <= 0) {
    g <- pu$unit_id[!is.na(pu$county_nad) & !is.na(d$cc) & pu$county_nad == d$cc]; how <- "county"
  }
  ok <- g[vapply(g, function(u) fits(names_of[[u]] %||% character(), d$ddb_name_raw, d$toks[[1]]),
                 logical(1))]
  if (length(ok) != 1) return(NULL)
  tibble(unit_id = ok, dedik = d$dedik, ddb_name = d$ddb_name, how = how)
})) %>%
  left_join(pu %>% select(unit_id, pid, u_start = start_date, u_end = end_date), by = "unit_id") %>%
  mutate(primary = !unit_id %in% ded_u$unit_id)
ded_grp <- ded_grp %>% group_by(unit_id) %>% mutate(primary = primary & row_number() == 1) %>% ungroup()
say("dedik: %d more catalogue parishes matched within their SCB74 group or county (%d by group)",
    nrow(ded_grp), sum(ded_grp$how == "SCB74 group"))
ded_u <- bind_rows(ded_u %>% mutate(how = "name"), ded_grp)
code_dedik <- ded_u %>%
  transmute(unit_id, system = "dedik", code = as.character(dedik),
            start_date = u_start, end_date = u_end, precision = "none",
            source = ifelse(how == "name",
                            "DDB parish catalogue (KOD.DEDIKKATALOG), re-keyed by name and SCB74",
                            paste0("DDB parish catalogue, matched by name within its ", how)))
ded_windows <- ded_u %>% transmute(unit_id, code = as.character(dedik), w_start = DMIN, w_end = DMAX)
ded_pool <- pool_codes(code_dedik, ded_windows, "dedik", one_per_unit = FALSE) %>%
  mutate(system = "dedik", precision = "none",
         source = "DDB parish catalogue (another version of the same registry entry)") %>%
  select(-any_of("whole_life"))
code_dedik <- bind_rows(code_dedik, ded_pool)
say("dedik: %d parish units matched in the DDB catalogue (%d candidates dropped as a coexisting duplicate)",
    nrow(code_dedik), nrow(dropped_dedik))

# ---- The earlier build's registry codes, by pid -----------------------------------------------
# The registry of the earlier build (sources/parish_registry_111.csv: Skatteverket's codes and parish_medta's
# dedik) gives some parishes a code that neither the SCB lists nor the DDB catalogue reach by name:
# Älvsby 83050 ("ÄLVSBYN" in the catalogue), Vissefjärda 79220, Svanskog 81010, Sorunda 041304
# (its code before it moved to Stockholms län). The user-path tests found 66 such codes that the earlier build
# answered rightly and this build lost. They are carried by pid, which is frozen, so no name is
# matched: only codes on one territorial entry there, held by no unit here, never a part-00 forkod.
r111_f <- source_file("parish_registry_111.csv")
code_r111 <- tibble(unit_id = character(), system = character(), code = character(),
                    start_date = as.Date(character()), end_date = as.Date(character()),
                    precision = character(), source = character())
if (!is.na(r111_f)) {
  r111 <- read_csv(r111_f, show_col_types = FALSE)
  take <- function(sys, fmt) {
    held <- c(forkod = list(as.numeric(forkod_all$code)),
              # a code dropped above because its name fit two parishes is eligible: the registry
              # settles that by pid (Hanebo 53510)
              dedik = list(as.numeric(code_dedik$code)))[[sys]]
    r111 %>% filter(category %in% "territorial", !is.na(.data[[sys]]), .data[[sys]] > 0) %>%
      transmute(pid, v = as.numeric(.data[[sys]])) %>% distinct() %>%
      group_by(v) %>% filter(n_distinct(pid) == 1) %>% ungroup() %>%
      filter(!v %in% held, !(sys == "forkod" & v %% 100 == 0)) %>%
      inner_join(pu %>% select(unit_id, pid, start_date, end_date), by = "pid") %>%
      transmute(unit_id, system = sys, code = fmt(v), start_date, end_date, precision = "none",
                source = "parish registry of the earlier build (Skatteverket / parish_medta), carried by pid")
  }
  r111_dedik <- take("dedik", function(v) as.character(as.integer(v)))
  # A dedik the DDB catalogue has must carry a catalogue name that fits a name of the parish:
  # parish_medta's raw values include codes of another parish (50010 UPPSALA, Uppsala
  # landsförsamling, on Helga Trefaldighet), which the re-key had corrected and the carry would
  # bring back. A code the catalogue copy lacks is carried: real data use some of them.
  if (!is.na(ddb_f)) {
    cat_all <- read_csv(ddb_f, col_types = cols(.default = "c")) %>%
      transmute(code = as.character(as.integer(dedik)), ddb_name_raw)
    r111_dedik <- r111_dedik %>% left_join(cat_all, by = "code") %>%
      filter(mapply(function(u, nm) {
        if (is.na(nm)) return(TRUE)   # not in this copy of the catalogue: nothing contradicts it
        # (Myrdal uses 53510 HANEBO OCH KATRINEBERGS BRUKSKAP., which the copy lacks)
        b <- fold_name(gsub("(*UCP)(F\u00d6RS)(\\.|\\b)", " ", nm, perl = TRUE))
        bt <- strsplit(fold_tokens(nm), "-")[[1]]
        any(vapply(names_of[[u]] %||% character(), function(n) {
          a <- fold_name(n); at <- strsplit(fold_tokens(n), "-")[[1]]
          startsWith(a, b) || startsWith(b, a) ||
            utils::adist(a, b) <= 0.2 * max(nchar(a), nchar(b)) ||
            (length(at) && all(at %in% bt)) || (length(bt) && all(bt %in% at))
        }, logical(1)))
      }, unit_id, ddb_name_raw)) %>%
      select(-ddb_name_raw)
  }
  code_r111 <- bind_rows(r111_dedik, take("forkod", function(v) sprintf("%06d", as.integer(v))))
}
say("codes carried from the earlier build's registry by pid: %d dedik, %d forkod",
    sum(code_r111$system == "dedik"), sum(code_r111$system == "forkod"))
code_dedik <- bind_rows(code_dedik, code_r111 %>% filter(system == "dedik"))
forkod_all <- bind_rows(forkod_all, code_r111 %>% filter(system == "forkod"))

# ---- Researched code corrections (corrections_codes.csv, kind "code") ---------------------
# A code no rule reaches gets its parish by hand, with the source: SANKT JOHANNIS (the Latin
# genitive) is Johannes församling. The code is taken off any other unit first.
cc_f <- "data-raw/model/corrections_codes.csv"
corr_codes <- if (file.exists(cc_f)) read_csv(cc_f, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(kind == "code") else tibble(system = character(), code = character(), pid = character())
code_corr <- corr_codes %>% mutate(pid = as.integer(pid)) %>%
  inner_join(pu %>% select(unit_id, pid, start_date, end_date), by = "pid") %>%
  transmute(unit_id, system, code, start_date, end_date, precision = "none",
            source = paste0("correction: ", source))
code_dedik <- code_dedik %>% filter(!code %in% code_corr$code[code_corr$system == "dedik"]) %>%
  bind_rows(code_corr %>% filter(system == "dedik"))
forkod_all <- forkod_all %>% filter(!code %in% code_corr$code[code_corr$system == "forkod"]) %>%
  bind_rows(code_corr %>% filter(system == "forkod"))
say("code corrections applied: %d rows on %d units", nrow(code_corr), n_distinct(code_corr$unit_id))

# dedikscb: the SCB code as it is carried in the DDB/NAD metadata (= the unit's own forkod)
code_dedikscb <- forkod_last %>% inner_join(pu %>% select(unit_id, start_date, end_date), by = "unit_id") %>%
  transmute(unit_id, system = "dedikscb", code = sprintf("%06d", as.integer(forkod)),
            start_date, end_date, precision = "none",
            source = "parish_medta dedikscb (= the unit's own forkod)")

# ── 8. pid rows ──────────────────────────────────────────────────────────────
code_pid <- pu %>% filter(!is.na(pid)) %>%
  transmute(unit_id, system = "pid", code = as.character(pid), start_date, end_date,
            precision = "none",
            source = ifelse(unit_id %in% new_pids$unit_id,
                            "new pid (proposed, data-raw/model/out/pid_lookup_new.csv)",
                            "frozen pid (data-raw/pid_lookup.csv)"))

unit_codes <- bind_rows(code_ref, code_nad, code_pid, forkod_all, code_dedik, code_dedikscb) %>%
  mutate(start_date = pmax(start_date, DMIN), end_date = pmin(end_date, DMAX)) %>%
  filter(start_date <= end_date) %>%
  arrange(unit_id, system, start_date)

# ── 9. unit_names_extra ──────────────────────────────────────────────────────
pid2unit <- pu %>% filter(!is.na(pid)) %>% select(pid, unit_id, u_start = start_date, u_end = end_date)

# (a) SFGT namn (official renames and former names), attached to the unit that holds the pid
# and, for a pid with several units, to the unit whose lifetime the name period touches.
nx_sfgt <- reg_names %>% inner_join(pid2unit, by = "pid", relationship = "many-to-many") %>%
  mutate(start_date = coalesce(start_date, u_start), end_date = coalesce(end_date, u_end)) %>%
  filter(start_date <= u_end, u_start <= end_date) %>%
  transmute(unit_id, name, start_date = pmax(start_date, u_start), end_date = pmin(end_date, u_end),
            precision, kind, source = "SFGT (Skatteverket 1989), namn")

# (b) SCB code-list names, per code interval (dated)
nx_scb <- m_scb %>% transmute(unit_id, name = s_name, start_date = from_c, end_date = to_c,
                              precision = "day", kind = "scb", source = "SCB parish code lists")

# (c) SCB change list: renames 1970-1990 (types 16, 30-33 recoded+renamed, 40 renamed)
nx_chg <- tibble()
if (nrow(chg)) {
  chg <- chg %>%
    filter(date <= DMAX, type %in% c("16", "30", "31", "32", "33", "40"),
           !is.na(new_name), new_name != old_name)
  # only the parish's own codes: a kyrkobokföringsdistrikt's code (given to its parish for lookup)
  # would bring the kbfd's names in as the parish's ("Stora Tuna kbfd del st -> Stora Tuna kbfd")
  hold <- forkod_all %>% filter(!grepl(LOOKUP_ONLY, source)) %>% select(unit_id, code, start_date, end_date)
  nx_chg <- bind_rows(
    chg %>% inner_join(hold, by = c("new" = "code"), relationship = "many-to-many") %>%
      filter(date >= start_date - 366, date <= end_date + 1) %>%
      transmute(unit_id, name = new_name, start_date = date, end_date = as.Date(NA)),
    chg %>% inner_join(hold, by = c("old" = "code"), relationship = "many-to-many") %>%
      filter(date > start_date, date <= end_date + 366) %>%
      transmute(unit_id, name = old_name, start_date = as.Date(NA), end_date = date - 1)) %>%
    left_join(pu %>% select(unit_id, u_start = start_date, u_end = end_date), by = "unit_id") %>%
    mutate(start_date = pmax(coalesce(start_date, u_start), u_start),
           end_date = pmin(coalesce(end_date, u_end), u_end)) %>%
    filter(start_date <= end_date) %>%
    transmute(unit_id, name, start_date, end_date, precision = "day", kind = "official",
              source = "SCB change list 1970-1990")
}
say("SCB change-list renames 1970-1990: %d name rows", nrow(nx_chg))

# (d) DDB names (the parish's own entry)
# only the primary code's name: a chapel's or bönehus's DDB name is not a name of the parish
nx_ddb <- ded_u %>% filter(primary) %>% transmute(unit_id, name = ddb_name, start_date = u_start, end_date = u_end,
                              precision = "none", kind = "ddb", source = "DDB parish catalogue")

# (e) SCB names from parish_medta, per NAD record (own, per version)
nx_medta <- nad_names %>% transmute(unit_id, name = stringr::str_to_title(name_scb),
                                    start_date = year_start(start), end_date = year_end(end),
                                    precision = "none", kind = "scb",
                                    source = "parish_medta (SCB socken name per NAD code)")

# (f) SFGT indelning events of type "renamed"
load("data-raw/sfgt/sfgt_indelning.rda")
nx_ind <- sfgt_indelning %>% filter(event_type == "renamed", !is.na(other_parish)) %>%
  inner_join(pid2unit, by = "pid", relationship = "many-to-many") %>%
  transmute(unit_id, name = other_parish, start_date = u_start,
            end_date = if_else(is.na(year), u_end, pmin(year_end(coalesce(year, 1990L)), u_end)),
            precision = ifelse(is.na(year), "none", "year"), kind = "former",
            source = "SFGT indelning (parsed)")

# (g) parish_registry name_previous, only where the SFGT namn parser produced nothing
load("data/parish_registry.rda")
np <- parish_registry %>% filter(!is.na(name_previous), !pid %in% reg_names$pid) %>%
  tidyr::separate_rows(name_previous, sep = ",\\s*") %>%
  mutate(y = suppressWarnings(as.integer(str_extract(name_previous, "\\d{4}"))),
         nm = clean_name(gsub("\\d{4}(-\\d{2}(-\\d{2})?)?", " ", name_previous))) %>%
  filter(!is.na(nm), nzchar(nm), vapply(nm, ok_name, logical(1)))
nx_prev <- np %>% inner_join(pid2unit, by = "pid", relationship = "many-to-many") %>%
  transmute(unit_id, name = nm, start_date = u_start,
            end_date = if_else(is.na(y), u_end, pmin(year_end(coalesce(y, 1990L)), u_end)),
            precision = ifelse(is.na(y), "none", "year"),
            kind = ifelse(is.na(y), "former", "official"),
            source = "parish_registry name_previous")

# An SCB change-list row for a name that has ended gives only the end date; its start is the
# unit's start. Where SFGT dates the same name, that row is the better one, so the SCB copy goes.
nx_chg <- nx_chg %>% mutate(k = fold_name(name)) %>%
  anti_join(nx_sfgt %>% filter(kind == "official") %>% transmute(unit_id, k = fold_name(name)),
            by = c("unit_id", "k")) %>% select(-k)
unit_names_extra <- bind_rows(nx_sfgt, nx_scb, nx_chg, nx_ddb, nx_medta, nx_ind, nx_prev) %>%
  filter(!is.na(name), nzchar(trimws(name))) %>%
  mutate(name = trimws(name), start_date = pmax(start_date, DMIN), end_date = pmin(end_date, DMAX)) %>%
  filter(start_date <= end_date) %>%
  left_join(unames %>% transmute(unit_id, m1_key = fold_name(name), m1_start = start_date,
                                 m1_end = end_date), by = "unit_id",
            relationship = "many-to-many") %>%
  mutate(key = fold_name(name),
         same_as_m1 = !is.na(m1_key) & key == m1_key & start_date <= m1_end & m1_start <= end_date) %>%
  group_by(unit_id, name, start_date, end_date, precision, kind, source, key) %>%
  summarise(same_as_m1 = any(same_as_m1), .groups = "drop") %>%
  distinct(unit_id, key, start_date, end_date, kind, .keep_all = TRUE) %>%
  arrange(unit_id, start_date, kind)
say("unit_names_extra: %d rows (%d official, %d not a name m1 has for the unit)",
    nrow(unit_names_extra), sum(unit_names_extra$kind == "official"),
    sum(!unit_names_extra$same_as_m1))

# Official names that contradict the m1 source name over the same period (Eslöv/Västra Sallerup)
renamed_back <- unit_names_extra %>% filter(kind == "official", !same_as_m1) %>%
  inner_join(pu %>% select(unit_id, u_name = name), by = "unit_id") %>%
  filter(fold_tokens(u_name) != fold_tokens(name)) %>%
  select(unit_id, source_name = u_name, official_name = name, start_date, end_date, precision, source)
say("earlier official names that differ from the source name: %d on %d units",
    nrow(renamed_back), n_distinct(renamed_back$unit_id))

# ── 10. parish_link_compat ───────────────────────────────────────────────────
cmeta <- read.csv("data-raw/data/county_meta.csv") %>% select(code, letter, county_name = name)
cur_name <- bind_rows(
  unames %>% semi_join(pu, by = "unit_id") %>% transmute(unit_id, name, end_date, kind, pr = 2),
  unit_names_extra %>% filter(kind == "official") %>% transmute(unit_id, name, end_date, kind, pr = 1)) %>%
  group_by(unit_id) %>% arrange(desc(end_date), pr, .by_group = TRUE) %>% slice(1) %>% ungroup() %>%
  select(unit_id, name_current = name)
county_end <- forkod_last %>% mutate(cc_forkod = as.integer(forkod %/% 10000)) %>% select(unit_id, cc_forkod)
parish_link_compat <- pu %>%
  select(unit_id, pid, name, start_date, end_date, county_nad, own_prefix) %>%
  left_join(cur_name, by = "unit_id") %>%
  left_join(forkod_last %>% select(unit_id, forkod), by = "unit_id") %>%
  left_join(county_end, by = "unit_id") %>%
  # the primary code; the others are in unit_codes (and the package's parish_codes)
  left_join(bind_rows(ded_u %>% filter(primary) %>% transmute(unit_id, dedik = as.integer(dedik)),
                      code_dedik %>% filter(!unit_id %in% ded_u$unit_id) %>% group_by(unit_id) %>%
                        slice(which.max(end_date)) %>% ungroup() %>%
                        transmute(unit_id, dedik = as.integer(code))), by = "unit_id") %>%
  left_join(nad %>% group_by(unit_id) %>% slice(which.max(end)) %>% ungroup() %>%
              transmute(unit_id, nadkod = as.numeric(nad), parse = nad), by = "unit_id") %>%
  left_join(reg %>% select(pid, reg_county = county, reg_name = name, charid), by = "pid") %>%
  mutate(county = coalesce(cc_forkod, county_nad, reg_county),
         county_src = case_when(!is.na(cc_forkod) ~ "forkod at end", !is.na(county_nad) ~ "NAD code",
                                !is.na(reg_county) ~ "registry", TRUE ~ NA_character_)) %>%
  left_join(cmeta, by = c("county" = "code")) %>%
  transmute(unit_id, pid, source_name = name, name = coalesce(name_current, name),
            registry_name = reg_name, charid, start_date, end_date,
            county, letter, county_name, county_src, forkod, dedik, nadkod, parse)

# ── 11. Invariants ───────────────────────────────────────────────────────────
say("--- m1b invariants ---")
overlap_pairs <- function(d){
  d %>% inner_join(d, by = c("system", "code"), suffix = c("", ".y"),
                   relationship = "many-to-many") %>%
    filter(unit_id < unit_id.y, start_date <= end_date.y, start_date.y <= end_date)
}
conf <- overlap_pairs(unit_codes %>% select(unit_id, system, code, start_date, end_date))
# ref_code is unique per type by construction; a code may repeat across types (b-check 7 is
# (type, ref_code)), so the ref_code test is run per type
conf <- conf %>% left_join(units %>% select(unit_id, type_id), by = "unit_id") %>%
  left_join(units %>% select(unit_id.y = unit_id, type_id.y = type_id), by = "unit_id.y") %>%
  filter(system != "ref_code" | type_id == type_id.y)
# Allow-list: none is needed for the cross-unit test. SCB does give one parish two codes at the
# same time when it is divided by a municipal boundary ("Delad av kommungräns"): those pairs sit
# on ONE unit (13 units, e.g. Danderyd 016201 + 018501 in 1968-70) and are kept as they are.
# Any row below is a real conflict between two units.
within <- unit_codes %>% filter(system %in% c("forkod", "dedik")) %>%
  inner_join(unit_codes %>% filter(system %in% c("forkod", "dedik")) %>%
               select(unit_id, system, code2 = code, s2 = start_date, e2 = end_date),
             by = c("unit_id", "system"), relationship = "many-to-many") %>%
  filter(code < code2, start_date <= e2, s2 <= end_date)
split_codes <- scb$code[scb$split]
say("units with two codes of one system at one date: %d (%d of the %d pairs are SCB codes of a parish divided by a municipal boundary)",
    n_distinct(within$unit_id), sum(within$code %in% split_codes | within$code2 %in% split_codes),
    nrow(within))
n_conf <- conf %>% count(system, name = "pairs")
print(as.data.frame(n_conf))
write_csv(conf %>% arrange(system, code), file.path(out_dir, "m1b_code_conflicts.csv"))
check(nrow(conf) == 0, sprintf("no code of one system on two coexisting units (%d pairs)", nrow(conf)))
check(all(!is.na(pu$pid)), sprintf("every parish unit has a pid (%d missing)", sum(is.na(pu$pid))))
check(!any(duplicated(parish_link_compat$unit_id)), "one compatibility row per parish unit")

# ── 12. What the build checks b1-b5 would still find ─────────────────────────
b <- list()
b$b1_pid_unique_at_date <- conf %>% filter(system == "pid") %>% nrow()
b$b2_code_unique_at_date <- conf %>% filter(system %in% c("forkod", "dedik", "nadkod", "parse")) %>% nrow()
# b3: a unit sharing a pid with another has a code system the other has (versions of one pid)
sys4 <- c("forkod", "dedik", "nadkod", "parse")
has <- unit_codes %>% filter(system %in% sys4) %>% distinct(unit_id, system) %>%
  inner_join(pu %>% select(unit_id, pid), by = "unit_id")
b3 <- pu %>% group_by(pid) %>% filter(n() > 1) %>% ungroup() %>%
  select(unit_id, pid, name, start_date, end_date) %>% tidyr::crossing(system = sys4) %>%
  left_join(has %>% mutate(h = TRUE), by = c("unit_id", "pid", "system")) %>%
  group_by(pid, system) %>%
  mutate(first_code = suppressWarnings(min(unit_codes$start_date[
    unit_codes$unit_id %in% unit_id[!is.na(h)] & unit_codes$system == system[1]]))) %>%
  ungroup() %>% filter(is.na(h)) %>% distinct(unit_id, system, .keep_all = TRUE) %>%
  mutate(reason = ifelse(!is.na(first_code) & end_date < first_code,
                         "the version ended before the code existed", "gap"))
b$b3_codes_on_versions <- sum(b3$reason == "gap")
b$b3_versions_before_the_code <- sum(b3$reason != "gap")
write_csv(b3, file.path(out_dir, "m1b_versions_without_a_code.csv"))
# Units with no forkod / no dedik at all, for the report
no_code <- pu %>% select(unit_id, name, start_date, end_date, county_nad, nad_last, own_prefix, pid) %>%
  mutate(has_forkod = unit_id %in% forkod_all$unit_id, has_dedik = unit_id %in% code_dedik$unit_id,
         nad_total = as.numeric(substr(nad_last, 1, 6)) %% 100 >= 70,
         alive_1967 = end_date >= as.Date("1967-01-01")) %>%
  filter(!has_forkod | !has_dedik)
write_csv(no_code, file.path(out_dir, "m1b_units_without_a_code.csv"))
say("units without a forkod: %d (%d of them alive in 1967 or later, %d of those with a NAD total code CCMM7x)",
    sum(!no_code$has_forkod), sum(!no_code$has_forkod & no_code$alive_1967),
    sum(!no_code$has_forkod & no_code$alive_1967 & no_code$nad_total))
say("units without a dedik: %d (%d alive in 1967 or later)", sum(!no_code$has_dedik),
    sum(!no_code$has_dedik & no_code$alive_1967))
scb_left <- scb %>% filter(!paste(code, from) %in% paste(m_scb$code, m_scb$from))
write_csv(scb_left, file.path(out_dir, "m1b_scb_codes_unmatched.csv"))
say("SCB code periods starting before 1991 that reach no unit: %d of %d (kbfd, parish parts and renamed units)",
    nrow(scb_left), nrow(scb))
# b4: the DDB name must be a name the unit has had
ddb_rows <- unit_names_extra %>% filter(kind == "ddb")
own_keys <- bind_rows(unit_name_index %>% transmute(unit_id, k = key),
                      unit_name_index %>% transmute(unit_id, k = key2),
                      unit_names_extra %>% filter(kind != "ddb") %>% transmute(unit_id, k = key),
                      unit_names_extra %>% filter(kind != "ddb") %>%
                        transmute(unit_id, k = fold_tokens(name))) %>% distinct()
b$b4_name_ddb <- ddb_rows %>% mutate(k1 = key, k2 = fold_tokens(name)) %>%
  anti_join(own_keys, by = c("unit_id", "k1" = "k")) %>%
  anti_join(own_keys, by = c("unit_id", "k2" = "k")) %>% nrow()
# b5: placeholder codes
fk <- unit_codes %>% filter(system == "forkod") %>% mutate(n = as.numeric(code))
b$b5_placeholder_codes <- sum(fk$n %% 100 == 0) +
  sum(fk$n %% 100 >= 70 & !fk$code %in% scb_all_codes) +
  sum(unit_codes$system == "dedik" & unit_codes$code %in% c("0", "00000"))
checks <- tibble(check = names(b), violations = unlist(b))
print(as.data.frame(checks))

# ── 13. Save ─────────────────────────────────────────────────────────────────
match_report <- pu %>% select(unit_id, name, start_date, end_date, county_nad, nad_last, own_prefix, pid) %>%
  left_join(matched_how %>% select(-pid), by = "unit_id") %>%
  left_join(forkod_all %>% count(unit_id, name = "n_forkod"), by = "unit_id") %>%
  left_join(code_dedik %>% group_by(unit_id) %>%
              summarise(dedik = paste(unique(code), collapse = ";"), .groups = "drop"), by = "unit_id") %>%
  mutate(pid_source = case_when(unit_id %in% new_pids$unit_id ~ "new",
                                !is.na(score) ~ "registry", TRUE ~ "none"))
write_csv(match_report, file.path(out_dir, "m1b_match_report.csv"))
write_csv(renamed_back, file.path(out_dir, "m1b_earlier_official_names.csv"))
# The registry names parishes in the base form ("Kulltorp", not "Kulltorps församling")
reg_style <- function(x){
  x <- trimws(gsub("\\([^)]*\\)", " ", x))
  keep <- grepl("(stads|lands|domkyrko)församling$", x)     # the registry keeps these in full
  y <- trimws(sub("\\s*församling.*$", "", x))
  y <- sub("([a-zåäö])s$", "\\1", y)
  ifelse(keep, x, y)
}
new_pids$reg_name <- if (nrow(new_pids)) reg_style(new_pids$name) else character()
write_csv(new_pids, file.path(out_dir, "m1b_new_pids.csv"))
write_csv(bind_rows(pid_lookup,
                    new_pids %>% transmute(pid, name = reg_name, charid = NA_character_,
                                           forkod = NA_integer_)),
          file.path(out_dir, "pid_lookup_new.csv"))
saveRDS(list(unit_codes = unit_codes, unit_names_extra = unit_names_extra,
             parish_link_compat = parish_link_compat, match_report = match_report,
             new_pids = new_pids, conflicts = conf, checks = checks),
        file.path(out_dir, "m1b.rds"))
say("unit_codes: %d rows", nrow(unit_codes))
print(as.data.frame(unit_codes %>% count(system, precision)))
say("Saved %s/m1b.rds (%.1f min)", out_dir, as.numeric(difftime(Sys.time(), t0, units = "mins")))
