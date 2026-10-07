#' m4b: The datasets match_units() needs, rebuilt from the model
#'
#' The codes and names come from m1b, so every correction it made reaches matching: a parish
#' version now carries its own pid and codes instead of its successor's, and a parish is called
#' what it was called at the date (Västra Sallerup, not Eslöv, before 1951).
#'
#' Input:  data-raw/model/out/{m1,m1b,m4}.rds, data/parish_registry.rda (for the non-territorial
#' parishes, which have no geometry and so no unit)
#' Output: out/pkg/data/{parish_link,parish_meta,parish_registry,unit_variants}.rda

source("data-raw/build_helpers.R")
source("data-raw/model/model_helpers.R")
m1 <- readRDS("data-raw/model/out/m1.rds")
m1b <- readRDS("data-raw/model/out/m1b.rds")
m4 <- readRDS("data-raw/model/out/m4.rds")
t0 <- timer_start("m4b: matching datasets")
bnd <- st_drop_geometry(m4$boundaries)
pk <- "data-raw/model/out/pkg/data"
yr <- function(d) as.integer(format(as.Date(d), "%Y"))

# Helpers used by the registry and the variants
type_word <- c(parish = "församling", county = "län", municipality = "kommun",
               pastorship = "pastorat", contract = "kontrakt", diocese = "stift",
               hundred = "härad", magistrates_court = "domsaga", district_court = "tingsrätt",
               court_of_appeal = "hovrätt", bailiwick = "fögderi")
drop_paren <- function(x) trimws(gsub("\\s+", " ", sub("\\s*\\([^)]*\\)\\s*$", "", x)))
# One regex per type: sub() takes only its first pattern, so a vector of patterns would apply
# The kind words a unit of a type can carry instead of the type's own word: a municipality is a
# "stad", a "köping" or a "landskommun" as often as a "kommun", and its stem is the place name
# either way ("Stockholms stad" -> Stockholm). 1.1.1 stripped these too, which is why
# match_units("Stockholm", "municipality", 1900) worked there and not here.
kind_words <- list(municipality = c("landskommun", "kommun", "stad", "köping",
                                    "municipalsamhälle"),
                   hundred = c("härad", "tingslag", "skeppslag", "bergslag", "stad"),
                   magistrates_court = c("domsaga", "rådhusrätt", "tingslag"),
                   district_court = c("tingsrätt", "domsaga"))
drop_type <- function(x, ty){
  out <- x
  for (t in unique(ty[!is.na(ty)])) {
    w <- unname(type_word[t]); if (is.na(w)) next
    i <- which(ty == t)
    if (!is.null(kind_words[[t]])) {
      pat2 <- paste0("\\s*(", paste(kind_words[[t]], collapse = "|"), ")\\s*$")
      out[i] <- trimws(gsub("\\s+", " ", sub(pat2, "", x[i])))
      next
    }
    # also "X landsförsamling" / "X stadsförsamling" / "X domkyrkoförsamling" -> "X": the two
    # halves of a town then share a stem, which match_units reports as multiple, honestly
    pat <- paste0("\\s*(lands|stads|domkyrko)?", substr(w, 1, 4), "[a-zåäö]*\\s*$")
    out[i] <- trimws(gsub("\\s+", " ", sub(pat, "", x[i])))
  }
  out
}

# ---- parish_link -----------------------------------------------------------------------------
pl_compat <- m1b$parish_link_compat
par <- bnd %>% filter(type_id == "parish") %>%
  select(geom_id, unit_id, topo_id, ref_code, name, start, end)
parish_link <- par %>%
  left_join(pl_compat %>% select(unit_id, pid, charid, county, letter, county_name,
                                 forkod, dedik, nadkod, parse_code = parse),
            by = "unit_id") %>%
  mutate(socken = NA_character_, alias = NA_character_, dedikscb = forkod,
         grkod = NA_character_, center = NA_character_, area = NA_real_) %>%
  select(geom_id, name, socken, alias, start, end, nadkod, forkod, dedikscb, dedik, grkod,
         county, letter, county_name, center, area, pid, charid, topo_id, ref_code, parse_code)
# The county of a parish version is the one it belonged to for most of its years, taken from the
# hierarchy, not the county in its last code. Almunge was in Stockholms län for 251 of its 271
# years and moved to Uppsala in 1971; its 1990 forkod says Uppsala (03), and reading the county
# off the code put it, and 119 parishes like it, in the county they ended in.
hier <- m4$hierarchy %>% filter(parent_type == "County", child_type == "Parish",
                                !is.na(parent_ref_code))
if (nrow(hier)) {
  maj <- hier %>%
    mutate(cc = suppressWarnings(as.integer(substr(sub("^SE/", "", parent_ref_code), 1, 2))),
           yrs = end - start + 1L) %>%
    filter(!is.na(cc)) %>%
    group_by(geom_id = child_geom_id, cc) %>% summarise(yrs = sum(yrs), .groups = "drop") %>%
    group_by(geom_id) %>% slice_max(yrs, n = 1, with_ties = FALSE) %>% ungroup()
  cmeta <- read.csv("data-raw/data/county_meta.csv") %>% transmute(cc = code, cletter = letter)
  maj <- maj %>% left_join(cmeta, by = "cc")
  n_moved <- sum(parish_link$county[match(maj$geom_id, parish_link$geom_id)] != maj$cc, na.rm = TRUE)
  parish_link <- parish_link %>% left_join(maj %>% select(geom_id, cc, cletter), by = "geom_id") %>%
    mutate(county = coalesce(cc, county), letter = coalesce(cletter, letter)) %>%
    select(-cc, -cletter)
  message(sprintf("  parish_link county from the hierarchy's majority: %d versions changed",
                  n_moved))
}

# The county's name comes from the county unit itself, at the parish's own date, not from an
# external county table: that table calls county 20 "Dalarnas", a name it only took in 1997, and
# 86 parishes carried it for 1600-1990 - the same anachronism the model exists to remove.
cty <- bnd %>% filter(type_id == "county") %>%
  transmute(code2 = substr(sub("^SE/", "", ref_code), 1, 2), cname = name,
            c_start = start, c_end = end) %>% filter(!is.na(code2))
parish_link <- parish_link %>%
  mutate(.code2 = sprintf("%02d", suppressWarnings(as.integer(county)))) %>%
  left_join(cty, by = c(".code2" = "code2"), relationship = "many-to-many") %>%
  mutate(.ov = pmin(end, c_end) - pmax(start, c_start)) %>%
  group_by(geom_id) %>% slice_max(.ov, n = 1, with_ties = FALSE) %>% ungroup() %>%
  mutate(county_name = coalesce(cname, county_name)) %>%
  select(-.code2, -cname, -c_start, -c_end, -.ov)
message(sprintf("  parish_link: %d rows, %d with a pid; county names from the county units",
                nrow(parish_link), sum(!is.na(parish_link$pid))))
anach <- sum(grepl("^Dalarnas|^Sk\u00e5ne|^V\u00e4stra G\u00f6talands", parish_link$county_name))
check(anach == 0, sprintf("no parish carries a post-1990 county name (%d do)", anach))

parish_meta <- parish_link %>%
  left_join(par %>% select(geom_id, topo_id2 = topo_id, ref_code2 = ref_code), by = "geom_id") %>%
  transmute(geom_id, topo_id = topo_id2, ref_code = ref_code2, name, start, end,
            nadkod, forkod, dedikscb, county)

# ---- parish_registry -------------------------------------------------------------------------
# One row per pid: the model's own parishes, plus the non-territorial ones (garrison, hospital,
# kbfd ...) carried over from 1.1.1, which have no geometry and so no unit of their own.
# The registry of the earlier build, frozen as a build input (data-raw/sources/parish_registry_111.csv), so
# that a rebuild does not read the previous release's data/ directory. What is taken from it is
# Skatteverket's own material: the socken and alias fields, and the non-territorial parishes
# (garrison, hospital, kbfd, ships' congregations), which have no territory and so no unit here.
reg_f <- source_file("parish_registry_111.csv")
old_reg <- if (!is.na(reg_f)) read_csv(reg_f, show_col_types = FALSE) else {
  e <- new.env(); load("data/parish_registry.rda", envir = e); e$parish_registry
}
names_by_unit <- bind_rows(
  m1$unit_names %>% transmute(unit_id, name, kind, start = yr(start_date), end = yr(end_date)),
  m1b$unit_names_extra %>% transmute(unit_id, name, kind, start = yr(start_date), end = yr(end_date)))
pick <- function(k) names_by_unit %>% filter(kind == k) %>% group_by(unit_id) %>%
  slice_max(end, n = 1, with_ties = FALSE) %>% ungroup() %>% select(unit_id, nm = name)
# All names a unit ever had, comma separated. name_previous and alias are multi-value columns in
# the registry, and parish matching searches them; keeping only the latest former name lost
# "Högbo" (renamed Sandviken), "Sankt Nikolai" (now Stockholms domkyrkoförsamling) and the like.
pick_all <- function(ks) names_by_unit %>% filter(kind %in% ks, !is.na(name)) %>%
  distinct(unit_id, name) %>% group_by(unit_id) %>%
  summarise(nm = paste(unique(name), collapse = ", "), .groups = "drop")
# socken and alias are Skatteverket registry fields with no counterpart in the model; carry
# them over by pid so match_units() keeps them
old_by_pid <- old_reg %>% select(pid, socken, alias) %>% distinct(pid, .keep_all = TRUE)
# The bare place name is a real alias of the town's own parish: "Uppsala" for Uppsala
# domkyrkoförsamling, "Härnösand" for Härnösands domkyrkoförsamling, "Skara" for Skara
# domkyrkoförsamling. Parish matching reads the registry's columns, not unit_variants, so
# without this a bare town name finds nothing. Where a town has both a stads- and a
# landsförsamling both get the alias and match_units reports it as ambiguous, which is right.
add_alias <- function(name, alias){
  stem <- trimws(gsub("\\s+", " ", sub("\\s*(lands|stads|domkyrko)?församling(en)?\\s*$", "",
                                     sub("\\s*\\([^)]*\\)\\s*$", "", name))))
  stem <- ifelse(stem == name | nchar(stem) < 2, NA_character_, stem)
  # also without the genitive s: the death book writes "Söderköping", the parish is
  # "Söderköpings församling"
  bare <- ifelse(!is.na(stem) & grepl("s$", stem), sub("s$", "", stem), NA_character_)
  add <- ifelse(is.na(bare), stem, paste(stem, bare, sep = ", "))
  out <- ifelse(is.na(alias) | !nzchar(alias), add,
                ifelse(is.na(add), alias, paste(alias, add, sep = ", ")))
  ifelse(is.na(out), NA_character_, out)
}
model_reg <- pl_compat %>%
  left_join(old_by_pid, by = "pid") %>%
  mutate(alias_out = add_alias(name, alias)) %>%
  left_join(par %>% group_by(unit_id) %>% slice_max(end, n = 1, with_ties = FALSE) %>%
              ungroup() %>% select(unit_id, geom_id, topo_id, ref_code), by = "unit_id") %>%
  left_join(pick("scb") %>% rename(name_scb = nm), by = "unit_id") %>%
  left_join(pick("ddb") %>% rename(name_ddb = nm), by = "unit_id") %>%
  left_join(pick_all(c("former", "official")) %>% rename(name_previous = nm), by = "unit_id") %>%
  # `name` is the short form the registry has always carried ("Hjälmseryd") and `name_full` the
  # form with its type word ("Hjälmseryds församling"); the model had both the same, so
  # match_units() answered with the long form where 1.1.1 answered with the short one.
  # `name` is the short form the registry has always carried ("Hjälmseryd") and `name_full` the
  # form with its type word ("Hjälmseryds församling"). The short form is Skatteverket's own, from
  # m1b's registry_name; stripping the type word would leave the genitive ("Hjälmseryds") and
  # stripping that in turn would ruin every name that ends in s of its own (Nås).
  mutate(name_long = name,
         name = ifelse(is.na(registry_name) | !nzchar(registry_name), name_long, registry_name)) %>%
  transmute(pid, geom_id, forkod, nadkod, dedik, dedikscb = forkod, charid, topo_id, ref_code,
            category = "territorial", parent_pid = NA_integer_, county, letter, county_name,
            name, name_full = name_long, socken, alias = alias_out, name_previous, name_scb,
            name_ddb, start = yr(start_date), end = yr(end_date))
non_terr <- old_reg %>% filter(!pid %in% model_reg$pid) %>%
  mutate(geom_id = NA_integer_)                      # its old geom_id no longer exists
# A carried row keeps no code that a parish of the model holds at any time. Those rows are the earlier build's
# pids for parishes the model has under another pid (Visby next to Visby domkyrkoförsamling,
# Fågelås next to Norra Fågelås): they have no polygon, and a code on two pids resolves to nothing,
# so 84410, 59744, 69810, 74200 and 67714 found no parish at all.
held <- m1b$unit_codes %>% filter(system %in% c("forkod", "dedik", "nadkod")) %>%
  mutate(v = suppressWarnings(as.numeric(code))) %>% filter(!is.na(v))
n_dropped <- 0L
for (cl in c("forkod", "dedik", "nadkod")) {
  hv <- c(held$v[held$system == cl], model_reg[[cl]])
  hit <- !is.na(non_terr[[cl]]) & non_terr[[cl]] %in% hv
  n_dropped <- n_dropped + sum(hit)
  non_terr[[cl]][hit] <- NA
}
non_terr$dedikscb[!is.na(non_terr$dedikscb) & non_terr$dedikscb %in% c(held$v, model_reg$forkod)] <- NA
# A forkod shared by several carried rows that are not one parish is no code of any of them
# (38100 on Herkulsberga, Husby, Sankt Olof, Vårfrukyrka ...)
shared <- non_terr %>% filter(!is.na(forkod)) %>% group_by(forkod) %>%
  filter(n_distinct(pid) > 1) %>% pull(forkod) %>% unique()
non_terr$forkod[non_terr$forkod %in% shared] <- NA
message(sprintf(paste("  carried registry rows: %d codes held by a model parish dropped,",
                      "%d forkods shared by several carried rows dropped"), n_dropped, length(shared)))
# A congregation's parent must lie in its own county. Hotagens lappförsamling (Jämtland) had Åsele
# (Västerbotten) as parent, so a user got Åsele for it (finding F). Where the parent is missing or
# in another county, the parent is the model parish of the same base name in the row's county,
# when there is exactly one.
base_of <- function(x) {
  x <- tolower(trimws(gsub("\\s*\\([^)]*\\)", "", x)))
  x <- sub("\\s*(lapp|lappmarks|bruks|slotts|kapell)?(församling|förs\\.?)\\s*$", "", x)
  # a kyrkobokföringsdistrikt: "Ekeby norra kbfd" lies in Ekeby, "Bromma kbfd" in Bromma
  x <- sub("\\s+(norra|södra|östra|västra|mellersta|övre|nedre)?\\s*(kbfd|kyrkobokföringsdistrikt)$", "", x)
  sub("s$", "", x)
}
mr <- model_reg %>% transmute(t_pid = pid, t_county = county, b = base_of(name)) %>%
  group_by(b, t_county) %>% filter(n() == 1) %>% ungroup()
par_county <- setNames(c(model_reg$county, old_reg$county), c(model_reg$pid, old_reg$pid))
fixp <- non_terr %>% filter(!is.na(county)) %>%
  mutate(p_county = unname(par_county[as.character(parent_pid)]),
         b = base_of(name)) %>%
  filter(is.na(parent_pid) | (!is.na(p_county) & p_county != county)) %>%
  inner_join(mr, by = c("b", "county" = "t_county")) %>% select(pid, t_pid, old_parent = parent_pid)
non_terr$parent_pid[match(fixp$pid, non_terr$pid)] <- fixp$t_pid
# Researched parents (corrections_codes.csv, kind "parent"): rows whose parent has no polygon of
# its own (the earlier build's row "Skellefteå") or lies outside the town (Karlskrona tyska under Augerum)
cp <- read_csv("data-raw/model/corrections_codes.csv", show_col_types = FALSE,
               col_types = cols(.default = "c")) %>% filter(kind == "parent") %>%
  transmute(pid = as.numeric(pid), parent = as.numeric(parent_pid))
hit <- match(cp$pid, non_terr$pid)
non_terr$parent_pid[hit[!is.na(hit)]] <- cp$parent[!is.na(hit)]
message(sprintf("  researched parent corrections applied: %d of %d", sum(!is.na(hit)), nrow(cp)))
message(sprintf("  non-territorial parents set from the same name in the same county: %d (%d replaced one in another county)",
                nrow(fixp), sum(!is.na(fixp$old_parent))))
parish_registry <- bind_rows(model_reg, non_terr %>% select(any_of(names(model_reg)))) %>%
  arrange(pid)
message(sprintf("  parish_registry: %d rows (%d territorial from the model, %d carried over)",
                nrow(parish_registry), nrow(model_reg), nrow(non_terr)))

# ---- unit_variants ----------------------------------------------------------------------------
# Every name a unit had, with the period it applies to, plus normalised and historical-spelling
# forms and the codes. Built per geom_id so match_units() can use it unchanged.
unit_names_all <- bind_rows(
  m1$unit_names %>% transmute(unit_id, name, kind, start = yr(start_date), end = yr(end_date),
                              source = "source"),
  m1b$unit_names_extra %>% transmute(unit_id, name, kind, start = yr(start_date),
                                     end = yr(end_date), source = kind)) %>%
  filter(!is.na(name), nchar(name) > 1) %>%
  # m4 cleans the names it puts in `boundaries`; these come straight from unit_names and did not
  # get that treatment, so the register's own "Amals fogderi fogderi" (unit F:00370) was a variant
  # and b9 counted it as unmatchable -- rightly, since nobody will ever type it.
  mutate(name = clean_unit_name(name)) %>% distinct()
gid_periods <- bnd %>% select(geom_id, unit_id, type_id, g_name = name,
                              g_start = start, g_end = end)
# Seed from the features themselves, so every unit has at least its own name as a variant.
# unit_names covers only the units that come from the Riksarkivet source; the municipalities
# that exist only in SCB (KS:) and the courts from the statskalender (DS:) are not in it, and
# without this those 284 of 287 municipalities at 1990 had no variant at all and could not be
# matched by name.
nv_self <- bnd %>%
  transmute(geom_id, type_id, canonical_name = name, variant = name, start, end,
            source = "name_official", priority = 1L)
nv <- bind_rows(nv_self, unit_names_all %>%
  inner_join(gid_periods, by = "unit_id", relationship = "many-to-many") %>%
  filter(start <= g_end, g_start <= end) %>%
  # canonical_name is the FEATURE's name, not the alias's: match_units() reports it as `name`, so
  # with the alias there match_units("Dalarnas lan", "county") answered "Dalarnas lan" for a unit
  # the package calls Kopparbergs lan -- the variant echoed back instead of the unit named.
  transmute(geom_id, type_id, canonical_name = g_name, variant = name,
            start = pmax(start, g_start), end = pmin(end, g_end),
            source = paste0("name_", kind), priority = ifelse(kind == "official", 1L, 3L))) %>%
  distinct(geom_id, variant, .keep_all = TRUE)

# Without the county suffix, and without the type word. Parish names often carry the county
# ("Segerstads församling (H-län)"), so the type word is not at the end and a rule anchored
# there strips nothing: that is what cost the parish stems, and with them the name matching.
# the first row's type to every row (which left parishes with no stem at all).
no_paren <- nv %>% mutate(variant = drop_paren(variant), source = "no_county", priority = 3L) %>%
  filter(variant != canonical_name, nchar(variant) > 1)
base_nm <- nv %>%
  mutate(variant = drop_type(drop_paren(variant), type_id), source = "stem", priority = 4L) %>%
  filter(variant != canonical_name, nchar(variant) > 1)
# Some source names carry the unit's period: "Kristianstads kommun 1971-", "Nacka kommun 1971-",
# "Alingsås församling -1618". The name is kept as the source has it, because it distinguishes two
# units of one name (146 names are on more than one unit of a type, 239 without the suffixes), but
# nothing found them: "Kristianstads kn" in the death book matched nothing, and neither did the
# curated abbreviations, which are keyed by the name. So the name without its period is a variant.
drop_years <- function(x) trimws(sub("\\s+\\(?[0-9]{0,4}\\s?-\\s?[0-9]{0,4}\\)?$", "", x))
# Measured, and not kept: adding the period-free form as a variant gave 104 new b9 violations,
# and not only ambiguity. A bailiwick called "Kungsbacka fögderi 1720-1920" and one called
# "Kungsbacka stad" both reduce to Kungsbacka, and the matcher then returned the stad for the
# fögderi's own name — a silent wrong answer in place of no answer. The curated variants are
# still joined on the period-free name below, which is safe because those names are hand-made.
# What the period-free form was meant to fix (the death book's "Kristianstads kn") is not fixed
# by it: 1.1.1 fails that string too, so the death book's municipality drop has another cause.
message("  (period-free name variants tried and reverted: see the comment above)")

# The stem keeps the genitive: "Stockholms stad" gives the stem "Stockholms", and
# match_units("Stockholm", "municipality", 1900) found nothing where the earlier build had the bare form as a
# curated variant. The genitive-free stem is added for the types whose names are place + kind, and
# only where it does not already belong to another unit of the same type at the same time — a
# variant that points at two units is what b9 counts as a failure.
nogen <- base_nm %>% filter(grepl("s$", variant)) %>%
  mutate(variant = sub("s$", "", variant), source = "stem", priority = 5L) %>%
  filter(nchar(variant) > 2, type_id != "parish")
if (nrow(nogen)) {
  clash <- bind_rows(nv, no_paren, base_nm) %>% select(type_id, variant, geom_id) %>%
    inner_join(nogen %>% select(type_id, variant, ng_geom = geom_id),
               by = c("type_id", "variant"), relationship = "many-to-many") %>%
    filter(geom_id != ng_geom) %>% distinct(type_id, variant)
  nogen <- nogen %>% anti_join(clash, by = c("type_id", "variant")) %>%
    group_by(type_id, variant) %>% filter(n_distinct(geom_id) == n()) %>% ungroup()
  message(sprintf("  genitive-free stems added: %d (%d dropped as another unit's name)",
                  nrow(nogen), nrow(clash)))
}
# No pre-generated historical spellings: match_units() applies normalize_historical_spelling()
# at query time, which is the right place. Generating them here produced unmatchable strings
# ("Asks kommun" -> "Asx kommun") and misspelt type words ("rådhusrätt" -> "rådhusrett").
# Only ref_code belongs here: forkod, dedik, nadkod, parse and pid are read from parish_link and
# parish_registry, and storing them as variants added 16,000 rows that cannot round-trip.
codes <- m1b$unit_codes %>% filter(system == "ref_code") %>%
  inner_join(gid_periods, by = "unit_id", relationship = "many-to-many") %>%
  mutate(s = pmax(yr(start_date), g_start), e = pmin(yr(end_date), g_end)) %>%
  filter(!is.na(s), !is.na(e), s <= e) %>%
  # the feature's own name, not NA: match_units() reports the variant's canonical_name, so a code
  # match returned the right unit with no name at all
  transmute(geom_id, type_id, canonical_name = g_name,
            # lower case, as the earlier build stored them: the lookup is case-sensitive, and
            # "SE/010199005" simply never matched while "se/010199005" does
            # lower case with the se/ prefix for every type, as 1.1.1 stored them: a parish's
            # ref_code is matched against parish_registry$ref_code, which holds the full code,
            # so stripping "SE/" made all 3,051 of them unmatchable
            variant = tolower(code),
            start = s, end = e, source = paste0("code_", system), priority = 2L)
# 1.1.1's hand-made variants (abbreviation, manual) are curated knowledge with no source in
# the model; carry them onto whichever unit now has that canonical name and period
# The curated variants (abbreviations, the county-code forms, the manual additions), likewise
# frozen as a build input rather than read back from the last release.
cur_f <- source_file("unit_variants_curated.csv")
curated0 <- if (!is.na(cur_f)) read_csv(cur_f, show_col_types = FALSE) else {
  ov <- new.env(); load("data/unit_variants.rda", envir = ov)
  ov$unit_variants %>% filter(source %in% c("abbreviation", "manual", "county_code"))
}
curated <- curated0 %>%
  select(variant, canonical_name, type_id, source) %>% distinct()
# joined on the name and on the name without its period, so a curated abbreviation reaches
# "Kristianstads kommun 1971-" as well
by_name <- bind_rows(
  nv %>% select(geom_id, type_id, canonical_name, start, end),
  nv %>% select(geom_id, type_id, canonical_name, start, end) %>%
    mutate(canonical_name = drop_years(canonical_name))) %>% distinct()
carried <- curated %>% inner_join(by_name, by = c("canonical_name", "type_id"),
                                  relationship = "many-to-many") %>%
  mutate(priority = 3L)
message(sprintf("  curated variants carried over: %d of %d", n_distinct(carried$variant),
                n_distinct(curated$variant)))
# variant_normalized must be what the matcher computes from a user's query, or the variant can
# never be found: match_units() looks a query up as normalize_unit_name(query) against this column.
# The hand-rolled version here (chartr before tolower) left an uppercase A-ring unfolded and
# stripped none of the type words, so 239 of the genitive-free stems -- "Norra Angermanland" for
# "Norra Angermanlands fogderi" -- were stored unfindable, which is what b9 caught. So use the
# package's own function, read out of R/, and check afterwards that every row agrees with it. This
# is the one place where the build reads the package's code on purpose: the index and the lookup
# must use one key, and it needs the source tree, not an installed swehist.
mp_env <- new.env(parent = globalenv())
sys.source("R/match_parishes.R", mp_env)
normalize_unit_name <- mp_env$normalize_unit_name
unit_variants <- bind_rows(nv, no_paren, base_nm, nogen, codes, carried) %>%
  mutate(variant_normalized = normalize_unit_name(variant)) %>%
  filter(!is.na(variant), nchar(variant) > 0) %>%
  distinct(geom_id, variant, .keep_all = TRUE) %>%
  select(variant, variant_normalized, geom_id, canonical_name, type_id, start, end, source,
         priority) %>% arrange(geom_id, priority, variant)
message(sprintf("  unit_variants: %d rows for %d units", nrow(unit_variants),
                n_distinct(unit_variants$geom_id)))
bad <- grepl("vörsamling|rådusrätt|vögderi|rådhusrett|hovrett|fögderi[^ ]", unit_variants$variant)
message(sprintf("  spelling bugs from the earlier variant builder present: %d", sum(bad)))

# ---- parish_codes -----------------------------------------------------------------------------
# Every dated code of every parish version: parish_link carries one code per version, but a parish
# has had several (DDB gives Byske 82983 and 82990; SCB recoded parishes in 1952-1990), and a user's
# data carries the one valid at its date. match_units() looks here when parish_link has nothing.
parish_codes <- m1b$unit_codes %>% filter(system %in% c("forkod", "dedik", "nadkod")) %>%
  inner_join(par %>% select(geom_id, unit_id, g_start = start, g_end = end), by = "unit_id",
             relationship = "many-to-many") %>%
  mutate(start = pmax(yr(start_date), g_start), end = pmin(yr(end_date), g_end)) %>%
  filter(start <= end) %>%
  left_join(parish_link %>% select(geom_id, pid), by = "geom_id") %>%
  transmute(geom_id, pid, system, code = as.numeric(code), start = as.integer(start),
            end = as.integer(end), precision, source) %>%
  distinct() %>% arrange(system, code, start)
message(sprintf("  parish_codes: %d rows (%s)", nrow(parish_codes),
                paste(sprintf("%s %d", names(table(parish_codes$system)), table(parish_codes$system)),
                      collapse = ", ")))
save(parish_codes, file = file.path(pk, "parish_codes.rda"), compress = "xz")

save(parish_link, file = file.path(pk, "parish_link.rda"), compress = "gzip")
save(parish_meta, file = file.path(pk, "parish_meta.rda"), compress = "gzip")
save(parish_registry, file = file.path(pk, "parish_registry.rda"), compress = "gzip")
save(unit_variants, file = file.path(pk, "unit_variants.rda"), compress = "gzip")
timer_end(t0)

message("  --- m4b checks ---")
check(nrow(parish_link) == sum(bnd$type_id == "parish"), "one parish_link row per parish feature")
check(!anyNA(parish_link$geom_id), "every parish_link row has a geom_id")
check(sum(bad) == 0, sprintf("no spelling-bug variants from the earlier variant builder (got %d)", sum(bad)))
# Every parish whose name carries a type word must have a stem variant: the earlier build had
# these ("stripped"), and losing them cost about 1.5 points a decade on name matching.
# The stem STRING must be present; its source label may be something else, because a DDB or SCB
# name can be the same string and wins the de-duplication.
need_stem <- nv %>% filter(type_id == "parish", grepl("församling", canonical_name)) %>%
  mutate(want = drop_type(drop_paren(canonical_name), type_id)) %>%
  filter(want != canonical_name, nchar(want) > 1) %>% distinct(geom_id, want)
miss_stem <- need_stem %>%
  anti_join(unit_variants %>% transmute(geom_id, want = variant), by = c("geom_id", "want"))
check(nrow(miss_stem) == 0,
      sprintf("every parish named '... församling' has its stem as a variant (%d without, e.g. %s)",
              nrow(miss_stem), paste(head(miss_stem$want, 3), collapse = ", ")))
no_var <- setdiff(bnd$geom_id, unit_variants$geom_id)
check(length(no_var) == 0, sprintf("every feature has a variant (%d without)", length(no_var)))
check(nrow(unit_variants) > 20000, sprintf("unit_variants > 20000 (got %d)", nrow(unit_variants)))
# The lookup key is the matcher's own: a row whose variant_normalized is anything else is dead
# weight, findable only if the variant happens to be a unit's name in `boundaries` as well.
off_key <- unit_variants %>% filter(variant_normalized != normalize_unit_name(variant))
check(nrow(off_key) == 0,
      sprintf("every variant_normalized is normalize_unit_name(variant) (%d not, e.g. %s)",
              nrow(off_key), paste(head(off_key$variant, 3), collapse = ", ")))
saveRDS(list(parish_link = parish_link, parish_registry = parish_registry,
             unit_variants = unit_variants), "data-raw/model/out/m4b.rds")
message("  Saved data-raw/model/out/m4b.rds and the package datasets")
