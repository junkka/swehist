#' m3b: Resolve the evidence into memberships
#'
#' One answer per (atom, parent_type, period), chosen from the competing claims, with the
#' source and a quality grade kept on every row. Three passes, mirroring the real
#' administrative structure.
#'
#' Input:  data-raw/model/out/{m1,m2,m3,m3s,m1b}.rds, optionally out/m3a.rds (SCB
#' municipalities) and out/m3c.rds (courts from the statskalender)
#' Output: data-raw/model/out/m3b.rds: list(membership, parts, unresolved, precedence, report)

source("data-raw/build_helpers.R")
source("data-raw/model/model_helpers.R")
m1 <- readRDS("data-raw/model/out/m1.rds")
m2 <- readRDS("data-raw/model/out/m2.rds")
m3 <- readRDS("data-raw/model/out/m3.rds")
m3s <- readRDS("data-raw/model/out/m3s.rds")
has <- function(f) file.exists(file.path("data-raw/model/out", f))
m3a <- if (has("m3a.rds")) readRDS("data-raw/model/out/m3a.rds") else NULL
m3c <- if (has("m3c.rds")) readRDS("data-raw/model/out/m3c.rds") else NULL
m3d <- if (has("m3d.rds")) readRDS("data-raw/model/out/m3d.rds") else NULL
t0 <- timer_start("m3b: resolve memberships")
atoms <- st_drop_geometry(m2$atoms)
Y0 <- 1600L; Y1 <- 1990L

# ---- Claims ------------------------------------------------------------------------------
claims <- bind_rows(
  m3$evidence %>% mutate(parent_name = NA_character_),
  m3s$evidence,
  # the statskalender's ecklesiastikstat: kontrakt, pastorat and stift, parish by parish (m3d).
  # Kontrakt membership had no source at all before this — only containment.
  if (!is.null(m3d)) m3d$evidence %>% mutate(parent_name = NA_character_) else NULL
) %>% filter(!is.na(atom_id))
if (!is.null(m3a)) {
  # An SCB municipality that m3a matched to a source municipality is the SAME unit: use the
  # source unit_id, or both would build their own features and share a kommunkod (a7).
  scb_id <- m3a$mun_units %>%
    transmute(mun_id, use_id = ifelse(status == "in_source" & !is.na(m1_unit_id),
                                      m1_unit_id, mun_id))
  scb <- bind_rows(
    m3a$mun_evidence %>% filter(!is.na(m1_parish_unit)) %>%
      left_join(scb_id, by = "mun_id") %>% mutate(mun_id = coalesce(use_id, mun_id)) %>%
      transmute(child_unit = m1_parish_unit, parent_unit = mun_id, parent_type = "municipality",
                start = as.integer(format(start_date, "%Y")), end = as.integer(format(end_date, "%Y")),
                partial = coalesce(partial, FALSE)),
    m3a$county_evidence %>% filter(!is.na(m1_county_unit)) %>%
      transmute(child_unit = m1_parish_unit, parent_unit = m1_county_unit, parent_type = "county",
                start = as.integer(format(start_date, "%Y")), end = as.integer(format(end_date, "%Y")),
                partial = FALSE)
  ) %>% filter(!is.na(child_unit)) %>%
    inner_join(atoms %>% select(atom_id, unit_id, a_start = start, a_end = end),
               by = c("child_unit" = "unit_id"), relationship = "many-to-many") %>%
    mutate(start = pmax(start, a_start), end = pmin(end, a_end)) %>% filter(start <= end) %>%
    transmute(atom_id, child_unit, parent_unit, parent_type, start, end,
              source = "scb", dated = TRUE, share = NA_real_, parent_name = NA_character_)
  claims <- bind_rows(claims, scb %>% select(-any_of("partial")))
  message(sprintf("  SCB claims added: %d", nrow(scb)))
}
claims$source_rank <- NA_integer_

PREC <- list(
  municipality = c("correction", "scb", "register_dated", "register_undated", "containment"),
  bailiwick    = c("correction", "register_dated", "register_undated", "containment"),
  hundred      = c("correction", "statskalender", "register_dated", "register_undated", "containment"),
  pastorship   = c("correction", "sfgt", "statskalender", "register_dated", "containment"),
  contract     = c("correction", "statskalender", "containment"),
  county       = c("correction", "scb", "sfgt", "register_dated", "register_undated", "containment")
)
# The order above is a hypothesis, not a law: a dated source is not automatically better than
# geometry when its dates are coarse (the held-out reading of 27 September). SWEHIST_PREC overrides
# one or more types for an experiment, as "pastorship:correction>statskalender>sfgt>containment;
# hundred:correction>statskalender>containment>register_undated". The winning order of the
# sweep that tried the alternatives is written in here.
prec_override <- Sys.getenv("SWEHIST_PREC", "")
if (nzchar(prec_override)) {
  for (part in strsplit(prec_override, ";")[[1]]) {
    kv <- strsplit(trimws(part), ":")[[1]]
    if (length(kv) != 2) next
    PREC[[kv[1]]] <- trimws(strsplit(kv[2], ">")[[1]])
    message(sprintf("  PREC override: %-12s %s", kv[1], paste(PREC[[kv[1]]], collapse = " > ")))
  }
}
# Pass 2's order, per type, with VIA_STK and VIA_CHAIN standing for the chains through each
# intermediate type. It is per type because pass 1 is, and because a source can be right about one
# level and wrong about another: the statskalender's courts test better than the register's, its
# kontrakt do not. SWEHIST_PREC2 overrides it, either as one order for every type
# ("correction>scb>...") or per type ("contract:correction>containment>statskalender;diocese:...").
PREC2_DEFAULT <- c("correction", "scb", "sfgt", "statskalender", "VIA_STK", "VIA_CHAIN",
                   "register_dated", "register_undated", "containment")
# contract: containment before the statskalender and before the pastorat chain, which is the
# opposite of every other type here. Measured against BiSOS 1880 Tab. 4, which gives the population
# of each of the 181 deaneries: with the statskalender first, 54 of 115 matched deaneries came within
# 3% of BiSOS's population; with containment first, 61 of 117 (within 10%: 0.626 -> 0.684). Those
# figures are from the matcher of 27 September; on the current build the same benchmark reads
# bisos.deanery_matched 147 of 181 and bisos.deanery_pop_within3 103 of 147. The
# external benchmark, with better name matching, moves the same way and by the same amount
# (0.701 -> 0.640 when the statskalender was put first). The statskalender's kontrakt were not
# filling gaps -- containment covers the level -- they were displacing it, and 15 deaneries that had
# been within 1% of BiSOS went 20 to 115% out, in both directions, which is what moving parishes
# between neighbouring kontrakt looks like. The reason is in m3d: the 1866 edition's composition
# resolves to a unit for only about 70% of the names it prints, and what does resolve is then painted
# from the unit's start to 1880, so one mis-resolved parish sits in the wrong kontrakt for centuries.
# The sourced grade is therefore not claimed for kontrakt. m3d's diocese chain and its pastorat
# evidence are kept: both measure better (bisos.county_diocese_area_within2 0.618 -> 0.677,
# rosenberg.pastorship_pairs 0.987 -> 0.990, a1's pastorship rows 120 -> 27).
PREC2 <- list(contract = c("correction", "containment", "statskalender",
                           "VIA_STK", "VIA_CHAIN", "register_dated", "register_undated"))
o2 <- Sys.getenv("SWEHIST_PREC2", "")
if (nzchar(o2)) {
  if (grepl(":", o2)) {
    for (part in strsplit(o2, ";")[[1]]) {
      kv <- strsplit(trimws(part), ":")[[1]]
      if (length(kv) != 2) next
      PREC2[[kv[1]]] <- trimws(strsplit(kv[2], ">")[[1]])
      message(sprintf("  PREC2 override: %-12s %s", kv[1], paste(PREC2[[kv[1]]], collapse = " > ")))
    }
  } else {
    PREC2_DEFAULT <- trimws(strsplit(o2, ">")[[1]])
    message(sprintf("  PREC2 override (all types): %s", paste(PREC2_DEFAULT, collapse = " > ")))
  }
}

# Evidence strength, strongest first. Used by the weakest-link rule on chains below: a chain
# through an intermediate whose own link is asserted or derived cannot be better than that.
GRADE_RANK <- c(corrected = 5L, sourced = 4L, chained_sourced = 3L, chained = 2L,
                asserted = 1L, derived = 0L)
GRADE <- c(correction = "corrected", scb = "sourced", statskalender = "sourced",
           sfgt = "sourced", register_dated = "sourced", register_undated = "asserted",
           containment = "derived")

# A claim that names no unit cannot produce an answer: 8% of the SFGT pastorat claims name a
# pastorat the source does not have, and if such a claim wins the parish ends up with a
# membership but no polygon (310,000 km2 of apparent pastorship gaps). Keep them as evidence,
# but let a lower-ranked claim that does name a unit fill the years.
# A correction may assert that there was NO parent of a type (Bohuslän had no diocese before
# 1658). It is kept here so it outranks every other claim, and removed after resolution.
claims$none_claim <- claims$source == "correction" & is.na(claims$parent_unit)
unnamed <- sum(is.na(claims$parent_unit) & !claims$none_claim)
claims <- claims %>% filter(!is.na(parent_unit) | none_claim) %>%
  mutate(parent_unit = ifelse(none_claim, "NONE", parent_unit))
message(sprintf("  claims naming no unit, set aside: %d", unnamed))

# Clip every claim to the parent unit's own lifetime. A source may assert a link that outlives
# the unit (SFGT keeps a parish in "Bunkeflo pastorat 1862-1976" into the 1980s); without this
# the unit gets versions after it ended.
life <- bind_rows(
  m1$units %>% transmute(parent_unit = unit_id, u_start = as.integer(format(start_date, "%Y")),
                         u_end = as.integer(format(end_date, "%Y"))),
  if (!is.null(m3a)) m3a$mun_units %>%
    transmute(parent_unit = mun_id, u_start = as.integer(format(start_date, "%Y")),
              u_end = as.integer(format(end_date, "%Y"))) else NULL,
  if (!is.null(m3c)) m3c$court_units %>%
    transmute(parent_unit = court_id, u_start = as.integer(format(start_date, "%Y")),
              u_end = as.integer(format(end_date, "%Y"))) else NULL) %>%
  filter(!is.na(u_start), !is.na(u_end)) %>% distinct(parent_unit, .keep_all = TRUE)
n0 <- nrow(claims)
claims <- claims %>% left_join(life, by = "parent_unit") %>%
  mutate(start = ifelse(is.na(u_start), start, pmax(start, u_start)),
         end   = ifelse(is.na(u_end),   end,   pmin(end,   u_end))) %>%
  filter(start <= end) %>% select(-u_start, -u_end)
message(sprintf("  claims clipped to the parent's lifetime: %d dropped", n0 - nrow(claims)))
claims <- claims %>%
  mutate(src = case_when(source == "register" & dated ~ "register_dated",
                         source == "register" & !dated ~ "register_undated",
                         TRUE ~ source))

# ---- Interval painting -------------------------------------------------------------------
# Highest precedence first; a claim only fills years still open.
`%||%` <- function(a, b) if (is.null(a)) b else a
paint_group <- function(d, order_src){
  d <- d[d$src %in% order_src, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  # Within one precedence level, the MOST SPECIFIC claim wins: shortest span first, and on a tie
  # the larger share. Longest-first did the opposite and a vague claim swallowed a precise one --
  # the register gives Alboga parish to Ulricehamns fogderi for 1946-1951 and to Boras fogderi for
  # 1918-1966, and the second, being longer, took the whole of it. 939 fully dated claims won zero
  # years that way, 295 of them beaten by a claim running to 1990 or from 1600. For containment the
  # share is the point: 25 atoms were given a hundred holding half of them while one holding all of
  # them was on the list.
  d <- d[order(match(d$src, order_src), d$end - d$start, -(d$share %||% 0)), , drop = FALSE]
  filled <- rep(NA_integer_, Y1 - Y0 + 1L)
  for (i in seq_len(nrow(d))) {
    ix <- (max(d$start[i], Y0) - Y0 + 1L):(min(d$end[i], Y1) - Y0 + 1L)
    ix <- ix[is.na(filled[ix])]
    if (length(ix)) filled[ix] <- i
  }
  k <- which(!is.na(filled))
  if (!length(k)) return(NULL)
  brk <- c(0, which(diff(k) != 1 | diff(filled[k]) != 0), length(k))
  do.call(rbind, lapply(seq_len(length(brk) - 1), function(b){
    seg <- k[(brk[b] + 1):brk[b + 1]]
    i <- filled[seg[1]]
    data.frame(parent_unit = d$parent_unit[i], parent_name = d$parent_name[i],
               start = seg[1] + Y0 - 1L, end = seg[length(seg)] + Y0 - 1L,
               source = d$src[i], share = d$share[i],
               # the parish-to-intermediate source, for the weakest-link grade on chains; pass 1
               # has no such column, so it is NA there
               low_src = if (is.null(d$low_src)) NA_character_ else d$low_src[i],
               stringsAsFactors = FALSE)
  }))
}

resolve_type <- function(ty, order_src){
  d <- claims %>% filter(parent_type == ty)
  if (!nrow(d)) return(NULL)
  sp <- split(d, d$atom_id)
  out <- lapply(names(sp), function(a){
    r <- paint_group(sp[[a]], order_src)
    if (is.null(r)) return(NULL)
    r$atom_id <- a; r
  })
  out <- bind_rows(out)
  if (!nrow(out)) return(NULL)
  out$parent_type <- ty
  out
}

message("  Pass 1: direct parents")
direct <- bind_rows(lapply(names(PREC), function(ty){
  r <- resolve_type(ty, PREC[[ty]])
  if (!is.null(r)) message(sprintf("    %-14s %6d rows for %5d atoms", ty, nrow(r),
                                   n_distinct(r$atom_id)))
  r
}))

# ---- Pass 2: parents reached through a direct parent ---------------------------------------
# Chain links between higher units (register_chain), plus the statskalender courts if present
chain <- m3$evidence %>% filter(source == "register_chain", is.na(atom_id)) %>%
  transmute(child_unit, parent_unit, parent_type, start, end, src = "register_chain")
if (!is.null(m3c)) {
  # The register does not have every court the statskalender prints (Karlskoga domsaga, the
  # Eskilstuna and Lund rådhusrätter, 30 in all). Such a court is its own unit, identified by its
  # court_id: m4 builds its polygon from the hundreds that make it up, exactly as it builds a
  # diocese from its contracts, so the parishes below it get a court instead of none.
  chain <- bind_rows(chain, m3c$court_evidence %>%
    transmute(child_unit, parent_unit = coalesce(court_unit, court_id),
              parent_type = "magistrates_court",
              start = as.integer(format(start_date, "%Y")), end = as.integer(format(end_date, "%Y")),
              src = "statskalender") %>% filter(!is.na(child_unit), !is.na(parent_unit)))
  message(sprintf("  statskalender court links: %d", sum(chain$src == "statskalender")))

  # And the court -> court of appeal links the statskalender prints (m3c$court_hovratt). m3c
  # built them and nothing read them, so every court of appeal came from the register chain,
  # which is where the Gotland error lives (Göta until 1947 for an island that has been under
  # Svea since 1645). The statskalender names the hovrätt of every court in each edition.
  hov_name <- c(Svea = "Svea hovrätt", "Göta" = "Göta hovrätt",
                "Skåne och Blekinge" = "Hovrätten över Skåne och Blekinge",
                "Övre Norrland" = "Hovrätten för Övre Norrland",
                "Nedre Norrland" = "Hovrätten för Nedre Norrland",
                "Västra Sverige" = "Hovrätten för Västra Sverige")
  coa <- m1$units %>% filter(type_id == "court_of_appeal") %>% select(unit_id, name)
  cu <- m3c$court_units %>% transmute(court_id, court_unit = coalesce(m1_unit, court_id))
  hv <- m3c$court_hovratt %>%
    mutate(coa_name = unname(hov_name[hovratt])) %>%
    inner_join(coa, by = c("coa_name" = "name")) %>%
    inner_join(cu, by = "court_id") %>%
    transmute(child_unit = court_unit, parent_unit = unit_id,
              parent_type = "court_of_appeal",
              start = as.integer(format(start_date, "%Y")),
              end = as.integer(format(end_date, "%Y")), src = "statskalender") %>%
    filter(!is.na(child_unit), !is.na(parent_unit))
  chain <- bind_rows(chain, hv)
  message(sprintf("  statskalender court -> court of appeal links: %d (%d of %d hovrätt rows resolved)",
                  nrow(hv), nrow(hv), nrow(m3c$court_hovratt)))
}

# The statskalender's own chain: a pastorat's kontrakt and a kontrakt's stift, so a parish the name
# matching could not place still reaches its kontrakt through the pastorat the SFGT gives it.
if (!is.null(m3d) && nrow(m3d$chain)) {
  chain <- bind_rows(chain, m3d$chain %>% select(child_unit, parent_unit, parent_type, start, end,
                                                 src))
  message(sprintf("  statskalender ecclesiastical chain links: %d", nrow(m3d$chain)))
}

# The chain needs the same lifetime clip the claims get. Without it a chain link hands a parish to
# a unit that did not exist yet: Skelleftea domsaga, which the register dates 1967-1970, was given
# to parishes from 1866, and m4 then built a polygon for it in those years. 2,412 memberships over
# 2,054 atoms, graded chained_sourced, all of them "the date the source began recording, not the
# date the unit existed" -- the error this rebuild exists to remove.
if (nrow(chain)) {
  nc0 <- nrow(chain)
  chain <- chain %>% left_join(life, by = "parent_unit") %>%
    mutate(start = ifelse(is.na(u_start), start, pmax(start, u_start)),
           end   = ifelse(is.na(u_end),   end,   pmin(end,   u_end))) %>%
    filter(start <= end) %>% select(-u_start, -u_end)
  message(sprintf("  chain links clipped to the parent's lifetime: %d dropped", nc0 - nrow(chain)))
}

# Types whose source polygons are too unreliable for containment to be used at all. The stift
# polygons are the oversized ones the 1.1.1 audit found (Västerås covering 29% of Sweden), and
# the model's own evidence puts Bromma inside Visby stift and Bollebygd inside Lunds stift with
# share 1.0. A missing diocese is better than a confident wrong one.
NO_CONTAINMENT <- "diocese"
claims <- claims %>% filter(!(parent_type %in% NO_CONTAINMENT & src == "containment"))
message(sprintf("  containment claims dropped for %s", paste(NO_CONTAINMENT, collapse = ", ")))

VIA <- list(county = c("municipality", "bailiwick"),
            magistrates_court = "hundred",
            district_court = "municipality",
            court_of_appeal = c("magistrates_court", "district_court"),
            contract = "pastorship",      # the statskalender names the kontrakt of every pastorat
            diocese = "contract")

message("  Pass 2: parents through a chain")
derived <- list()
# A type that is also in PREC is resolved twice: pass 1's answer is discarded at the end (the
# filter on names(VIA) below), but it used to stay in the accumulating `resolved` that later types
# chain through. When diocese chained through contract it therefore read pass-1 AND pass-2 contract
# rows together -- 16,617 instead of 8,057, with atoms carrying two conflicting contract parents.
# Each type chains through the finished answer for the intermediate: pass 2's if it has one, pass
# 1's otherwise.
final_for <- function(via) if (!is.null(derived[[via]])) derived[[via]] else
  direct %>% filter(parent_type == via)
for (ty in names(VIA)) {
  cl <- list()
  for (via in VIA[[ty]]) {
    base <- final_for(via)
    if (!nrow(base)) next
    lk <- chain %>% filter(parent_type == ty)
    if (!nrow(lk)) next
    j <- base %>% select(atom_id, mid = parent_unit, b_start = start, b_end = end,
                         b_src = source) %>%
      inner_join(lk, by = c("mid" = "child_unit"), relationship = "many-to-many") %>%
      mutate(s = pmax(b_start, start), e = pmin(b_end, end)) %>% filter(s <= e) %>%
      # keep which source the chain link came from: a statskalender court link must outrank a
      # register one, and the grade has to say which was used
      transmute(atom_id, parent_unit, parent_name = NA_character_, start = s, end = e,
                src = paste0("via_", via, "_", src), share = NA_real_, low_src = b_src)
    cl[[via]] <- j
  }
  cl <- bind_rows(cl)
  ct <- claims %>% filter(parent_type == ty)              # direct claims for this type too
  all_cl <- bind_rows(
    if (nrow(cl)) cl %>% transmute(atom_id, parent_unit, parent_name, start, end, src, share,
                                   low_src) else NULL,
    if (nrow(ct)) ct %>% transmute(atom_id, parent_unit, parent_name, start, end, src, share,
                                   low_src = NA_character_) else NULL)
  if (!nrow(all_cl)) { message(sprintf("    %-18s no claims", ty)); next }
  vias <- unlist(VIA[[ty]])
  # pass 2's order, with VIA_STK and VIA_CHAIN standing for the chains through each intermediate
  # type; SWEHIST_PREC2 overrides it the same way as SWEHIST_PREC does pass 1
  tmpl <- PREC2[[ty]]
  if (is.null(tmpl)) tmpl <- PREC2_DEFAULT
  ordv <- unlist(lapply(tmpl, function(t)
    if (t == "VIA_STK") paste0("via_", vias, "_statskalender")
    else if (t == "VIA_CHAIN") paste0("via_", vias, "_register_chain") else t))
  sp <- split(all_cl, all_cl$atom_id)
  r <- bind_rows(lapply(names(sp), function(a){
    x <- paint_group(sp[[a]], ordv); if (is.null(x)) return(NULL); x$atom_id <- a; x }))
  if (nrow(r)) { r$parent_type <- ty; derived[[ty]] <- r
    message(sprintf("    %-18s %6d rows for %5d atoms", ty, nrow(r), n_distinct(r$atom_id))) }
}
# Pass 2 re-resolves its types from the chains AND their direct claims, so the pass-1 rows for
# those types would be duplicates.
membership <- bind_rows(direct %>% filter(!parent_type %in% names(VIA)), bind_rows(derived)) %>%
  filter(parent_unit != "NONE") %>%                     # the "no parent" assertions, now honoured
  # A chain is only as good as its weakest link (see the header). Until 2026-09-29 the grade was
  # read off the UPPER step alone, so a diocese reached through a deanery that itself rests on
  # containment was still called chained_sourced: 99.9% of chained_sourced diocese parish-years and
  # 79% of chained_sourced judicial-district years rested on a derived or asserted lower link.
  # `low_src` carries the parish-to-intermediate source through the join, and the chain now takes
  # the weaker of the two grades.
  mutate(up_grade = unname(ifelse(startsWith(source, "via_"),
                                  ifelse(grepl("_statskalender$", source), "chained_sourced", "chained"),
                                  GRADE[source])),
         up_grade = ifelse(is.na(up_grade), "derived", up_grade),
         low_grade = unname(GRADE[low_src]),
         # a lower link that is corrected or sourced always outranks a chain grade, so only an
         # asserted or derived lower link pulls the chain down to itself
         grade = ifelse(startsWith(source, "via_") & !is.na(low_grade) &
                          GRADE_RANK[low_grade] < GRADE_RANK[up_grade], low_grade, up_grade)) %>%
  select(-up_grade, -low_grade) %>%
  left_join(atoms %>% select(atom_id, child_unit = unit_id), by = "atom_id") %>%
  select(atom_id, child_unit, parent_type, parent_unit, parent_name, start, end, source, grade,
         share) %>%
  arrange(atom_id, parent_type, start)

# ---- Fill the gaps a source's own dating leaves -------------------------------------------------
# A link is dated by the record it comes from, and the records of two levels do not always meet:
# 47 parishes reach Kristianstads domsaga (1967-1970) only in 1970, because that is where their
# hundred's link is dated, so the court's 1967-1969 version was built from the one parish whose
# link is dated from 1967 — a court of 3,800 km2 reduced to 15 for three years. Where an atom has
# no parent of a type for a stretch, the parent it has on the other side of the gap exists then,
# and nothing else claims those years, the parent is extended over the gap. It fills a hole in one
# unit's own lifetime; it never carries a parent beyond the unit, and a year a source speaks about
# is never touched, because the gap is by definition a year no claim covered.
life_all <- life %>% rename(pu = parent_unit)
mb <- membership %>% arrange(atom_id, parent_type, start)
heal <- mb %>% group_by(atom_id, parent_type) %>%
  mutate(prev_end = lag(end), prev_parent = lag(parent_unit), prev_source = lag(source),
         prev_grade = lag(grade)) %>% ungroup() %>%
  filter(!is.na(prev_end), start > prev_end + 1L)            # a gap before this row
n_filled <- 0L; filled <- NULL
if (nrow(heal)) {
  # extend backwards when this row's parent already existed in the gap, otherwise forwards when
  # the previous row's parent still existed
  heal <- heal %>% mutate(g_start = prev_end + 1L, g_end = start - 1L)
  back <- heal %>% left_join(life_all, by = c("parent_unit" = "pu")) %>%
    filter(!is.na(u_start), u_start <= g_start) %>%
    transmute(atom_id, child_unit, parent_type, parent_unit, parent_name,
              start = g_start, end = g_end, source, grade, share)
  fwd <- heal %>% left_join(life_all, by = c("prev_parent" = "pu")) %>%
    filter(!is.na(u_end), u_end >= g_end) %>%
    transmute(atom_id, child_unit, parent_type, parent_unit = prev_parent, parent_name,
              start = g_start, end = g_end, source = prev_source, grade = prev_grade, share)
  # one filler per gap: backwards first (the unit that follows is the one the source names for the
  # years after the gap, and a gap before a unit's first dated link is the common case)
  key <- function(d) paste(d$atom_id, d$parent_type, d$start, d$end)
  fwd <- fwd[!key(fwd) %in% key(back), ]
  fill <- bind_rows(back, fwd) %>% filter(start <= end) %>%
    mutate(source = paste0(source, "+gap"), grade = "asserted")
  if (nrow(fill)) {
    membership <- bind_rows(membership, fill) %>% arrange(atom_id, parent_type, start)
    n_filled <- nrow(fill); filled <- fill
  }
}
message(sprintf("  gaps filled by extending the parent over them: %d rows (%d atoms)",
                n_filled, if (is.null(filled)) 0L else n_distinct(filled$atom_id)))
if (!is.null(filled))
  print(as.data.frame(filled %>% count(parent_type, name = "rows") %>% arrange(desc(rows))),
        row.names = FALSE)

# ---- Parish parts ---------------------------------------------------------------------------
# Known cases where part of a parish belonged elsewhere. The sources name the farms, not a
# boundary, so a part is recorded as a flagged membership row, not as split geometry.
parts <- bind_rows(
  m3s$parts %>% transmute(unit_id, parent_type = "county", parent_name = county_name,
                          start, end, source = "sfgt", note = notes),
  if (!is.null(m3a) && !is.null(m3a$report$parishes_split_between_municipalities))
    m3a$report$parishes_split_between_municipalities %>%
      mutate(parent_type = "municipality", source = "scb") else NULL
)
message(sprintf("  parish parts recorded (flagged, geometry not split): %d", nrow(parts)))

# ---- Report ---------------------------------------------------------------------------------
cover <- membership %>% group_by(parent_type, grade) %>%
  summarise(rows = n(), atoms = n_distinct(atom_id), .groups = "drop")
print(cover %>% tidyr::pivot_wider(names_from = grade, values_from = c(rows, atoms),
                                   values_fill = 0) %>% as.data.frame(), row.names = FALSE)
years <- c(1650, 1700, 1750, 1800, 1850, 1880, 1900, 1930, 1950, 1970, 1990)
byyear <- bind_rows(lapply(years, function(y){
  live <- atoms$atom_id[atoms$start <= y & atoms$end >= y]
  membership %>% filter(start <= y, end >= y, atom_id %in% live) %>%
    group_by(parent_type) %>% summarise(share = n_distinct(atom_id) / length(live), .groups = "drop") %>%
    mutate(year = y)
}))
print(byyear %>% tidyr::pivot_wider(names_from = year, values_from = share) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame(), row.names = FALSE)
timer_end(t0)

message("  --- m3b checks ---")
# A row-count floor cannot fail (84,952 against 20,000), so it says nothing. What can fail: every
# membership must lie inside the parent unit's OWN lifetime. No check in checks/ can see this,
# because they all test against boundaries$start/end, which m4 derives from this very membership --
# the circularity that let 2,412 rows put parishes under units that did not exist yet.
oob <- membership %>% left_join(life, by = "parent_unit") %>%
  filter(!is.na(u_start), start < u_start | end > u_end)
if (nrow(oob)) {
  ex <- oob %>% left_join(m1$units %>% select(parent_unit = unit_id, pname = name),
                          by = "parent_unit") %>% head(3)
  message(sprintf("    e.g. %s", paste(sprintf("%s %d-%d given for %d-%d", ex$pname, ex$u_start,
                                               ex$u_end, ex$start, ex$end), collapse = "; ")))
}
check(nrow(oob) == 0, sprintf("every membership lies inside its parent's lifetime (%d do not)",
                              nrow(oob)))
check(nrow(membership) > 20000, sprintf("membership rows > 20000 (got %d)", nrow(membership)))
check(all(membership$start <= membership$end), "every membership has start <= end")
ovl <- membership %>% group_by(atom_id, parent_type) %>% arrange(start, .by_group = TRUE) %>%
  mutate(bad = !is.na(lag(end)) & start <= lag(end)) %>% ungroup() %>% filter(bad)
check(nrow(ovl) == 0, sprintf("no atom has two parents of one type at a date (got %d)", nrow(ovl)))
# The statskalender's first edition opens backwards (m3c, m3d): a statement printed in 1866 is
# carried to 1600 because nothing earlier was read. That is a reasonable way to fill the years, but
# it is not what the edition says, and grading it `sourced` is the very error this model exists to
# remove - a later state copied back over a unit's whole life. 49% of the parish-years of diocese
# links sourced from the statskalender, and 80% of the pastorate years, lie before 1866. The years
# before the first edition are therefore split off and graded `asserted`: the membership is
# unchanged, only the claim made about the evidence for it.
STK_FIRST <- 1866L
stk <- grepl("statskalender", membership$source, fixed = TRUE)
back <- stk & membership$start < STK_FIRST
if (any(back)) {
  pre  <- membership[back, ] %>% mutate(end = pmin(end, STK_FIRST - 1L), grade = "asserted")
  post <- membership[back, ] %>% mutate(start = STK_FIRST) %>% filter(start <= end)
  membership <- bind_rows(membership[!back, ], pre, post)
  message(sprintf("  statskalender before %d regraded asserted: %d rows split, %d wholly before",
                  STK_FIRST, sum(back), nrow(pre) - nrow(post)))
}

saveRDS(list(membership = membership, parts = parts, precedence = PREC,
             coverage = cover, by_year = byyear), "data-raw/model/out/m3b.rds")
message("  Saved data-raw/model/out/m3b.rds")
