#' m4: Derive the compatibility datasets from atoms + membership
#'
#' Every higher unit is the union of its member parishes for each period in which that set is
#' constant, so the levels line up with each other by construction: no copied polygons, no
#' slivers between a parish and its hundred, and the Skåne correction (applied to the atoms
#' before unioning) reaches every type at once.
#'
#' Usage:  Rscript data-raw/model/m4_derive.R [--no-relations]
#' Input:  data-raw/model/out/{m1,m2,m1b,m3b}.rds, data-raw/model/skane_transform.R
#' Output: data-raw/model/out/m4.rds and out/pkg/data/{boundaries,hierarchy,relations,sweden}.rda
#' (out/pkg is a package tree whose R/ and DESCRIPTION link to the real ones, so the
#' package can be checked against this build with SWEHIST_ROOT=data-raw/model/out/pkg)

source("data-raw/build_helpers.R")
source("data-raw/model/model_helpers.R")
source("data-raw/model/skane_transform.R")
args <- commandArgs(TRUE)
m1 <- readRDS("data-raw/model/out/m1.rds"); m2 <- readRDS("data-raw/model/out/m2.rds")
m1b <- readRDS("data-raw/model/out/m1b.rds"); m3b <- readRDS("data-raw/model/out/m3b.rds")
hasf <- function(f) file.exists(file.path("data-raw/model/out", f))
m3a <- if (hasf("m3a.rds")) readRDS("data-raw/model/out/m3a.rds") else NULL
m3c <- if (hasf("m3c.rds")) readRDS("data-raw/model/out/m3c.rds") else NULL
t0 <- timer_start("m4: derive boundaries, hierarchy, relations")
units <- m1$units
membership <- m3b$membership

# ---- Atoms, corrected -----------------------------------------------------------------------
# The source is EPSG:3021; the model works in 3006 from here on, so the Skåne correction and
# every union happen in the published CRS.
atoms <- st_transform(m2$atoms, 3006)
st_geometry(atoms) <- skane_transform(st_geometry(atoms))
message(sprintf("  To EPSG:3006 and Skåne correction applied to %d atoms", nrow(atoms)))

# ---- Exclaves the source dropped ------------------------------------------------------------
# 80 detached parts, 633 km2, that Lantmäteriet's boundaries give to parish X and the Riksarkivet
# polygons give to the parish around them (data/exclave_pieces.gpkg, made by
# data/make_exclave_pieces.R; evidence in corrections_boundary.csv: the owner's area
# plus the exclave equals the 1999 distrikt area, and census areas from counties that were not
# re-measured already include it, so the source dropped the part rather than the boundary moving).
# The piece is moved for the whole time both parishes exist: these are jordebok irregularities of
# the 17th-19th centuries, and where they can be dated they are older than 1882.
as_mp <- function(g) {                    # st_difference can give a collection; keep the polygons
  g <- tryCatch(st_make_valid(g), error = function(e) g)
  g <- tryCatch(polygons_only(g), error = function(e) g)
  g
}
f_ex <- "data-raw/model/data/exclave_pieces.gpkg"
if (file.exists(f_ex)) {
  ex <- st_read(f_ex, quiet = TRUE) %>% st_transform(3006)
  pt <- st_as_sf(st_drop_geometry(ex)[, c("owner_x", "owner_y")],
                 coords = c("owner_x", "owner_y"), crs = 3006)
  a90 <- which(atoms$start <= 1990 & atoms$end >= 1990)
  own <- st_within(pt, atoms[a90, ])
  gex <- st_geometry(ex); names(gex) <- NULL
  gA <- st_geometry(atoms)
  moved <- 0; n_ok <- 0L; skip <- character()
  for (i in seq_len(nrow(ex))) {
    if (!length(own[[i]])) { skip <- c(skip, ex$distrikt[i]); next }
    u <- atoms$unit_id[a90[own[[i]][1]]]
    pg <- gex[i]
    k_own <- which(atoms$unit_id == u)
    yrs <- range(c(atoms$start[k_own], atoms$end[k_own]))
    # every atom that covers the piece while the owner exists loses it
    k_host <- which(atoms$unit_id != u & atoms$start <= yrs[2] & atoms$end >= yrs[1] &
                      lengths(st_intersects(gA, pg)) > 0)
    if (!length(k_host)) { skip <- c(skip, paste0(ex$distrikt[i], " (no host)")); next }
    for (k in k_host) {
      g2 <- tryCatch(st_difference(gA[k], pg), error = function(e) gA[k])
      if (!length(g2) || st_is_empty(g2)) next
      gA[k] <- as_mp(g2)
    }
    for (k in k_own) {
      g2 <- tryCatch(st_union(gA[k], pg), error = function(e) gA[k])
      gA[k] <- as_mp(g2)
    }
    moved <- moved + ex$km2[i]; n_ok <- n_ok + 1L
  }
  st_geometry(atoms) <- gA
  atoms$km2 <- as.numeric(st_area(atoms)) / 1e6
  message(sprintf("  Exclaves restored: %d of %d pieces, %.0f km2%s", n_ok, nrow(ex), moved,
                  if (length(skip)) sprintf(" (not applied: %s)",
                                            paste(unique(skip), collapse = ", ")) else ""))
}
ga <- st_geometry(atoms); names(ga) <- NULL
atom_ix <- setNames(seq_len(nrow(atoms)), atoms$atom_id)

name_at <- bind_rows(
  m1$unit_names %>% filter(kind == "official") %>%
    transmute(unit_id, nm = name, ns = as.integer(format(start_date, "%Y")),
              ne = as.integer(format(end_date, "%Y"))),
  if (!is.null(m1b$unit_names_extra))
    m1b$unit_names_extra %>% filter(kind == "official") %>%
      transmute(unit_id, nm = name, ns = as.integer(format(start_date, "%Y")),
                ne = as.integer(format(end_date, "%Y"))) else NULL) %>%
  filter(!is.na(nm))
# Year in which a unit's official name changes, so a version never straddles a rename
name_breaks <- name_at %>% arrange(unit_id, ns) %>% group_by(unit_id) %>%
  summarise(brk = list(sort(unique(ns[-1]))), .groups = "drop")
name_brk <- setNames(name_breaks$brk, name_breaks$unit_id)

# ---- Higher types: union of member atoms per stable period ----------------------------------
build_type <- function(ty){
  mb <- membership %>% filter(parent_type == ty, !is.na(parent_unit))
  if (!nrow(mb)) return(NULL)
  sp <- split(mb, mb$parent_unit)
  res <- lapply(names(sp), function(pu){
    d <- sp[[pu]]
    # A version must not span a year the unit was renamed, or one of the two names is wrong for
    # part of it: Rekarne fögderi was "Tredje fögderi (D-län)" until 1885, and a single 1720-1966
    # version was labelled with whichever name overlapped it longest — the older one — so the data
    # said "Tredje fögderi" for 1903. 81 features of 7 types had a name wrong for part of their
    # period this way. The rename years break the intervals, and the merge below keeps them apart.
    brk <- name_brk[[pu]]
    brk <- brk[!is.na(brk) & brk > min(d$start) & brk <= max(d$end)]
    pts <- sort(unique(c(d$start, d$end + 1L, brk)))
    iv <- data.frame(s = head(pts, -1), e = tail(pts, -1) - 1L)
    iv <- iv[iv$s <= iv$e, , drop = FALSE]
    if (!nrow(iv)) return(NULL)
    mem <- lapply(seq_len(nrow(iv)), function(i)
      sort(d$atom_id[d$start <= iv$s[i] & d$end >= iv$e[i]]))
    keep <- lengths(mem) > 0
    iv <- iv[keep, , drop = FALSE]; mem <- mem[keep]
    if (!nrow(iv)) return(NULL)
    key <- vapply(mem, paste, character(1), collapse = ",")
    # ... and two intervals with the same members are one version only if no rename separates them
    grp <- cumsum(c(TRUE, key[-1] != head(key, -1) | iv$s[-1] != head(iv$e, -1) + 1L |
                      iv$s[-1] %in% brk))
    do.call(rbind, lapply(unique(grp), function(g){
      k <- which(grp == g)
      data.frame(parent_unit = pu, start = min(iv$s[k]), end = max(iv$e[k]),
                 members = key[k[1]], n_members = length(mem[[k[1]]]),
                 stringsAsFactors = FALSE)
    }))
  })
  r <- bind_rows(res)
  if (!nrow(r)) return(NULL)
  g <- do.call(c, lapply(seq_len(nrow(r)), function(i){
    ix <- atom_ix[strsplit(r$members[i], ",")[[1]]]
    st_union(ga[ix[!is.na(ix)]])
  }))
  # A new version only where the TERRITORY changes. The member set changes whenever any member
  # parish splits or is renamed, but the union is then the same polygon: without this, Svea
  # hovrätt would get a new version every time any parish in half of Sweden changed.
  out <- st_sf(r, geometry = g)
  keep <- rep(TRUE, nrow(out))
  for (pu in unique(out$parent_unit)) {
    k <- which(out$parent_unit == pu)
    if (length(k) < 2) next
    k <- k[order(out$start[k])]
    prev <- k[1]
    for (i in k[-1]) {
      # not across a year in which the unit was renamed: otherwise one version spans both names
      # and a map of 1980 shows "Bunkeflo pastorat" for what was Oxie pastorat from 1977
      renamed_between <- any(name_brk[[pu]] > out$end[prev] & name_brk[[pu]] <= out$end[i])
      same <- out$end[prev] + 1L == out$start[i] && !renamed_between &&
        tryCatch(as.numeric(sum(st_area(st_sym_difference(g[prev], g[i])))) < 1e4,
                 error = function(e) FALSE)
      if (same) { out$end[prev] <- out$end[i]; keep[i] <- FALSE } else prev <- i
    }
  }
  out <- out[keep, ]
  out$type_id <- ty
  message(sprintf("    %-18s %5d features for %4d units", ty, nrow(out), n_distinct(out$parent_unit)))
  out
}
higher_types <- setdiff(unique(membership$parent_type), "parish")
message("  Building higher types from their parishes")
higher <- bind_rows(lapply(higher_types, build_type))
higher$from_source <- FALSE

# ---- Units with a source polygon that no membership reached -----------------------------------
# A unit is built from its member parishes, so a unit whose members were never established would
# simply vanish — 333 of them in the first full build, among them Tingsryds kommun 1971-1990,
# Bodens domsaga and 175 pastorat, most of them in Norrland where the register's links are
# thinnest. Dropping a unit that the source has, silently, is worse than the imprecision the
# model exists to remove, so such a unit keeps its source polygon, cut against the units of its
# own type that were built from members (which win, so no type overlaps itself), and says so in
# `geometry_from`. A check counts them, because each one is a piece of missing membership.
miss <- units %>% filter(type_id %in% higher_types, !unit_id %in% higher$parent_unit)
if (nrow(miss)) {
  rec <- m1$records %>% filter(unit_id %in% miss$unit_id, type_id %in% higher_types)
  if (nrow(rec)) {
    rg <- skane_transform(st_geometry(st_transform(rec, 3006)))
    rec$km2 <- as.numeric(st_area(rg)) / 1e6
    ord <- order(-rec$km2)                     # largest first, so slivers are cut from slivers
    add <- vector("list", length(ord)); n_add <- 0L; added_km2 <- 0
    hg <- st_geometry(higher)
    for (i in ord) {
      ty <- rec$type_id[i]; s0 <- rec$start[i]; e0 <- rec$end[i]
      g0 <- rg[i]
      k <- which(higher$type_id == ty & higher$start <= e0 & higher$end >= s0)
      if (length(k)) {
        k <- k[lengths(st_intersects(hg[k], g0)) > 0]
        if (length(k)) g0 <- tryCatch(st_difference(g0, st_union(hg[k])), error = function(e) g0)
      }
      # and against the fallbacks already added for the same type and period
      if (n_add) {
        prev <- Filter(function(z) z$type_id == ty && z$start <= e0 && z$end >= s0,
                       add[seq_len(n_add)])
        if (length(prev)) {
          pg <- do.call(c, lapply(prev, function(z) z$geometry))
          pg <- pg[lengths(st_intersects(pg, g0)) > 0]
          if (length(pg)) g0 <- tryCatch(st_difference(g0, st_union(pg)), error = function(e) g0)
        }
      }
      if (!length(g0) || st_is_empty(g0)) next
      a <- as.numeric(sum(st_area(g0))) / 1e6
      if (a < 1) next                          # what is left is a sliver: not a unit
      n_add <- n_add + 1L
      add[[n_add]] <- list(type_id = ty, parent_unit = rec$unit_id[i], start = s0, end = e0,
                           geometry = as_mp(g0))
      added_km2 <- added_km2 + a
    }
    if (n_add) {
      add <- add[seq_len(n_add)]
      fb <- st_sf(data.frame(parent_unit = vapply(add, `[[`, "", "parent_unit"),
                             start = vapply(add, `[[`, 1L, "start"),
                             end = vapply(add, `[[`, 1L, "end"),
                             members = NA_character_, n_members = 0L,
                             type_id = vapply(add, `[[`, "", "type_id"),
                             from_source = TRUE, stringsAsFactors = FALSE),
                  geometry = do.call(c, lapply(add, `[[`, "geometry")))
      st_crs(fb) <- st_crs(higher)
      higher <- rbind(higher, fb)
      message(sprintf("  units kept from their source polygon (no membership): %d features for %d units, %.0f km2",
                      nrow(fb), n_distinct(fb$parent_unit), added_km2))
      print(as.data.frame(st_drop_geometry(fb) %>% count(type_id, name = "features")),
            row.names = FALSE)
    }
  }
}

# ---- Assemble boundaries ---------------------------------------------------------------------
# Units that exist only in SCB (KS:...) or the statskalender (DS:...) have no source record,
# so their names come from those modules; without this they reach boundaries unnamed.
extra_names <- c(
  if (!is.null(m3a)) setNames(m3a$mun_units$name, m3a$mun_units$mun_id) else NULL,
  if (!is.null(m3c)) setNames(m3c$court_units$name, m3c$court_units$court_id) else NULL)
uname <- c(setNames(units$name, units$unit_id), extra_names)
uname <- uname[!duplicated(names(uname))]
# corrections.csv name rows: the researched name of a unit the sources leave unnamed. A
# municipality is keyed there as SCB:<kommunkod>, so resolve it through m3a's codes.
corr_nm <- read_corrections() %>% filter(kind == "name", !is.na(name))
if (nrow(corr_nm) && !is.null(m3a)) {
  code_to_id <- m3a$mun_units %>%
    tidyr::separate_rows(codes, sep = ";") %>%
    transmute(key = paste0("SCB:", trimws(codes)), mun_id) %>% distinct(key, .keep_all = TRUE)
  fix <- corr_nm %>% inner_join(code_to_id, by = c("topo_id" = "key"))
  if (nrow(fix)) {
    uname[fix$mun_id] <- fix$name
    message(sprintf("  names from corrections.csv (SCB keys): %d units", nrow(fix)))
  }
}
# and the rows keyed by a Riksarkivet topo_id ("Kinnarums landskommun" -> Kinnarumma, the six
# communes renamed after a town). The name is also added to unit_names, so a version gets the
# corrected name for the period it applies to rather than only the unit's headline name.
if (nrow(corr_nm)) {
  t2u <- units %>% tidyr::separate_rows(topo_ids, sep = ";") %>%
    transmute(topo_id = topo_ids, unit_id) %>% filter(!is.na(topo_id))
  fix2 <- corr_nm %>% inner_join(t2u, by = "topo_id")
  if (nrow(fix2)) {
    uname[fix2$unit_id] <- fix2$name
    name_at <- bind_rows(name_at,
      fix2 %>% transmute(unit_id, nm = name, ns = coalesce(start, 1600L),
                         ne = coalesce(end, 1990L)))
    message(sprintf("  names from corrections.csv (topo_id keys): %d units", nrow(fix2)))
  }
}
utopo <- setNames(units$topo_ids, units$unit_id)
ref <- m1b$unit_codes %>% filter(system == "ref_code") %>%
  group_by(unit_id) %>% slice(1) %>% ungroup() %>% select(unit_id, ref_code = code)

par_rows <- atoms %>%
  transmute(unit_id, type_id = "parish", name, start, end, from_source = FALSE) %>%
  mutate(topo_id = sub(";.*", "", utopo[unit_id]))
hi_rows <- higher %>%
  transmute(unit_id = parent_unit, type_id, name = unname(uname[parent_unit]), start, end,
            from_source) %>%
  mutate(topo_id = sub(";.*", "", utopo[unit_id]))

# Name each version by the unit's official name AT that period, not by one name for the whole
# unit. A renamed unit otherwise carries one name across the rename: Bunkeflo pastorat was
# renamed Oxie pastorat in 1977, and m1 correctly joins them into one unit, but naming every
# version "Bunkeflo pastorat 1862-1976" would repeat the very error the model exists to fix.
rename_at <- function(d){
  k <- d %>% mutate(.row = row_number()) %>%
    inner_join(name_at, by = "unit_id", relationship = "many-to-many") %>%
    filter(ns <= end, start <= ne) %>%
    mutate(ov = pmin(end, ne) - pmax(start, ns)) %>%
    group_by(.row) %>% slice_max(ov, n = 1, with_ties = FALSE) %>% ungroup() %>%
    select(.row, nm)
  d$name[k$.row] <- k$nm
  d
}
bnd <- rename_at(rbind(par_rows, hi_rows)) %>% left_join(ref, by = "unit_id")
bnd$name <- clean_unit_name(bnd$name)          # "Ore kommun kommun", "Enångers kommune", spaces
# A municipality that exists only in the SCB lists has no Riksarkivet referenskod. Its identity
# is its kommunkod, and the source encodes exactly that in the first four digits of the
# referenskod ("SE/140700000" = 1407), so give it the same shape: without a key here the
# municipality comparisons cannot find it at all.
if (!is.null(m3a)) {
  scb_key <- m3a$mun_units %>%
    transmute(unit_id = mun_id, code = coalesce(code_last, code_first)) %>%
    filter(!is.na(code)) %>%
    mutate(ref_scb = paste0("SE/", sprintf("%04s", code), "00000"))
  i <- match(bnd$unit_id, scb_key$unit_id)
  fill <- is.na(bnd$ref_code) & !is.na(i)
  bnd$ref_code[fill] <- scb_key$ref_scb[i[fill]]
  message(sprintf("  SCB-only municipalities given a kommunkod ref_code: %d features", sum(fill)))
}
message(sprintf("  features named by the name in force at their period: %d units have >1 official name",
                sum(table(name_at$unit_id) > 1)))
bnd$type <- unname(setNames(names(TYPE_ID_MAP), TYPE_ID_MAP)[bnd$type_id])
bnd$type <- unname(TYPE_MAP[bnd$type])

bnd$geom_id <- assign_frozen_ids(st_drop_geometry(bnd))
n_kept <- sum(bnd$geom_id %in% read_geom_id_lookup()$geom_id)
message(sprintf("  geom_id: %d of %d features keep a frozen id (%.0f%%)",
                n_kept, nrow(bnd), 100 * n_kept / nrow(bnd)))

# ---- kind: the institution, where the type conflates two ---------------------------------------
# Riksarkivet's own types put two institutions in one bucket: "Domsaga / rådhusrätt" holds both
# the rural domsagor and the town courts, the Härad type holds the towns that stood outside the
# härad, and Kommun holds städer, köpingar and landskommuner until 1971. Users had to read the
# names to tell them apart, so the distinction is a column. Courts take
# it from the statskalender where m3c identified the unit, otherwise from the name; a rural court
# with a bare härad name (Ydre, Mo) is a domsaga.
ck <- if (!is.null(m3c)) {
  cu <- m3c$court_units
  k <- c(setNames(cu$court_kind, cu$court_id),
         setNames(cu$court_kind[!is.na(cu$m1_unit)], cu$m1_unit[!is.na(cu$m1_unit)]))
  k <- c(domsaga = "domsaga", radhusratt = "rådhusrätt", tingsratt = "tingsrätt")[k] %>%
    setNames(names(k))
  k
} else character()
kind_of <- function(type_id, name, unit_id, start){
  k <- rep(NA_character_, length(name))
  ct <- type_id %in% c("magistrates_court", "district_court")
  if (any(ct)) {
    n <- name[ct]
    v <- ifelse(grepl("rådhusrätt", n), "rådhusrätt",
         ifelse(grepl("tingsrätt", n), "tingsrätt",
         ifelse(grepl("domsaga", n), "domsaga", NA_character_)))
    from_stk <- unname(ck[unit_id[ct]])
    v <- ifelse(!is.na(v), v, ifelse(!is.na(from_stk), from_stk,
         ifelse(grepl("stad|köping", n), "rådhusrätt", "domsaga")))
    k[ct] <- v
  }
  hu <- type_id == "hundred"
  if (any(hu)) {
    n <- name[hu]
    # skeppslag (the coastal boat-levy districts of Roslagen) and bergslag (the mining districts)
    # are hundreds of their own kind, not härader; the source names them and they were called
    # "härad" here until 2026-09-29.
    k[hu] <- ifelse(grepl("tingslag", n), "tingslag",
             ifelse(grepl("skeppslag", n), "skeppslag",
             ifelse(grepl("bergslag", n), "bergslag",
             ifelse(grepl("stad|köping", n), "stad", "härad"))))
  }
  mu <- type_id == "municipality"
  if (any(mu)) {
    n <- name[mu]; y <- start[mu]
    k[mu] <- ifelse(grepl("stad", n), "stad",
             ifelse(grepl("köping", n), "köping",
             ifelse(grepl("municipalsamh", n), "municipalsamhälle",
             ifelse(y >= 1971, "kommun", "landskommun"))))
  }
  k
}
bnd$geometry_from <- ifelse(bnd$type_id == "parish", "source",
                            ifelse(bnd$from_source, "source", "members"))
bnd$kind <- kind_of(bnd$type_id, bnd$name, bnd$unit_id, bnd$start)
message("  kind column: ",
        paste(sprintf("%s %d", names(table(bnd$kind)), table(bnd$kind)), collapse = ", "))

# Closing the holes here was tried on 28 September and reverted. The rule (fill an enclosed hole
# unless another unit of the same type claims it) is sound within a type and creates no overlap, but
# it is applied per type independently, and that breaks the invariants BETWEEN types: a court of
# appeal that absorbs its lakes covers 101.5% of the parish territory the coverage test measures
# against, and a child that fills a hole its parent keeps is no longer inside its parent
# (a3_parent_contains 2 -> 18, a4 0 -> 12, bisos.county_diocese 0.676 -> 0.647, two tests failing).
# The geometry therefore stays the honest union of the members, and callers who want a solid polygon
# for a map ask for one: get_boundaries(..., fill_holes = TRUE), which is where the rule now lives.

boundaries <- bnd %>%
  select(geom_id, unit_id, topo_id, ref_code, name, kind, type, type_id, start, end,
         geometry_from) %>%
  arrange(geom_id) %>% st_cast("MULTIPOLYGON")
# tibble-backed, as the shipped datasets have always been: code that expects a tibble (and the
# print method) sees the difference
class(boundaries) <- c("sf", "tbl_df", "tbl", "data.frame")
# One row with a name, as ?sweden documents it: parish_territory() returns the geometry only, and
# m4 shipped that bare sfc, so data(sweden) had no `name` column and nrow(sweden) was NULL.
# Record the ids: every id in use keeps its unit and period in the frozen lookup, and the new ones
# are added. Without this the next build issues different ids for the same features, which is the
# one thing geom_ids are meant to guarantee (1.1.1 did it in step6; the model had dropped it, and
# 1,653 ids of this build were not in the lookup).
update_geom_id_lookup(st_drop_geometry(boundaries))

sweden <- st_sf(name = "Sweden (parish territory 1600-1990)",
                geometry = parish_territory(boundaries))

# ---- hierarchy --------------------------------------------------------------------------------
gid <- boundaries %>% st_drop_geometry() %>% select(geom_id, unit_id, type_id, start, end)
# From the finished boundaries, not the `ref` table: m4 fills a kommunkod code for the SCB-only
# municipalities after `ref` is built, and a hierarchy row must report what the feature carries.
rc_by_unit <- boundaries %>% st_drop_geometry() %>% filter(!is.na(ref_code)) %>%
  distinct(unit_id, ref_code)
child_gid <- atoms %>% st_drop_geometry() %>% select(atom_id, unit_id, a_start = start, a_end = end) %>%
  left_join(gid %>% filter(type_id == "parish") %>% select(geom_id, unit_id, start, end),
            by = "unit_id", relationship = "many-to-many") %>%
  filter(a_start == start, a_end == end) %>% select(atom_id, child_geom_id = geom_id)
hierarchy <- membership %>%
  inner_join(child_gid, by = "atom_id") %>%
  inner_join(gid %>% select(parent_geom_id = geom_id, parent_unit = unit_id,
                            p_start = start, p_end = end),
             by = "parent_unit", relationship = "many-to-many") %>%
  mutate(s = pmax(start, p_start), e = pmin(end, p_end)) %>% filter(s <= e) %>%
  left_join(rc_by_unit %>% rename(parent_unit = unit_id, parent_ref_code = ref_code),
            by = "parent_unit") %>%
  # The API matches on the display type ("County"), not the type_id ("county")
  # the parent's name AT that version, the same as boundaries: taking the unit's last name put
  # the alias "Dalarnas län" (1997-) on Kopparbergs län in 1880, and everything that reads the
  # hierarchy rather than boundaries saw it
  left_join(bnd %>% st_drop_geometry() %>% select(parent_geom_id = geom_id, pname = name),
            by = "parent_geom_id") %>%
  transmute(parent_geom_id, child_geom_id,
            parent_type = unname(TYPE_MAP[names(TYPE_ID_MAP)[match(parent_type, TYPE_ID_MAP)]]),
            child_type = "Parish",
            parent_ref_code, parent_name = pname, start = s, end = e,
            source = source, grade = grade) %>%
  distinct() %>% arrange(parent_type, start, parent_geom_id)
message(sprintf("  hierarchy: %d parish rows", nrow(hierarchy)))

# Rows between higher levels (County -> Municipality, Diocese -> Contract, ...), so the
# hierarchy is the same lattice 1.1.1 had and get_children() works between any two levels.
# A unit of the lower type belongs to the unit of the upper type that holds most of its
# parishes in that period.
OVER <- list(County = c("Municipality", "Bailiwick"), "Magistrates Court" = "Hundred",
             "Court of Appeal" = c("Magistrates Court", "District Court"),
             Diocese = "Contract", Contract = "Pastorship",
             "District Court" = "Municipality")
mem_by_type <- membership %>% select(atom_id, parent_type, parent_unit, start, end)
Y0 <- 1600L; Y1 <- 1990L
# One parent per child unit per year (the parent holding most of the child's parishes that
# year), then compressed to runs. Taking the majority over each overlapping atom window
# instead produced overlapping intervals, and so a pastorship with two contracts at once.
inter <- bind_rows(lapply(names(OVER), function(up) bind_rows(lapply(OVER[[up]], function(dn){
  up_id <- unname(TYPE_ID_MAP[names(TYPE_MAP)[match(up, TYPE_MAP)]])
  dn_id <- unname(TYPE_ID_MAP[names(TYPE_MAP)[match(dn, TYPE_MAP)]])
  a <- mem_by_type %>% filter(parent_type == dn_id) %>%
    select(atom_id, child_unit = parent_unit, cs = start, ce = end)
  b2 <- mem_by_type %>% filter(parent_type == up_id) %>%
    select(atom_id, up_unit = parent_unit, us = start, ue = end)
  if (!nrow(a) || !nrow(b2)) return(NULL)
  j <- inner_join(a, b2, by = "atom_id", relationship = "many-to-many") %>%
    mutate(s = pmax(cs, us), e = pmin(ce, ue)) %>% filter(s <= e)
  if (!nrow(j)) return(NULL)
  res <- lapply(split(j, j$child_unit), function(d){
    tab <- matrix(0L, nrow = Y1 - Y0 + 1L, ncol = length(unique(d$up_unit)),
                  dimnames = list(NULL, unique(d$up_unit)))
    for (k in seq_len(nrow(d)))
      tab[(d$s[k] - Y0 + 1L):(d$e[k] - Y0 + 1L), d$up_unit[k]] <-
        tab[(d$s[k] - Y0 + 1L):(d$e[k] - Y0 + 1L), d$up_unit[k]] + 1L
    # a real majority, not merely the top candidate: with one parish carrying a diocese and 45
    # not, `which.max` gave the whole kontrakt that diocese (46 Bohuslän parishes chained to
    # Göteborg from a single row)
    tot <- rowSums(tab)
    top <- apply(tab, 1, max)
    best <- ifelse(tot == 0 | top * 2 <= tot, NA_integer_, apply(tab, 1, which.max))
    keep <- which(!is.na(best))
    if (!length(keep)) return(NULL)
    brk <- c(0, which(diff(keep) != 1 | diff(best[keep]) != 0), length(keep))
    do.call(rbind, lapply(seq_len(length(brk) - 1), function(bi){
      seg <- keep[(brk[bi] + 1):brk[bi + 1]]
      data.frame(child_unit = d$child_unit[1], up_unit = colnames(tab)[best[seg[1]]],
                 s = seg[1] + Y0 - 1L, e = seg[length(seg)] + Y0 - 1L,
                 stringsAsFactors = FALSE)
    }))
  })
  r <- bind_rows(res)
  if (!nrow(r)) return(NULL)
  r %>% inner_join(gid %>% select(child_geom_id = geom_id, child_unit = unit_id,
                                  c_start = start, c_end = end),
                   by = "child_unit", relationship = "many-to-many") %>%
    inner_join(gid %>% select(parent_geom_id = geom_id, up_unit = unit_id,
                              p_start = start, p_end = end),
               by = "up_unit", relationship = "many-to-many") %>%
    mutate(s2 = pmax(s, c_start, p_start), e2 = pmin(e, c_end, p_end)) %>% filter(s2 <= e2) %>%
    left_join(bnd %>% st_drop_geometry() %>% select(parent_geom_id = geom_id, pname = name),
              by = "parent_geom_id") %>%
    transmute(parent_geom_id, child_geom_id, parent_type = up, child_type = dn,
              parent_name = pname, start = s2, end = e2,
              source = "derived", grade = "chained")
}))))
# A link between two higher levels is derived from the parishes they share, and a majority of one
# parish's kontrakt can carry its stift to a unit on the other side of the country: a3 found 12
# parishes whose parent does not contain them and a6 17 children reaching outside their parent, all
# of them from this rule. So the child has to lie inside the parent as well as be named by it.
if (!is.null(inter) && nrow(inter)) {
  n0 <- nrow(inter)
  g_by_id <- setNames(seq_len(nrow(bnd)), bnd$geom_id)
  gb <- st_geometry(bnd)
  # one test per distinct pair, not per period, and an area only where the cheap test fails: a
  # child that is covered by its parent needs no intersection at all
  pr <- inter %>% distinct(child_geom_id, parent_geom_id) %>%
    mutate(ci = unname(g_by_id[as.character(child_geom_id)]),
           pi = unname(g_by_id[as.character(parent_geom_id)])) %>%
    filter(!is.na(ci), !is.na(pi))
  pr$inside <- mapply(function(a, b) length(st_covered_by(gb[a], gb[b])[[1]]) > 0, pr$ci, pr$pi)
  pr$share <- ifelse(pr$inside, 1, NA_real_)
  todo <- which(!pr$inside)
  for (i in todo) {
    a <- tryCatch(as.numeric(sum(st_area(st_intersection(gb[pr$ci[i]], gb[pr$pi[i]])))),
                  error = function(e) 0)
    ac <- as.numeric(st_area(gb[pr$ci[i]]))
    pr$share[i] <- if (ac > 0) a / ac else 0
  }
  inter <- inter %>%
    left_join(pr %>% select(child_geom_id, parent_geom_id, share), by = c("child_geom_id", "parent_geom_id")) %>%
    filter(!is.na(share), share >= 0.5) %>% select(-share)
  message(sprintf("  inter-level links dropped for not containing the child: %d of %d (%d pairs tested)",
                  n0 - nrow(inter), n0, nrow(pr)))
}
if (!is.null(inter) && nrow(inter)) {
  # parent_ref_code must be the parent's real code: assign_parent() returns it, and callers
  # read the county letter out of it (a NA here made every municipality county-less, which
  # silently emptied the death-book municipality metrics)
  inter <- inter %>%
    left_join(gid %>% select(parent_geom_id = geom_id, pu = unit_id), by = "parent_geom_id") %>%
    left_join(rc_by_unit %>% rename(pu = unit_id, prc = ref_code), by = "pu") %>%
    mutate(parent_ref_code = prc) %>% select(-pu, -prc) %>%
    distinct(parent_geom_id, child_geom_id, parent_type, child_type, start, end, .keep_all = TRUE)
  hierarchy <- bind_rows(hierarchy, inter) %>% arrange(parent_type, child_type, start)
  message(sprintf("  hierarchy: +%d rows between higher levels, %d type pairs", nrow(inter),
                  nrow(distinct(hierarchy, parent_type, child_type))))
}

# ---- Regiments: hierarchy rows without geometry -------------------------------------------------
# A regiment has no polygon, so it is not a unit here, but the register links parishes to regiments
# and 1.1.1 carried those links (3,813 rows, 107 regiments) with no parent_geom_id. Without them
# get_children(1800, "regiment", "Uppland", "parish") has nothing to find. Each link is attached to
# every version of the parish that coexists with it.
m3 <- if (hasf("m3.rds")) readRDS("data-raw/model/out/m3.rds") else NULL
REGIMENT_FROM <- 1682L   # indelningsverket established
REGIMENT_TO   <- 1901L   # 1901 års härordning ends it
if (!is.null(m3) && !is.null(m3$regiment_links) && nrow(m3$regiment_links)) {
  par_gid <- gid %>% filter(type_id == "parish") %>% select(geom_id, unit_id, start, end)
  reg <- m3$regiment_links %>%
    inner_join(par_gid, by = c("child_unit" = "unit_id"), relationship = "many-to-many") %>%
    # The allotment system (indelningsverket) tied farms to regiments from 1682 until the army
    # order of 1901; the register carries no dates of its own for these links, so before
    # 2026-09-29 they ran the whole life of the parish - 94% of them past 1901 and 3,327 to 1990,
    # which made assign_parent(..., 1950, "regiment") answer with a regiment. Clip to the life of
    # the institution. Parishes whose own period ends before 1682 or begins after 1901 drop out.
    mutate(s = pmax(start.x, start.y, REGIMENT_FROM), e = pmin(end.x, end.y, REGIMENT_TO)) %>%
    filter(s <= e) %>%
    transmute(parent_geom_id = NA_integer_, child_geom_id = geom_id,
              parent_type = "Regiment", child_type = "Parish", parent_ref_code, parent_name,
              start = s, end = e, source = "register", grade = "asserted") %>%
    distinct()
  hierarchy <- bind_rows(hierarchy, reg) %>% arrange(parent_type, child_type, start)
  message(sprintf("  hierarchy: +%d regiment rows for %d regiments (no geometry)", nrow(reg),
                  n_distinct(reg$parent_name)))
}

# ---- relations (successor rule: successor >= 10%% of the smaller unit; transfer >= 5 km2) ---------
relations <- NULL
if (!"--no-relations" %in% args) {
  t1 <- timer_start("  relations")
  b <- boundaries
  b$km2 <- as.numeric(st_area(b)) / 1e6
  rel <- bind_rows(lapply(unique(b$type_id), function(ty){
    x <- b[b$type_id == ty, ]
    ii <- st_intersects(x)
    pr <- do.call(rbind, lapply(seq_along(ii), function(i){
      j <- ii[[i]][ii[[i]] != i]; if (length(j)) cbind(i, j) }))
    if (is.null(pr)) return(NULL)
    pr <- pr[x$end[pr[, 1]] + 1L == x$start[pr[, 2]], , drop = FALSE]   # predecessor -> successor
    if (!nrow(pr)) return(NULL)
    ov <- vapply(seq_len(nrow(pr)), function(k){
      v <- tryCatch(st_area(st_intersection(st_geometry(x)[pr[k, 1]], st_geometry(x)[pr[k, 2]])),
                    error = function(e) 0)
      if (length(v)) as.numeric(sum(v)) / 1e6 else 0 }, numeric(1))
    small <- pmin(x$km2[pr[, 1]], x$km2[pr[, 2]])
    # the column names `relations` has had since 1.1.1: parent_id is the earlier unit, child_id
    # the later one, and transition_year is the parent's last year (the child starts the year
    # after). get_boundaries() and match_units() read exactly these.
    tibble(parent_id = x$geom_id[pr[, 1]], child_id = x$geom_id[pr[, 2]], type_id = ty,
           transition_year = x$end[pr[, 1]], overlap_km2 = ov, share = ov / small) %>%
      filter(overlap_km2 >= 0.01) %>%
      mutate(relation = ifelse(share >= 0.10, "successor",
                        ifelse(overlap_km2 >= 5, "transfer", NA_character_))) %>%
      filter(!is.na(relation))
  }))
  relations <- rel %>%
    select(parent_id, child_id, type_id, transition_year, relation, overlap_km2, share) %>%
    arrange(type_id, parent_id, child_id)
  message(sprintf("  relations: %d successors, %d transfers",
                  sum(relations$relation == "successor"), sum(relations$relation == "transfer")))
  timer_end(t1)
}

# ---- Coverage per type and year -----------------------------------------------------------------
# The share of the parish territory that a type's units cover, which get_boundaries() warns on.
# Same rule as the earlier pipeline: the feature areas inside `sweden` over its area, for every
# year. It is written to out/coverage.csv, and into R/sysdata.rda only when
# SWEHIST_UPDATE_SYSDATA=1, because R/ is shared with the installed 1.1.1 package.
t2 <- timer_start("m4: coverage table")
swe_area <- as.numeric(st_area(sweden))
inside <- suppressWarnings(st_intersection(boundaries[, "geom_id"], st_geometry(sweden)))
a_in <- tapply(as.numeric(st_area(inside)), inside$geom_id, sum)
areas <- unname(dplyr::coalesce(a_in[as.character(boundaries$geom_id)], 0))
coverage <- bind_rows(lapply(split(seq_len(nrow(boundaries)), boundaries$type_id), function(ix){
  tibble(type_id = unname(boundaries$type_id[ix[1]]), year = 1600:1990,
         share = pmin(1, vapply(1600:1990, function(y){
           a <- ix[boundaries$start[ix] <= y & boundaries$end[ix] >= y]
           sum(areas[a]) / swe_area
         }, numeric(1))))
}))
readr::write_csv(coverage, "data-raw/model/out/coverage.csv")
if (identical(Sys.getenv("SWEHIST_UPDATE_SYSDATA"), "1")) {
  .coverage <- coverage
  save(.coverage, file = "R/sysdata.rda", compress = "xz")
  message("  R/sysdata.rda updated with this build's coverage")
}
message("  coverage 1900: ",
        paste(sprintf("%s %.0f%%", coverage$type_id[coverage$year == 1900],
                      100 * coverage$share[coverage$year == 1900]), collapse = ", "))
timer_end(t2)

# ---- Package tree for checking the build -------------------------------------------------------
pkg <- "data-raw/model/out/pkg"
dir.create(file.path(pkg, "data"), recursive = TRUE, showWarnings = FALSE)
for (f in c("R", "DESCRIPTION", "NAMESPACE"))
  if (!file.exists(file.path(pkg, f)))
    file.symlink(normalizePath(f), file.path(pkg, f))
old <- list.files("data", "\\.rda$", full.names = TRUE)          # carry over what m4 does not build
for (f in old) {
  tgt <- file.path(pkg, "data", basename(f))
  if (!basename(f) %in% c("boundaries.rda", "hierarchy.rda", "relations.rda", "sweden.rda") &&
      !file.exists(tgt)) file.copy(f, tgt)
}
# gzip, not xz: this is a working tree for checking the build, not the shipped data
save(boundaries, file = file.path(pkg, "data", "boundaries.rda"), compress = "gzip")
save(hierarchy,  file = file.path(pkg, "data", "hierarchy.rda"),  compress = "gzip")
save(sweden,     file = file.path(pkg, "data", "sweden.rda"),     compress = "gzip")
if (!is.null(relations)) save(relations, file = file.path(pkg, "data", "relations.rda"), compress = "gzip")
message(sprintf("  package tree written: %s", pkg))
timer_end(t0)

message("  --- m4 checks ---")
if (!is.null(relations))
  check(all(c("parent_id", "child_id", "type_id", "transition_year", "relation") %in%
              names(relations)),
        "relations has the columns the package reads (parent_id, child_id, transition_year)")
check(!anyNA(boundaries$geom_id), "every feature has a geom_id")
check(!anyNA(boundaries$name), sprintf("every feature has a name (%d missing)", sum(is.na(boundaries$name))))
check(!anyDuplicated(boundaries$geom_id), "geom_id unique")
check(all(!st_is_empty(boundaries)), "no empty geometry")
check(all(st_is_valid(boundaries)), "all geometries valid")
check(st_crs(boundaries)$epsg == 3006, "CRS is EPSG:3006")
# A row must carry its parent's ref_code whenever the parent has one. Some units exist only
# outside the Riksarkivet source and have no code at all; for those NA is the truth.
with_rc <- bnd %>% filter(!is.na(ref_code)) %>% distinct(parent_geom_id = geom_id)
nrc <- hierarchy %>% filter(is.na(parent_ref_code), parent_type != "Regiment") %>%
  semi_join(with_rc, by = "parent_geom_id") %>% nrow()
no_code <- hierarchy %>% filter(is.na(parent_ref_code)) %>% distinct(parent_name) %>% nrow()
message(sprintf("  hierarchy rows whose parent has no ref_code at all: %d parents", no_code))
check(nrc == 0, sprintf("a row carries the parent's ref_code where the parent has one (%d without)",
                        nrc))
check(all(hierarchy$parent_type %in% TYPE_MAP),
      sprintf("hierarchy parent_type uses display names (bad: %s)",
              paste(setdiff(hierarchy$parent_type, TYPE_MAP), collapse = ", ")))
for (y in c(1700, 1800, 1900, 1990)) {
  n <- sum(boundaries$type_id == "county" & boundaries$start <= y & boundaries$end >= y)
  message(sprintf("  counties at %d: %d", y, n))
}
print(st_drop_geometry(boundaries) %>% count(type_id) %>% as.data.frame(), row.names = FALSE)
saveRDS(list(boundaries = boundaries, hierarchy = hierarchy, relations = relations),
        "data-raw/model/out/m4.rds")
message("  Saved data-raw/model/out/m4.rds")
