#' m3: Membership evidence — every claim about which parent a parish had, and when
#'
#' Every claim is kept, with its source and how well it is dated. Resolution (precedence,
#' conflicts, parish parts) happens in m3b, once the SCB municipalities (m3a) are in.
#'
#' Input:  data-raw/model/out/m1.rds, out/m2.rds
#' data-raw/data/Tbl_topografi_rel.csv, data-raw/sfgt/sfgt_{pastorat,lan}.rda
#' data-raw/model/corrections.csv (kind = membership)
#' Output: data-raw/model/out/m3.rds: list(evidence, containment, register_quality)

source("data-raw/build_helpers.R")
source("data-raw/model/model_helpers.R")
m1 <- readRDS("data-raw/model/out/m1.rds")
m2 <- readRDS("data-raw/model/out/m2.rds")
t0 <- timer_start("m3: membership evidence")
atoms <- m2$atoms
rec <- m1$records
unit_of <- function(topo, type = NULL){
  m <- st_drop_geometry(rec) %>% distinct(topo_id, unit_id, type_id)
  if (!is.null(type)) m <- m %>% filter(type_id == type)
  m$unit_id[match(topo, m$topo_id)]
}

# ---- Register links ----------------------------------------------------------------------
rel_raw <- read_csv("data-raw/data/Tbl_topografi_rel.csv", show_col_types = FALSE) %>%
  filter(Ordning == "Överordnad") %>%
  transmute(parent_topo = kalla_id, child_topo = dest_id,
            open_start = is.na(Start) | Start == 0, open_end = is.na(Slut) | Slut >= 9999,
            rel_start = ifelse(open_start, 1600L, as.integer(Start)),
            rel_end   = ifelse(open_end, 1990L, pmin(as.integer(Slut), 1990L)))
rel <- rel_raw
tmeta <- st_drop_geometry(rec) %>% distinct(topo_id, unit_id, type_id)
rel <- rel %>%
  inner_join(tmeta %>% rename(parent_unit = unit_id, parent_type = type_id),
             by = c("parent_topo" = "topo_id"), relationship = "many-to-many") %>%
  inner_join(tmeta %>% rename(child_unit = unit_id, child_type = type_id),
             by = c("child_topo" = "topo_id"), relationship = "many-to-many")

# Regiments (the register's "Militär indelning") have no polygon, so they are not units here and
# the join above drops their links: 3,826 rows and 107 regiments that the earlier build carried in the
# hierarchy, and get_children(1800, "regiment", ...) with them. They are kept as links, from the
# register's own metadata, and m4 puts them in the hierarchy with no parent_geom_id, as 1.1.1 did.
# (Making them units in m1, from the register rather than from the shapefile, is the proper fix.)
topo_meta <- read_csv("data-raw/data/Tbl_topografi.csv", show_col_types = FALSE) %>%
  transmute(topo_id, ref_code = referenskod, name = iconv(as.character(namn), "utf8", "utf8"),
            typ = as.character(typ))
regiment_links <- rel_raw %>%
  inner_join(topo_meta %>% filter(typ == "Militär indelning") %>%
               transmute(parent_topo = topo_id, parent_name = name, parent_ref_code = ref_code),
             by = "parent_topo") %>%
  inner_join(tmeta %>% filter(type_id == "parish") %>%
               transmute(child_topo = topo_id, child_unit = unit_id),
             by = "child_topo", relationship = "many-to-many") %>%
  transmute(child_unit, parent_name, parent_ref_code, start = rel_start, end = rel_end) %>%
  distinct()
message(sprintf("  regiment links (no geometry, kept for the hierarchy): %d for %d regiments",
                nrow(regiment_links), n_distinct(regiment_links$parent_name)))

register_quality <- rel %>% group_by(parent_type, child_type) %>%
  summarise(links = n(), undated_start = mean(open_start), undated_end = mean(open_end),
            .groups = "drop") %>% arrange(desc(links))
print(as.data.frame(register_quality))

# Parish links: to the atoms of that parish unit that coexist with the claim
par_links <- rel %>% filter(child_type == "parish")
ev_reg <- par_links %>%
  inner_join(st_drop_geometry(atoms) %>% select(atom_id, unit_id, a_start = start, a_end = end),
             by = c("child_unit" = "unit_id"), relationship = "many-to-many") %>%
  mutate(start = pmax(rel_start, a_start), end = pmin(rel_end, a_end)) %>%
  filter(start <= end) %>%
  transmute(atom_id, child_unit, parent_unit, parent_type, start, end,
            source = "register", dated = !(open_start & open_end), share = NA_real_)
message(sprintf("  register parish claims: %d (%.0f%% dated)", nrow(ev_reg), 100 * mean(ev_reg$dated)))

# Links between higher types, kept as unit-level claims (resolved by chaining in m3b)
ev_chain <- rel %>% filter(child_type != "parish", parent_type != "regiment") %>%
  transmute(atom_id = NA_character_, child_unit, parent_unit, parent_type,
            start = rel_start, end = rel_end, source = "register_chain",
            dated = !(open_start & open_end), share = NA_real_)
message(sprintf("  register chain claims: %d", nrow(ev_chain)))

# ---- SFGT ---------------------------------------------------------------------------------
# SFGT rows are keyed by pid, and parish_name is NA in most of them, so the SFGT evidence is
# built in m3s_sfgt.R once m1b has the pid -> unit mapping. (A first draft joined on the name
# here and found only 1,144 of about 8,000 pastorship claims.)

# ---- Containment ---------------------------------------------------------------------------
# Share of each parish atom inside each coexisting source polygon of a higher type. This is the
# slow part of the build (about 25 minutes), and it depends only on the atoms and the source
# records, so it is reused from the previous m3.rds when neither has changed: a change to the
# corrections then costs a minute instead of half an hour. The fingerprint is the number of atoms
# and records, their ids and their total area, so any change to either recomputes it.
fingerprint <- paste(nrow(atoms), nrow(rec), round(sum(atoms$km2)),
                     sum(nchar(atoms$atom_id)), length(unique(rec$topo_id)),
                     round(sum(rec$start) + sum(rec$end)), sep = "|")
prev <- if (file.exists("data-raw/model/out/m3.rds")) readRDS("data-raw/model/out/m3.rds") else NULL
newer_than_inputs <- function(){
  f <- file.mtime("data-raw/model/out/m3.rds")
  all(!is.na(f)) && f > max(file.mtime(c("data-raw/model/out/m1.rds", "data-raw/model/out/m2.rds")))
}
# An m3.rds from before the fingerprint existed is reused when it is newer than m1 and m2.
reuse <- !is.null(prev) && !is.null(prev$containment) &&
  (identical(prev$fingerprint, fingerprint) ||
     (is.null(prev$fingerprint) && newer_than_inputs()))
higher <- setdiff(unique(rec$type_id), c("parish", "regiment"))
containment <- if (reuse) {
  message("  containment reused from out/m3.rds (atoms and records unchanged)")
  prev$containment
} else bind_rows(lapply(higher, function(ty){
  p <- rec %>% filter(type_id == ty)
  if (!nrow(p)) return(NULL)
  ii <- st_intersects(atoms, p)
  pr <- do.call(rbind, lapply(seq_along(ii), function(i)
    if (length(ii[[i]])) cbind(i, ii[[i]])))
  if (is.null(pr)) return(NULL)
  keep <- atoms$start[pr[, 1]] <= p$end[pr[, 2]] & p$start[pr[, 2]] <= atoms$end[pr[, 1]]
  pr <- pr[keep, , drop = FALSE]
  ga <- st_geometry(atoms); gp <- st_geometry(p)
  sh <- vapply(seq_len(nrow(pr)), function(k){
    x <- tryCatch(st_area(st_intersection(ga[pr[k, 1]], gp[pr[k, 2]])), error = function(e) 0)
    if (length(x)) as.numeric(sum(x)) / (atoms$km2[pr[k, 1]] * 1e6) else 0
  }, numeric(1))
  message(sprintf("    %-18s %6d atom-polygon pairs", ty, nrow(pr)))
  tibble(atom_id = atoms$atom_id[pr[, 1]], child_unit = atoms$unit_id[pr[, 1]],
         parent_unit = p$unit_id[pr[, 2]], parent_type = ty,
         start = pmax(atoms$start[pr[, 1]], p$start[pr[, 2]]),
         end = pmin(atoms$end[pr[, 1]], p$end[pr[, 2]]), share = sh)
}))
stopifnot(nrow(containment) > 0)
message(sprintf("  containment pairs: %d (%d with share >= 0.5)",
                nrow(containment), sum(containment$share >= 0.5)))
ev_cont <- containment %>% filter(share >= 0.5) %>%
  mutate(source = "containment", dated = TRUE) %>%
  select(atom_id, child_unit, parent_unit, parent_type, start, end, source, dated, share)

# ---- Corrections ----------------------------------------------------------------------------
# A `membership` row says a child belongs to a parent for a period. The child (`topo_id`, or
# `name` when the source has no record) is resolved to its parish unit and then to that unit's
# atoms; the parent (`value`) to a unit, by topo_id or by name, including the units m1 created
# from `identity` rows. The parent's TYPE is taken from the parent unit itself, because the
# `type_id` column has been used for the child's type by some rows and the parent's by others.
corr <- read_corrections() %>% filter(kind == "membership")
ev_corr <- NULL
if (nrow(corr)) {
  umeta <- m1$units %>% transmute(unit_id, type_id, uname = name,
                                  u_start = as.integer(format(start_date, "%Y")),
                                  u_end = as.integer(format(end_date, "%Y")))
  # Riksarkivet topo_ids from the records, plus the "CORR:<slug>" keys of the units m1 created
  # from identity rows, so a correction can name either as a parent or a child.
  topo2unit <- bind_rows(
    st_drop_geometry(rec) %>% distinct(topo_id, unit_id),
    m1$units %>% filter(!is.na(topo_ids), startsWith(topo_ids, "CORR:")) %>%
      transmute(topo_id = topo_ids, unit_id)) %>% distinct(topo_id, .keep_all = TRUE)
  name2unit <- umeta %>% group_by(uname) %>% slice(1) %>% ungroup() %>% select(uname, unit_id)

  resolve <- function(key){
    u <- topo2unit$unit_id[match(key, topo2unit$topo_id)]
    ifelse(is.na(u), name2unit$unit_id[match(key, name2unit$uname)], u)
  }
  cm <- corr %>%
    mutate(child_unit = resolve(topo_id),
           child_unit = ifelse(is.na(child_unit), resolve(name), child_unit),
           none_type = ifelse(startsWith(coalesce(value, ""), "none:"),
                              sub("^none:", "", value), NA_character_),
           parent_unit = ifelse(is.na(none_type), resolve(value), NA_character_),
           start = coalesce(start, 1600L), end = coalesce(end, 1990L)) %>%
    filter(!is.na(child_unit), !is.na(none_type) | !is.na(parent_unit), start <= end)
  unresolved <- nrow(corr) - nrow(cm)
  cm <- cm %>% left_join(umeta %>% select(parent_unit = unit_id, p_type = type_id),
                         by = "parent_unit")
  # A child that is not a parish (a contract, a hundred) stands for its parishes: the sources
  # describe a diocese's composition by kontrakt, which is 40 rows instead of ~600 parish rows
  # that would freeze today's parish -> kontrakt containment.
  par_units <- st_drop_geometry(atoms) %>% distinct(unit_id)
  cm_par <- cm %>% filter(child_unit %in% par_units$unit_id)
  cm_grp <- cm %>% filter(!child_unit %in% par_units$unit_id)
  if (nrow(cm_grp)) {
    kids <- containment %>% filter(share >= 0.5) %>%
      select(atom_id, group_unit = parent_unit, g_start = start, g_end = end)
    cm_grp <- cm_grp %>%
      inner_join(kids, by = c("child_unit" = "group_unit"), relationship = "many-to-many") %>%
      mutate(gs = pmax(start, g_start), ge = pmin(end, g_end)) %>% filter(gs <= ge) %>%
      transmute(id, atom_id, child_unit, parent_unit, none_type, p_type,
                start = gs, end = ge)
    message(sprintf("    corrections whose child is a group unit: %d rows -> %d parish claims",
                    n_distinct(cm_grp$id), nrow(cm_grp)))
  }
  ev_corr <- bind_rows(
    cm_par %>%
      inner_join(st_drop_geometry(atoms) %>% select(atom_id, unit_id, a_start = start, a_end = end),
                 by = c("child_unit" = "unit_id"), relationship = "many-to-many") %>%
      mutate(s = pmax(start, a_start), e = pmin(end, a_end)) %>% filter(s <= e) %>%
      transmute(atom_id, child_unit, parent_unit, none_type, p_type, start = s, end = e),
    if (nrow(cm_grp)) cm_grp %>% select(-id) else NULL) %>%
    # "none:<type>" says the parish had no parent of that type then: kept as a claim so it wins
    # the precedence, and dropped after resolution
    mutate(parent_type = coalesce(p_type, none_type)) %>%
    transmute(atom_id, child_unit, parent_unit, parent_type, start, end,
              source = "correction", dated = TRUE, share = NA_real_)
  message(sprintf("  correction claims: %d for %d atoms (%d correction rows unresolved)",
                  nrow(ev_corr), n_distinct(ev_corr$atom_id), unresolved))
  if (unresolved) print(as.data.frame(head(corr[!corr$id %in% cm$id, c("id", "topo_id", "name", "value")], 5)),
                        row.names = FALSE)
}

evidence <- bind_rows(ev_reg, ev_chain, ev_cont, ev_corr)
print(evidence %>% count(source, parent_type) %>% arrange(source, desc(n)) %>% as.data.frame())
timer_end(t0)

message("  --- m3 checks ---")
check(nrow(evidence) > 20000, sprintf("evidence rows > 20000 (got %d)", nrow(evidence)))
check(all(evidence$start <= evidence$end), "every claim has start <= end")
check(all(evidence$parent_type %in% c(higher, "regiment")), "known parent types only")
saveRDS(list(fingerprint = fingerprint, evidence = evidence, containment = containment,
             regiment_links = regiment_links,
             register_quality = register_quality), "data-raw/model/out/m3.rds")
message("  Saved data-raw/model/out/m3.rds")
