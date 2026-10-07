#' Get boundaries
#'
#' Get administrative boundaries for a given date or date range.
#'
#' @param date a date, a year, or a vector with date/year range
#' @param type type of unit: "parish", "county", "municipality", "pastorship",
#'   "contract", "diocese", "hundred", "magistrates_court", "district_court",
#'   "court_of_appeal", or "bailiwick"
#' @param format format of return object: "sf" (default) or "meta"
#' @param fill_holes if TRUE, close the enclosed holes in the returned polygons
#'   that no other returned unit of the same type covers. Every unit above the
#'   parish is the union of its member parishes, so it has a hole wherever they
#'   do not reach: over a lake, which no parish covers, around a town that had
#'   its own jurisdiction, and along the slivers where the source's parish
#'   polygons do not quite meet. Only the town is a fact, and it is kept,
#'   because another unit of the type covers it. The default is FALSE: the
#'   shipped geometry is the honest union of the members, and filling is a
#'   choice made when drawing a map, not a property of the data. Filling is per
#'   type, so a filled layer need not nest inside a filled layer of another
#'   type, and its area may exceed the parish territory the coverage figures
#'   are measured against.
#'
#' @export
#' @import dplyr

get_boundaries <- function(
    date,
    type = c("parish", "county", "municipality", "pastorship", "contract",
             "diocese", "hundred", "magistrates_court", "district_court",
             "court_of_appeal", "bailiwick"),
    format = c("sf", "meta"),
    fill_holes = FALSE){

  typed  <- match.arg(type)
  format <- match.arg(format)
  if (!is.logical(fill_holes) || length(fill_holes) != 1 || is.na(fill_holes))
    stop("fill_holes must be TRUE or FALSE")

  date_l <- get_date(date)
  x <- date_l$x
  y <- date_l$y
  period <- date_l$period

  if (x > 1990 || x < 1600 || y > 1990 || y < 1600)
    stop("Date must be between 1600-01-01 and 1990-12-31")

  env <- environment()
  data(boundaries, package = "swehist", envir = env)

  res <- filter(boundaries, start <= x, end >= y, type_id == typed)
  warn_coverage(typed, y, x)

  if (isTRUE(fill_holes) && format == "sf") res <- fill_unclaimed_holes(res)

  if (period) {
    ids <- get_geom_period(x, y, typed)
    res <- get_geom_period_map(res, ids)
    return(list(
      map = switch(format,
        sf   = res,
        meta = as_tibble(sf::st_drop_geometry(res))
      ),
      lookup = ids
    ))
  }

  switch(format,
    sf   = res,
    meta = as_tibble(sf::st_drop_geometry(res))
  )
}


get_geom_period <- function(x, y, typed){
  e <- environment()
  data(relations, package = "swehist", envir = e)
  data(boundaries, package = "swehist", envir = e)

  # both sides of a transition are active within [y, x] only when y <= transition_year < x;
  # only successor links merge units, transfers of part do not
  rels <- filter(relations, relation == "successor",
                 transition_year >= y, transition_year < x,
                 type_id == typed) %>%
    select(geom_id = child_id, geom_id2 = parent_id)

  pars <- as_tibble(sf::st_drop_geometry(boundaries)) %>%
    filter(start <= x & end >= y, type_id == typed) %>%
    mutate(geom_id2 = geom_id) %>%
    select(geom_id, geom_id2)

  parsc <- rbind(rels, pars) %>%
    distinct() %>%
    mutate(geomid = create_block(geom_id, geom_id2)) %>%
    select(geom_id, geomid) %>%
    distinct()

  parsc
}

#' Get child units from hierarchy
#'
#' Find administrative units contained within a parent unit at a given date,
#' using the authoritative hierarchy from Riksarkivet.
#'
#' @param date a date or year (single value, not a range)
#' @param parent_type type of the parent unit (e.g. "county", "hundred", "diocese",
#'   "regiment")
#' @param parent_name name (or partial name) of the parent unit. Matched as a
#'   case-insensitive regular expression (or as plain text if it is not a
#'   valid regular expression) unless \code{exact = TRUE}.
#' @param child_type optional: type of child units to return. If NULL,
#'   returns all child types.
#' @param recursive if TRUE (default), traverse the hierarchy across multiple
#'   levels to reach the target child_type. For example, County to Parish is a
#'   2-hop path (County -> Municipality -> Parish). If FALSE, only return
#'   direct (1-hop) children.
#' @param format format of return object: "sf" (default) or "meta"
#' @param exact if TRUE, \code{parent_name} must equal the full unit name
#'   (case-insensitive). Default FALSE.
#'
#' @return An \code{sf} object (or tibble if format = "meta") of child units,
#'   with an additional \code{parent_name} column.
#'
#' @export
#' @import dplyr
#' @examples
#' # Parishes in Malmöhus län in 1880 (2-hop: county -> municipality -> parish)
#' get_children(1880, "county", "Malmöhus", "parish")
#'
#' # Municipalities in Malmöhus län (direct 1-hop)
#' get_children(1880, "county", "Malmöhus", "municipality")
#'
#' # All units under Skara stift in 1850
#' get_children(1850, "diocese", "Skara")
#'
#' # Parishes in Upplands regemente in 1800
#' get_children(1800, "regiment", "Uppland", "parish")
#'
get_children <- function(date, parent_type, parent_name,
                         child_type = NULL,
                         recursive = TRUE,
                         format = c("sf", "meta"),
                         exact = FALSE){
  format <- match.arg(format)

  date_l <- get_date(date)
  if (date_l$period) stop("get_children does not support date ranges; use a single date")
  yr <- date_l$x

  if (yr > 1990 || yr < 1600)
    stop("Date must be between 1600-01-01 and 1990-12-31")

  env <- environment()
  data(boundaries, package = "swehist", envir = env)
  data(hierarchy, package = "swehist", envir = env)

  # Map type names to match boundaries conventions
  type_lookup <- c(
    "parish" = "Parish", "county" = "County", "municipality" = "Municipality",
    "pastorship" = "Pastorship", "contract" = "Contract", "diocese" = "Diocese",
    "hundred" = "Hundred", "magistrates_court" = "Magistrates Court",
    "district_court" = "District Court", "court_of_appeal" = "Court of Appeal",
    "bailiwick" = "Bailiwick", "regiment" = "Regiment"
  )

  parent_type_eng <- type_lookup[parent_type]
  if (is.na(parent_type_eng))
    stop("Unknown parent_type: ", parent_type,
         ". Use one of: ", paste(names(type_lookup), collapse = ", "))

  child_type_eng <- NULL
  if (!is.null(child_type)) {
    child_type_eng <- type_lookup[child_type]
    if (is.na(child_type_eng))
      stop("Unknown child_type: ", child_type)
  }

  # Find matching parent features
  # Regiment parents have no geometry in boundaries — search hierarchy directly
  is_regiment <- parent_type_eng == "Regiment"

  if (is_regiment) {
    # Search hierarchy by parent_name column
    parent_pattern <- parent_name
    parents <- hierarchy %>%
      filter(parent_type == parent_type_eng, start <= yr, end >= yr,
             name_matches(parent_pattern, parent_name, exact)) %>%
      select(parent_ref_code, p_name = parent_name) %>%
      distinct()

    if (nrow(parents) == 0)
      stop(sprintf("No %s matching '%s' found at year %d", parent_type_eng, parent_name, yr))

    # For regiments, filter hierarchy by parent_ref_code
    if (!recursive) {
      links <- hierarchy %>%
        filter(parent_ref_code %in% parents$parent_ref_code,
               start <= yr, end >= yr)
      if (!is.null(child_type_eng))
        links <- links %>% filter(child_type == child_type_eng)
      links <- links %>%
        left_join(parents, by = "parent_ref_code")
    } else {
      # Regiment is always 1-hop to parish; BFS not needed but kept for consistency
      links <- hierarchy %>%
        filter(parent_ref_code %in% parents$parent_ref_code,
               start <= yr, end >= yr)
      if (!is.null(child_type_eng))
        links <- links %>% filter(child_type == child_type_eng)
      links <- links %>%
        left_join(parents, by = "parent_ref_code")
    }
  } else {
    parents <- boundaries %>%
      filter(type == parent_type_eng, start <= yr, end >= yr,
             name_matches(!!parent_name, name, exact)) %>%
      sf::st_drop_geometry()

    if (nrow(parents) == 0)
      stop(sprintf("No %s matching '%s' found at year %d", parent_type_eng, parent_name, yr))

    if (!recursive) {
      # Single-hop: only direct children
      links <- hierarchy %>%
        filter(parent_geom_id %in% parents$geom_id,
               start <= yr, end >= yr)
      if (!is.null(child_type_eng))
        links <- links %>% filter(child_type == child_type_eng)

      # Add parent name
      links <- links %>%
        left_join(parents %>% select(geom_id, p_name = name),
                  by = c("parent_geom_id" = "geom_id"))
    } else {
      # BFS traversal through hierarchy
      frontier <- parents$geom_id
      # Map frontier IDs back to top-level parent IDs
      parent_map <- tibble(current_id = frontier, top_parent_id = frontier)
      all_links <- tibble()

      for (i in seq_len(5)) {
        hop_links <- hierarchy %>%
          filter(parent_geom_id %in% frontier,
                 start <= yr, end >= yr)

        if (nrow(hop_links) == 0) break

        # Map each hop's children back to their top-level parent
        hop_links <- hop_links %>%
          left_join(parent_map, by = c("parent_geom_id" = "current_id"))

        if (!is.null(child_type_eng)) {
          target <- hop_links %>% filter(child_type == child_type_eng)
          non_target <- hop_links %>% filter(child_type != child_type_eng)
          all_links <- bind_rows(all_links, target)
          if (nrow(non_target) == 0) break
          # Continue exploring non-target children for multi-hop paths
          frontier <- unique(non_target$child_geom_id)
          parent_map <- non_target %>%
            select(current_id = child_geom_id, top_parent_id) %>%
            distinct()
        } else {
          # Collect all descendants
          all_links <- bind_rows(all_links, hop_links)
          frontier <- unique(hop_links$child_geom_id)
          parent_map <- hop_links %>%
            select(current_id = child_geom_id, top_parent_id) %>%
            distinct()
        }
      }

      links <- all_links

      # Add parent name via top-level parent
      if (nrow(links) > 0 && "top_parent_id" %in% names(links)) {
        links <- links %>%
          left_join(parents %>% select(geom_id, p_name = name),
                    by = c("top_parent_id" = "geom_id"))
      }
    }
  }

  if (nrow(links) == 0) {
    warning(sprintf("No children found for %s '%s' at year %d",
                    parent_type_eng, parent_name, yr))
    empty <- boundaries %>% filter(FALSE) %>% mutate(parent_name = character(0))
    if (format == "meta") return(as_tibble(sf::st_drop_geometry(empty)))
    return(empty)
  }

  # Get child features
  res <- boundaries %>%
    filter(geom_id %in% links$child_geom_id, start <= yr, end >= yr) %>%
    left_join(links %>% select(child_geom_id, parent_name = p_name) %>% distinct(),
              by = c("geom_id" = "child_geom_id"))

  switch(format,
    sf   = res,
    meta = as_tibble(sf::st_drop_geometry(res))
  )
}


#' Get border lines
#'
#' Compute border lines from polygon boundaries. Returns a single-row sf object
#' with MULTILINESTRING geometry representing all shared borders.
#'
#' Either provide \code{date} and \code{type} (same arguments as
#' \code{\link{get_boundaries}}) or pass an sf object directly via \code{data}.
#'
#' @param date a date, a year, or a vector with date/year range. Ignored if
#'   \code{data} is provided.
#' @param type type of unit (see \code{\link{get_boundaries}}). Ignored if
#'   \code{data} is provided.
#' @param data an sf object with polygon geometries (e.g. output of
#'   \code{get_boundaries()} or \code{get_children()})
#'
#' @return A single-row \code{sf} object with MULTILINESTRING geometry.
#'
#' @export
#' @import sf
#' @examples
#' # County borders for 1900
#' get_borders(1900, "county")
#'
#' # Borders from any sf polygons
#' parishes <- get_boundaries(1900, "parish")
#' get_borders(data = parishes)
#'
get_borders <- function(date = NULL, type = NULL, data = NULL){
  if (is.null(data)) {
    if (is.null(date) || is.null(type))
      stop("Provide either 'data' or both 'date' and 'type'")
    data <- get_boundaries(date, type)
    # Period maps return a list; extract the map sf
    if (is.list(data) && !inherits(data, "sf"))
      data <- data$map
  }

  if (!inherits(data, "sf"))
    stop("'data' must be an sf object")

  if (nrow(data) == 0) {
    # Return empty linestring sf with same CRS
    empty <- st_sf(geometry = st_sfc(st_multilinestring(), crs = st_crs(data)))
    return(empty)
  }

  # Boundary of each polygon, then union: shared edges are kept once and the
  # internal borders survive (st_boundary(st_union(x)) would only give the
  # outline).
  borders <- st_union(st_boundary(st_geometry(data)))

  # The union can produce a GEOMETRYCOLLECTION; extract lines
  if (inherits(borders[[1]], "GEOMETRYCOLLECTION"))
    borders <- st_collection_extract(borders, "LINESTRING")

  # Ensure MULTILINESTRING in a single-row sf
  borders <- st_sf(geometry = st_sfc(st_combine(borders), crs = st_crs(data)))

  borders
}


#' Administrative change timeline
#'
#' Trace the temporal history of an administrative unit through its
#' predecessors and successors in \code{\link{relations}}. Returns all
#' related temporal versions with event annotations.
#'
#' @param name name (or partial name) of the unit to trace. Matched as a
#'   case-insensitive regular expression (or as plain text if it is not a
#'   valid regular expression) against unit names in \code{boundaries},
#'   unless \code{exact = TRUE}.
#' @param type type of unit (default "parish")
#' @param direction which direction to trace: "both" (default) follows both
#'   predecessors and successors, "forward" follows successors only,
#'   "backward" follows predecessors only.
#' @param exact if TRUE, \code{name} must equal the full unit name
#'   (case-insensitive). Default FALSE.
#' @param transfers if TRUE, also follow \code{"transfer"} relations (part of
#'   a unit moving to a neighbour, less than 10\% of the smaller unit but at
#'   least 5 km2). Default FALSE: only successors.
#'
#' @return A tibble with one row per related temporal version, sorted by
#'   start year:
#' \describe{
#'   \item{geom_id}{Geometry ID linking to \code{boundaries}}
#'   \item{name}{Unit name}
#'   \item{start}{Start year}
#'   \item{end}{End year}
#'   \item{event}{How this version was created: "origin" (no predecessor),
#'     "split" (predecessor had multiple successors), "merge" (multiple
#'     predecessors), or "continuation" (single predecessor with single
#'     successor)}
#'   \item{from_ids}{Comma-separated geom_ids of predecessors, or NA}
#'   \item{to_ids}{Comma-separated geom_ids of successors, or NA}
#' }
#'
#' @export
#' @import dplyr
#' @examples
#' # Trace Luleå parish through all splits and merges
#' unit_history("Luleå", "parish")
#'
#' # What did Luleå become? (forward only)
#' unit_history("Luleå", "parish", direction = "forward")
#'
#' # County history
#' unit_history("Malmöhus", "county")
#'
unit_history <- function(name,
                         type = c("parish", "county", "municipality",
                                  "pastorship", "contract", "diocese",
                                  "hundred", "magistrates_court",
                                  "district_court", "court_of_appeal",
                                  "bailiwick"),
                         direction = c("both", "forward", "backward"),
                         exact = FALSE, transfers = FALSE){
  typed <- match.arg(type)
  direction <- match.arg(direction)
  nm <- name

  env <- environment()
  data(boundaries, package = "swehist", envir = env)
  data(relations, package = "swehist", envir = env)

  # Find matching units
  matches <- boundaries %>%
    filter(type_id == typed, name_matches(!!nm, name, exact)) %>%
    sf::st_drop_geometry()

  if (nrow(matches) == 0)
    stop(sprintf("No %s matching '%s' found", typed, nm))

  # Relations for this type
  rels <- relations %>% filter(type_id == typed)
  if (!transfers) rels <- rels %>% filter(relation == "successor")

  # BFS through relations to find connected component
  visited <- unique(matches$geom_id)
  frontier <- visited

  repeat {
    new_ids <- integer(0)
    if (direction != "backward") {
      new_ids <- c(new_ids, rels$child_id[rels$parent_id %in% frontier])
    }
    if (direction != "forward") {
      new_ids <- c(new_ids, rels$parent_id[rels$child_id %in% frontier])
    }
    new_ids <- setdiff(unique(new_ids), visited)
    if (length(new_ids) == 0) break
    visited <- c(visited, new_ids)
    frontier <- new_ids
  }

  # All features in the connected component
  features <- boundaries %>%
    filter(geom_id %in% visited) %>%
    sf::st_drop_geometry() %>%
    select(geom_id, name, start, end)

  # Build annotations using full relation data (not limited by direction)
  # so events are correct even with directional filtering

  # from_ids (predecessors) per geom_id
  from_info <- rels %>%
    filter(child_id %in% visited) %>%
    group_by(child_id) %>%
    summarise(
      from_ids = paste(sort(parent_id), collapse = ","),
      n_from = n(),
      .groups = "drop"
    ) %>%
    rename(geom_id = child_id)

  # to_ids (successors) per geom_id
  to_info <- rels %>%
    filter(parent_id %in% visited) %>%
    group_by(parent_id) %>%
    summarise(
      to_ids = paste(sort(child_id), collapse = ","),
      .groups = "drop"
    ) %>%
    rename(geom_id = parent_id)

  # For split detection: how many children does each parent have?
  parent_n_children <- rels %>%
    count(parent_id, name = "n_children")

  # For each child in visited, check if its parent had multiple children
  child_sibling_info <- rels %>%
    filter(child_id %in% visited) %>%
    select(child_id, parent_id) %>%
    left_join(parent_n_children, by = "parent_id") %>%
    group_by(child_id) %>%
    summarise(max_siblings = if (length(n_children)) max(n_children) else 0L,
              .groups = "drop") %>%
    rename(geom_id = child_id)

  # Assemble
  features <- features %>%
    left_join(from_info, by = "geom_id") %>%
    left_join(to_info, by = "geom_id") %>%
    left_join(child_sibling_info, by = "geom_id") %>%
    mutate(
      n_from = ifelse(is.na(n_from), 0L, n_from),
      max_siblings = ifelse(is.na(max_siblings), 0L, max_siblings),
      event = case_when(
        n_from == 0 ~ "origin",
        n_from > 1 ~ "merge",
        max_siblings > 1 ~ "split",
        TRUE ~ "continuation"
      )
    ) %>%
    select(geom_id, name, start, end, event, from_ids, to_ids) %>%
    arrange(start, name)

  features
}


#' Assign parent administrative unit
#'
#' Look up which parent unit (county, hundred, diocese, etc.) a set of
#' administrative units belonged to at a given date, using the authoritative
#' hierarchy from \code{\link{hierarchy}}.
#'
#' Traverses the hierarchy upward (up to 5 hops) to find the requested
#' parent type. For example, finding the county of a parish requires 2 hops
#' (Parish -> Municipality -> County).
#'
#' @param geom_ids integer vector of geom_ids from \code{boundaries}
#' @param date a date or year (single value, not a range)
#' @param parent_type type of parent to look up (default "county"). Must be
#'   a type that appears as a parent in \code{hierarchy}.
#'
#' @return A tibble with one row per input geom_id:
#' \describe{
#'   \item{geom_id}{The input geom_id}
#'   \item{parent_geom_id}{geom_id of the parent unit in \code{boundaries}.
#'     \code{NA} for Regiment parents (no geometry) or when no path exists.}
#'   \item{parent_name}{Name of the parent unit}
#'   \item{parent_ref_code}{Riksarkivet reference code of the parent}
#'   \item{multiple}{TRUE if the hierarchy lists more than one parent of the
#'     requested type at this date; the first one is returned}
#' }
#'
#' @details
#' The hierarchy paths are:
#' \itemize{
#'   \item Civil: Parish -> Municipality -> County (before municipalities
#'     existed, 1863, a parish reaches its county through its bailiwick or
#'     a direct County -> Parish link)
#'   \item Fiscal: Parish -> Bailiwick -> County
#'   \item Judicial: Parish -> Hundred -> Magistrates Court -> Court of Appeal
#'     (Parish -> Magistrates Court where there were no hundreds)
#'   \item Judicial (modern): Municipality -> District Court -> Court of Appeal
#'   \item Ecclesiastical: Parish -> Pastorship -> Contract -> Diocese
#'   \item Military: Parish -> Regiment (parent_geom_id will be NA)
#' }
#'
#' If no path exists at the date (e.g. a hundred for a parish in Norrland),
#' \code{parent_geom_id}, \code{parent_name}, and \code{parent_ref_code} will
#' be \code{NA}.
#'
#' @export
#' @import dplyr
#' @examples
#' # Which county do these parishes belong to?
#' parishes <- get_boundaries(1880, "parish")
#' assign_parent(parishes$geom_id[1:5], 1880, "county")
#'
#' # Which hundred?
#' assign_parent(parishes$geom_id[1:5], 1880, "hundred")
#'
assign_parent <- function(geom_ids, date,
                          parent_type = c("county", "municipality", "bailiwick",
                                          "hundred", "magistrates_court",
                                          "district_court", "court_of_appeal",
                                          "diocese", "contract", "pastorship",
                                          "regiment")){
  parent_type <- match.arg(parent_type)

  date_l <- get_date(date)
  if (date_l$period) stop("assign_parent does not support date ranges; use a single date")
  yr <- date_l$x

  if (yr > 1990 || yr < 1600)
    stop("Date must be between 1600-01-01 and 1990-12-31")

  if (length(geom_ids) == 0) {
    return(tibble(
      geom_id = integer(0), parent_geom_id = integer(0),
      parent_name = character(0), parent_ref_code = character(0),
      multiple = logical(0)
    ))
  }

  type_lookup <- c(
    "county" = "County", "municipality" = "Municipality",
    "bailiwick" = "Bailiwick", "hundred" = "Hundred",
    "magistrates_court" = "Magistrates Court",
    "district_court" = "District Court",
    "court_of_appeal" = "Court of Appeal",
    "diocese" = "Diocese", "contract" = "Contract",
    "pastorship" = "Pastorship", "regiment" = "Regiment"
  )
  pt <- type_lookup[parent_type]

  env <- environment()
  data(hierarchy, package = "swehist", envir = env)

  # Active hierarchy at this date (base R to avoid NSE clashes)
  hier <- hierarchy[hierarchy$start <= yr & hierarchy$end >= yr,
                         , drop = FALSE]

  # Initialize result
  result <- tibble(
    geom_id = geom_ids,
    parent_geom_id = NA_integer_,
    parent_name = NA_character_,
    parent_ref_code = NA_character_,
    multiple = FALSE,
    grade = NA_character_
  )

  # evidence behind a hierarchy row, best first (see ?hierarchy)
  grade_rank <- c(corrected = 1L, sourced = 2L, chained_sourced = 3L, chained = 4L,
                  asserted = 5L, derived = 6L)
  hier$.rank <- if ("grade" %in% names(hier)) {
    r <- grade_rank[hier$grade]; ifelse(is.na(r), 9L, r)
  } else 9L

  # BFS upward from input geom_ids
  frontier <- data.frame(row_idx = seq_along(geom_ids),
                         current_id = geom_ids,
                         stringsAsFactors = FALSE)

  for (hop in seq_len(5)) {
    if (nrow(frontier) == 0) break

    # Look up parents of current frontier
    links <- hier[hier$child_geom_id %in% frontier$current_id,
                  c("child_geom_id", "parent_geom_id", "parent_type",
                    "parent_name", "parent_ref_code", ".rank"), drop = FALSE]

    if (nrow(links) == 0) break

    # Join frontier with links
    joined <- merge(frontier, links,
                    by.x = "current_id", by.y = "child_geom_id",
                    all.x = FALSE, sort = FALSE)

    # Found target type?
    found <- joined[joined$parent_type == pt, , drop = FALSE]

    if (nrow(found) > 0) {
      n_cand <- tapply(found$parent_ref_code, found$row_idx,
                       function(v) length(unique(v)))
      result$multiple[as.integer(names(n_cand))] <- n_cand > 1
      # best-graded evidence first, so a dated source outranks an inference
      found <- found[order(found$.rank), , drop = FALSE]
      found <- found[!duplicated(found$row_idx), , drop = FALSE]
      result$parent_geom_id[found$row_idx] <- found$parent_geom_id
      result$parent_name[found$row_idx] <- found$parent_name
      result$parent_ref_code[found$row_idx] <- found$parent_ref_code
      if ("grade" %in% names(hier))
        result$grade[found$row_idx] <- names(grade_rank)[match(found$.rank, grade_rank)]
    }

    # Continue with unresolved
    resolved <- if (nrow(found) > 0) found$row_idx else integer(0)
    remaining <- joined[!joined$row_idx %in% resolved &
                        !is.na(joined$parent_geom_id), , drop = FALSE]
    # Keep all paths (don't dedup by row_idx — need to explore multiple
    # parent types to find the target, e.g. parish→municipality→county)
    frontier <- data.frame(row_idx = remaining$row_idx,
                           current_id = remaining$parent_geom_id,
                           stringsAsFactors = FALSE)
    frontier <- frontier[!duplicated(frontier), , drop = FALSE]
  }

  result
}


get_geom_period_map <- function(m, ids){
  has_kind <- "kind" %in% names(m)
  if (nrow(m) == 0) {
    out <- m %>% mutate(geomid = integer(0), geom_ids = character(0))
    return(out %>% select(any_of(c("geomid", "type", "name", "kind", "start", "end",
                                   "geom_ids"))))
  }
  # snap only the groups that actually merge; a single-member group keeps its shipped
  # geometry, so a period map of one year matches the year's own map
  multi <- ids %>% count(geomid) %>% filter(n > 1) %>% pull(geomid)
  if (length(multi)) {
    k <- m$geom_id %in% ids$geom_id[ids$geomid %in% multi]
    if (any(k)) {
      fixed <- tryCatch(sf::st_make_valid(sf::st_set_precision(sf::st_geometry(m)[k], 1)),
                        error = function(e) NULL)
      if (!is.null(fixed)) sf::st_geometry(m)[k] <- fixed
    }
  }
  out <- m %>%
    left_join(ids, by = "geom_id") %>%
    group_by(geomid) %>%
    summarise(
      type = first(type),
      name = first(name),
      # a merged group can span a change of institution (a landskommun that became a stad):
      # report the kind only where every feature in the group agrees
      kind = if (has_kind) {
        k <- unique(kind[!is.na(kind)])
        if (length(k) == 1) k else NA_character_
      } else NA_character_,
      start = min(start),
      end = max(end),
      geom_ids = paste(sort(unique(geom_id)), collapse = ","),
      .groups = "drop"
    )
  # A merged group can come back as a GEOMETRYCOLLECTION, and st_cast() then keeps only its first
  # part, which loses territory silently. Take the polygons instead.
  g <- sf::st_geometry(out)
  gc <- which(as.character(sf::st_geometry_type(g)) == "GEOMETRYCOLLECTION")
  for (i in gc) {
    p <- tryCatch(sf::st_collection_extract(g[i], "POLYGON"), error = function(e) NULL)
    if (!is.null(p) && length(p)) g[i] <- sf::st_union(p)
  }
  sf::st_geometry(out) <- sf::st_cast(g, "MULTIPOLYGON")
  if (!has_kind) out$kind <- NULL
  out
}


#' Close the holes in a set of polygons that nothing else in the set claims
#'
#' The rule behind \code{get_boundaries(fill_holes = TRUE)}: an enclosed hole is
#' filled unless another polygon in the same set covers it. A town that stood
#' outside its hundred is its own unit of that type, so its hole is kept; a lake
#' and a sliver are claimed by nothing and are closed. Filling by union rather
#' than by dropping the ring, because an island in a lake is a separate polygon
#' of the same geometry and dropping the ring would make the two overlap.
#' @param x an sf object of polygons
#' @return \code{x} with the unclaimed holes closed
#' @noRd
fill_unclaimed_holes <- function(x){
  if (!nrow(x)) return(x)
  g <- sf::st_geometry(x)
  for (i in seq_len(nrow(x))) {
    gi <- g[[i]]
    polys <- if (inherits(gi, "POLYGON")) list(gi) else
      if (inherits(gi, "MULTIPOLYGON")) gi else NULL
    if (is.null(polys) || !any(vapply(polys, length, integer(1)) > 1)) next
    rings <- list()
    for (p in polys) if (length(p) > 1) for (k in 2:length(p))
      rings[[length(rings) + 1]] <- sf::st_polygon(list(p[[k]]))
    if (!length(rings)) next
    rs <- sf::st_sfc(rings, crs = sf::st_crs(x))
    pts <- suppressWarnings(sf::st_point_on_surface(rs))
    others <- if (nrow(x) > 1) g[-i] else NULL
    claimed <- if (is.null(others)) rep(FALSE, length(rs)) else
      lengths(suppressMessages(sf::st_intersects(pts, others))) > 0
    if (!any(!claimed)) next
    ng <- sf::st_union(sf::st_sfc(gi, crs = sf::st_crs(x)), sf::st_union(rs[!claimed]))
    # repair only what was touched and only when it fails, since st_make_valid()
    # downcasts a single-part MULTIPOLYGON to POLYGON and breaks the uniform type
    if (!isTRUE(sf::st_is_valid(ng))) ng <- sf::st_make_valid(ng)
    # st_union() returns POLYGON for a single-part result, which would demote the whole column to
    # sfc_GEOMETRY; boundaries has been uniform MULTIPOLYGON since 1.1.1 and ?boundaries says so
    g[i] <- sf::st_cast(ng, "MULTIPOLYGON")
  }
  sf::st_geometry(x) <- g
  x
}
