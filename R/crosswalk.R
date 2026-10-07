
#' Unwrap a period map and give it a geom_id
#'
#' A period map returns \code{list(map, lookup)} and its id column is \code{geomid},
#' the group id from \code{create_block()}. Rename it so the rest of the package
#' can join on \code{geom_id}.
#' @noRd
period_map <- function(x){
  if (!inherits(x, "list")) return(x)
  m <- x$map
  if (!is.null(m) && !"geom_id" %in% names(m) && "geomid" %in% names(m))
    m$geom_id <- m$geomid
  m
}

#' Crosswalk between two administrative divisions
#'
#' Areal weights for moving data from one division to another, for example from the
#' parishes of 1880 to the municipalities of 1990.
#'
#' @param from,to Each a list or vector of length two giving a date and a type, e.g.
#'   `c(1880, "parish")` or `list(1990, "municipality")`. Any of the 11 types.
#' @param min_share Rows whose share of the source unit is below this are dropped
#'   (default 0.001, i.e. one part in a thousand); set to 0 to keep every sliver.
#' @return A tibble with one row per overlapping pair:
#'   \describe{
#'     \item{from_geom_id, from_name}{the source unit}
#'     \item{to_geom_id, to_name}{the target unit}
#'     \item{km2}{area of the overlap}
#'     \item{share_of_from}{the overlap as a share of the source unit: the weight for
#'       apportioning a count (population, deaths) from source to target}
#'     \item{share_of_to}{the overlap as a share of the target unit: the weight for
#'       apportioning from target back to source}
#'   }
#'
#' @details
#' Shares are areal, so they assume whatever is being moved is spread evenly over the unit. For
#' population that is an approximation, and a poor one where a small part of a parish holds a
#' town. `share_of_from` sums to 1 over each source unit wherever the two divisions cover the
#' same ground; it sums to less where the target division does not (see the coverage note in
#' \code{?boundaries}).
#'
#' @examples
#' \dontrun{
#' w <- crosswalk(c(1880, "parish"), c(1990, "municipality"))
#' # move a parish count to 1990 municipalities
#' library(dplyr)
#' deaths %>% left_join(w, by = c(geom_id = "from_geom_id")) %>%
#'   group_by(to_name) %>% summarise(deaths = sum(n * share_of_from))
#' }
#' @export
crosswalk <- function(from, to, min_share = 0.001){
  pick <- function(x, what){
    x <- as.list(x)
    if (length(x) != 2) stop("`", what, "` must be a date and a type, e.g. c(1880, \"parish\")",
                             call. = FALSE)
    num <- vapply(x, function(v) is.numeric(v) || grepl("^[0-9]{4}", as.character(v)), logical(1))
    if (!any(num)) stop("`", what, "` needs a date", call. = FALSE)
    list(date = x[[which(num)[1]]], type = as.character(x[[which(!num)[1]]]))
  }
  f <- pick(from, "from"); t2 <- pick(to, "to")

  a <- get_boundaries(f$date, f$type)
  b <- get_boundaries(t2$date, t2$type)
  a <- period_map(a)                          # a date range returns list(map, lookup)
  b <- period_map(b)
  a <- a[, c("geom_id", "name")]; b <- b[, c("geom_id", "name")]
  names(a) <- c("from_geom_id", "from_name", "geometry")
  names(b) <- c("to_geom_id", "to_name", "geometry")
  sf::st_geometry(a) <- "geometry"; sf::st_geometry(b) <- "geometry"

  a$from_km2 <- as.numeric(sf::st_area(a)) / 1e6
  b$to_km2   <- as.numeric(sf::st_area(b)) / 1e6

  old <- sf::sf_use_s2(FALSE); on.exit(sf::sf_use_s2(old), add = TRUE)
  inter <- suppressWarnings(sf::st_intersection(a, b))
  if (!nrow(inter)) return(tibble())
  inter$km2 <- as.numeric(sf::st_area(inter)) / 1e6

  out <- sf::st_drop_geometry(inter)
  out$share_of_from <- out$km2 / out$from_km2
  out$share_of_to   <- out$km2 / out$to_km2
  out <- out[out$share_of_from >= min_share & out$km2 > 0, , drop = FALSE]
  out <- out[order(out$from_geom_id, -out$share_of_from), , drop = FALSE]
  as_tibble(out[, c("from_geom_id", "from_name", "to_geom_id", "to_name", "km2",
                            "share_of_from", "share_of_to")])
}


#' Which unit contains a point
#'
#' @param x,y Coordinates. Longitude and latitude by default; give `crs` for anything else.
#' @param date A year or date, as for [get_boundaries()].
#' @param type One of the 11 administrative types.
#' @param crs Coordinate reference system of `x` and `y` (default 4326, WGS84).
#' @param nearest If `TRUE` (the default), a point that falls in no unit is assigned the nearest
#'   one and `dist_m` says how far. The parish polygons leave out most archipelago and lake
#'   islands (see \code{?boundaries}), so a coastal point often needs this.
#' @return A tibble with one row per point: `geom_id`, `name`, `dist_m` (0 when inside) and
#'   `inside`.
#' @examples
#' \dontrun{
#' locate_point(18.07, 59.33, 1880, "parish")     # Stockholm
#' }
#' @export
locate_point <- function(x, y, date, type = "parish", crs = 4326, nearest = TRUE){
  stopifnot(length(x) == length(y))
  b <- get_boundaries(date, type)
  b <- period_map(b)
  p <- sf::st_as_sf(data.frame(x = x, y = y), coords = c("x", "y"), crs = crs)
  p <- sf::st_transform(p, sf::st_crs(b))
  old <- sf::sf_use_s2(FALSE); on.exit(sf::sf_use_s2(old), add = TRUE)

  hit <- sf::st_intersects(p, b)
  idx <- vapply(hit, function(h) if (length(h)) h[1] else NA_integer_, integer(1))
  dist <- rep(0, length(idx))
  miss <- which(is.na(idx))
  if (length(miss) && isTRUE(nearest)) {
    n <- sf::st_nearest_feature(p[miss, ], b)
    idx[miss] <- n
    dist[miss] <- as.numeric(sf::st_distance(p[miss, ], b[n, ], by_element = TRUE))
  }
  tibble(geom_id = b$geom_id[idx], name = b$name[idx],
                 dist_m = round(dist), inside = dist == 0 & !is.na(idx))
}


#' Neighbouring units at a date
#'
#' @param date A year or date, as for [get_boundaries()].
#' @param type One of the 11 administrative types.
#' @param min_border_m Ignore contacts shorter than this (default 1 m), which removes the
#'   corner touches and slivers that make naive neighbour lists noisy.
#' @return A tibble of `a_geom_id`, `a_name`, `b_geom_id`, `b_name`, `border_m`, each unordered
#'   pair once.
#' @examples
#' \dontrun{
#' neighbours(1900, "county")
#' }
#' @export
neighbours <- function(date, type = "parish", min_border_m = 1){
  b <- get_boundaries(date, type)
  b <- period_map(b)
  old <- sf::sf_use_s2(FALSE); on.exit(sf::sf_use_s2(old), add = TRUE)
  ii <- sf::st_intersects(b)
  pr <- do.call(rbind, lapply(seq_along(ii), function(i){
    j <- ii[[i]][ii[[i]] > i]; if (length(j)) cbind(i, j) else NULL
  }))
  if (is.null(pr)) return(tibble())
  len <- vapply(seq_len(nrow(pr)), function(k){
    g <- try(sf::st_intersection(sf::st_geometry(b)[pr[k, 1]], sf::st_geometry(b)[pr[k, 2]]),
             silent = TRUE)
    if (inherits(g, "try-error") || !length(g)) return(0)
    # two units that touch intersect in a line; where they only meet at a corner it is a point,
    # and sometimes a collection of both, which st_boundary() cannot take
    ln <- try(suppressWarnings(sf::st_collection_extract(g, "LINESTRING")), silent = TRUE)
    if (inherits(ln, "try-error") || !length(ln)) return(0)
    as.numeric(sum(sf::st_length(ln)))
  }, numeric(1))
  keep <- which(len >= min_border_m)
  tibble(a_geom_id = b$geom_id[pr[keep, 1]], a_name = b$name[pr[keep, 1]],
                 b_geom_id = b$geom_id[pr[keep, 2]], b_name = b$name[pr[keep, 2]],
                 border_m = round(len[keep]))
}
