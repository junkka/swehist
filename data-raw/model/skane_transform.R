#' Skåne offset correction
#'
#' The source polygons of Skåne and south Halland lie 100-300 m south-east of Lantmäteriet's
#' distrikt, and the offset varies across the region.

skane_params_file <- "data-raw/model/skane_transform_params.csv"

skane_transform_read <- function(file = skane_params_file){
  p <- utils::read.csv(file, stringsAsFactors = FALSE)
  par <- p[p$role == "param", ]
  v <- stats::setNames(as.numeric(par$value), par$id)
  list(ctrl = p[p$role %in% c("control", "anchor"), c("role", "id", "x", "y", "dx", "dy", "w")],
       bandwidth = v[["bandwidth_m"]], r1 = v[["taper_r1_m"]], r2 = v[["taper_r2_m"]],
       densify = if ("densify_m" %in% names(v)) v[["densify_m"]] else NA_real_)
}

skane_transform_write <- function(params, file = skane_params_file){
  ctrl <- params$ctrl
  ctrl$value <- NA_real_
  par <- data.frame(role = "param", id = c("bandwidth_m", "taper_r1_m", "taper_r2_m", "densify_m"),
                    x = NA_real_, y = NA_real_, dx = NA_real_, dy = NA_real_, w = NA_real_,
                    value = c(params$bandwidth, params$r1, params$r2, params$densify))
  ctrl[c("x", "y", "dx", "dy")] <- lapply(ctrl[c("x", "y", "dx", "dy")], round, 1)
  ctrl$w <- round(ctrl$w, 4)
  utils::write.csv(rbind(par, ctrl[names(par)]), file, row.names = FALSE, na = "")
  invisible(file)
}

# smoothstep taper: 1 at d <= r1, 0 at d >= r2, continuous first derivative
skane_taper <- function(d, r1, r2){
  u <- pmin(pmax((r2 - d) / (r2 - r1), 0), 1)
  u * u * (3 - 2 * u)
}

# Displacement (n x 2 matrix, metres) at coordinates xy (n x 2). Row-wise arithmetic only (no
# BLAS), so identical coordinates always give identical results.
skane_field <- function(xy, ctrl, bandwidth, r1, r2, chunk = 4000L){
  xy <- as.matrix(xy)[, 1:2, drop = FALSE]
  out <- matrix(0, nrow(xy), 2, dimnames = list(NULL, c("dx", "dy")))
  is_c <- ctrl$role == "control"
  if (!any(is_c) || !nrow(xy)) return(out)
  cx <- ctrl$x; cy <- ctrl$y; w <- ctrl$w; sx <- ctrl$dx; sy <- ctrl$dy
  bb <- c(range(cx[is_c]) + c(-r2, r2), range(cy[is_c]) + c(-r2, r2))
  idx <- which(xy[, 1] > bb[1] & xy[, 1] < bb[2] & xy[, 2] > bb[3] & xy[, 2] < bb[4])
  h2 <- 2 * bandwidth^2
  for (s in split(idx, ceiling(seq_along(idx) / chunk))) {
    d2 <- outer(xy[s, 1], cx, "-")^2 + outer(xy[s, 2], cy, "-")^2
    dc <- d2[, is_c, drop = FALSE]
    dmin_c <- sqrt(dc[cbind(seq_along(s), max.col(-dc, ties.method = "first"))])
    dmin <- d2[cbind(seq_along(s), max.col(-d2, ties.method = "first"))]
    k <- exp(-(d2 - dmin) / h2) * rep(w, each = length(s))   # scaled by the nearest: no underflow
    ks <- rowSums(k)
    t <- skane_taper(dmin_c, r1, r2)
    out[s, 1] <- t * rowSums(k * rep(sx, each = length(s))) / ks
    out[s, 2] <- t * rowSums(k * rep(sy, each = length(s))) / ks
  }
  out
}

# Add a displacement to every vertex of an sfc. fun(xy) returns an n x 2 matrix.
displace_sfc <- function(g, fun){
  get <- function(o) {                     # every coordinate matrix / point, in a fixed order
    if (is.matrix(o)) return(list(o[, 1:2, drop = FALSE]))
    if (inherits(o, "POINT")) return(if (length(o) && !all(is.na(o))) list(matrix(o[1:2], 1)) else list())
    do.call(base::c, base::c(list(list()), lapply(o, get)))
  }
  acc <- do.call(base::c, base::c(list(list()), lapply(g, get)))
  if (!length(acc)) return(g)
  xy <- do.call(rbind, acc); rm(acc)
  new <- xy + fun(xy)
  i <- 0L
  put <- function(o) {                     # same order as get()
    if (is.matrix(o)) {
      n <- nrow(o)
      o[, 1:2] <- new[i + seq_len(n), ]
      i <<- i + n
      return(o)
    }
    if (inherits(o, "POINT")) {
      if (length(o) && !all(is.na(o))) { o[1:2] <- new[i + 1L, ]; i <<- i + 1L }
      return(o)
    }
    a <- attributes(o)
    o <- lapply(o, put)
    attributes(o) <- a
    o
  }
  gl <- lapply(g, put)
  stopifnot(i == nrow(new))
  sf::st_sfc(gl, crs = sf::st_crs(g), precision = sf::st_precision(g))
}

# Densify edges longer than `max_len` metres, only in features that reach the field's support
densify_support <- function(g, params, max_len){
  is_c <- params$ctrl$role == "control"
  r2 <- params$r2
  bb <- sf::st_bbox(c(xmin = min(params$ctrl$x[is_c]) - r2, ymin = min(params$ctrl$y[is_c]) - r2,
                      xmax = max(params$ctrl$x[is_c]) + r2, ymax = max(params$ctrl$y[is_c]) + r2),
                    crs = sf::st_crs(g))
  hit <- which(lengths(sf::st_intersects(g, sf::st_as_sfc(bb))) > 0)
  if (length(hit)) g[hit] <- sf::st_segmentize(g[hit], dfMaxLength = max_len)
  g
}

skane_transform <- function(x, params = skane_transform_read(), densify = params$densify){
  g <- sf::st_geometry(x)
  if (is.na(sf::st_crs(g)) || sf::st_crs(g) != sf::st_crs(3006)) stop("x must be in EPSG:3006")
  if (!is.null(densify) && !is.na(densify)) g <- densify_support(g, params, densify)
  g <- displace_sfc(g, function(xy) skane_field(xy, params$ctrl, params$bandwidth, params$r1, params$r2))
  if (inherits(x, "sf")) { sf::st_geometry(x) <- g; x } else g
}
