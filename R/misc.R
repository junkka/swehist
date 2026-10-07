#' Create lowest common denominator id
#'
#' For each \code{x} and \code{y}, checks for matches in all \code{x}
#'   and all \code{y}, gets all matches + all matches newid and updates
#'   \code{newid}. Essentially find the lowest common denominator.
#'
#' @param x id 1
#' @param y id 2
#' @keywords internal

create_block <- function(x, y){


  # a missing id links to nothing, so give each NA its own node
  x <- as.character(x)
  y <- as.character(y)
  x[is.na(x)] <- paste0("\001na_x", which(is.na(x)))
  y[is.na(y)] <- paste0("\001na_y", which(is.na(y)))

  all_ids <- unique(c(x, y))
  parent <- setNames(seq_along(all_ids), all_ids)
  rank <- rep(0L, length(all_ids))

  find <- function(i){
    while (parent[i] != i) {
      parent[i] <<- parent[parent[i]]
      i <- parent[i]
    }
    i
  }

  union <- function(a, b){
    ra <- find(a)
    rb <- find(b)
    if (ra == rb) return()
    if (rank[ra] < rank[rb]) { tmp <- ra; ra <- rb; rb <- tmp }
    parent[rb] <<- ra
    if (rank[ra] == rank[rb]) rank[ra] <<- rank[ra] + 1L
  }

  # build unions from the edge pairs
  id_idx <- match(c(x, y), all_ids)
  x_idx <- id_idx[seq_along(x)]
  y_idx <- id_idx[seq_along(x) + length(x)]

  for (i in seq_along(x)) {
    union(x_idx[i], y_idx[i])
  }

  # map each row to its root, then to a consecutive group id
  roots <- vapply(x_idx, find, integer(1))
  unique_roots <- unique(roots)

  return(match(roots, unique_roots))
}


#' Match unit names against a user-supplied pattern
#'
#' Case-insensitive. With \code{exact = TRUE} the whole name must match.
#' Otherwise \code{pattern} is matched as plain text, and as a regular
#' expression when the plain-text match finds nothing.
#'
#' @param pattern a single name or pattern
#' @param x names to match against
#' @param exact match the whole name
#' @noRd

name_matches <- function(pattern, x, exact = FALSE){

  if (length(pattern) != 1 || is.na(pattern))
    stop("name must be a single, non-missing string")

  if (exact) return(!is.na(x) & tolower(x) == tolower(pattern))

  # literal first, since unit names carry parentheses for the county qualifier
  lit <- grepl(tolower(pattern), tolower(x), fixed = TRUE)
  if (any(lit, na.rm = TRUE)) return(!is.na(x) & lit)

  tryCatch(
    suppressWarnings(grepl(pattern, x, ignore.case = TRUE)),
    error = function(e) !is.na(x) & lit
  )
}


#' Warn when a map covers only part of Sweden
#'
#' Uses the coverage table built with the data. Some types are partial by
#' nature (hundreds did not exist in Norrland) or partial in the source
#' (county and bailiwick before about 1720).
#'
#' @param typed unit type
#' @param y,x first and last year
#' @param threshold share below which to warn
#' @noRd

warn_coverage <- function(typed, y, x, threshold = 0.9){

  cov <- get0(".coverage", envir = asNamespace("swehist"), inherits = FALSE)
  if (is.null(cov)) return(invisible(NULL))

  sel <- cov$type_id == typed & cov$year >= y & cov$year <= x
  if (!any(sel)) return(invisible(NULL))

  s <- cov$share[sel]
  if (min(s) >= threshold) return(invisible(NULL))

  yr <- cov$year[sel][which.min(s)]

  warning(sprintf(
    "%s boundaries cover %.0f%% of Sweden in %d; the map is partial. See ?boundaries (Coverage).",
    typed, 100 * min(s), yr), call. = FALSE)

  invisible(NULL)
}
