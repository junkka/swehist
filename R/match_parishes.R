#' Match parish names or codes to the parish registry
#'
#' Matches a vector of parish names or codes to entries in the
#' \code{\link{parish_registry}} dataset, returning a tibble with match
#' results, quality indicators, and links to \code{boundaries}.
#'
#' @param x character or numeric vector of parish names or codes to match
#' @param by column to match against: NULL (auto-detect for codes), "pid",
#'   "nadkod", "dedik", "forkod", "dedikscb", or "name". When NULL, numeric
#'   input is classified by digit count; character input uses name matching.
#' @param fallback optional character vector of parish names to try when the
#'   primary match on \code{x} fails (returns NA). Must be the same length as
#'   \code{x}. Useful for code-first matching with name fallback, e.g.
#'   \code{match_parishes(df$forkod, fallback = df$name, county = df$county)}.
#'   The \code{county} and \code{fuzzy}/\code{max_dist} settings apply to
#'   fallback matching.
#' @param county optional county constraint (numeric code, letter like "BD",
#'   or name like "Norrbottens"). Used as a soft constraint to disambiguate
#'   multiple matches. Length 1 or same length as \code{x}.
#' @param date optional year (integer) or date string ("YYYY-MM-DD"). When
#'   provided, the returned \code{geom_id} is resolved to the temporal version
#'   active at that date via \code{\link{parish_link}}. Can be length 1
#'   (applied to all rows) or same length as \code{x} (per-row resolution).
#' @param fuzzy logical; if TRUE, attempt approximate matching for names
#'   that fail exact and normalized matching. Default FALSE.
#' @param max_dist numeric; maximum edit distance for fuzzy matching, as a
#'   share of the longer of the two names. Default 0.15.
#' @param expand logical; if TRUE and \code{date} is provided, expand each
#'   matched pid to ALL \code{parish_link} geom_ids active at that date.
#'   This produces a complete map without holes: parishes that NAPP treats
#'   as one unit but swehist has as separate polygons (e.g. stads/lands\-
#'   forsamling pairs) are all included. The result will have more rows
#'   than \code{length(x)} since one input code can map to multiple geom_ids.
#'   Default FALSE.
#'
#' @return A tibble with one row per element of \code{x}:
#' \describe{
#'   \item{input}{The original input value}
#'   \item{pid}{Matched parish ID from \code{parish_registry}}
#'   \item{geom_id}{Geometry ID linking to \code{boundaries}. If \code{date}
#'     is provided, this is the temporally resolved version. For
#'     non-territorial parishes (kbfd, bruk, etc.) that have no geometry,
#'     the parent territorial parish's \code{geom_id} is used as a
#'     fallback when \code{parent_pid} is available in
#'     \code{\link{parish_registry}}.}
#'   \item{name}{Canonical parish name from the registry}
#'   \item{match_type}{"exact_code", "exact_name", "normalized", or "fuzzy"}
#'   \item{match_column}{Which registry column produced the match}
#'   \item{distance}{Edit distance for fuzzy matches, NA otherwise}
#'   \item{county_match}{TRUE if county constraint matched, FALSE if it
#'     didn't, NA if no county was specified}
#'   \item{multiple}{TRUE if the input matched more than one parish}
#'   \item{start}{Start year of the matched \code{geom_id}'s temporal range
#'     from \code{parish_link}. \code{NA} if no geometry.}
#'   \item{end}{End year of the matched \code{geom_id}'s temporal range.
#'     \code{NA} if no geometry.}
#'   \item{resolved_name}{When the \code{geom_id} belongs to a different
#'     parish than the matched \code{pid} (e.g. a parish resolved to its
#'     predecessor at the target date), this gives the name of the parish
#'     that owns the geometry. \code{NA} when \code{geom_id} belongs to
#'     the matched parish.}
#' }
#' When \code{expand = TRUE}, additional rows are added for sibling geom_ids
#' (same pid, different geom_id). These rows copy the \code{input}, \code{pid},
#' and \code{name} from the primary match, with \code{match_type} set to
#' \code{"expanded"}.
#'
#' @details
#' The cascade, the county constraint and the date resolution are the same as in
#' \code{\link{match_units}}, which documents them.
#'
#' @keywords internal
#' @import dplyr
match_parishes <- function(x, by = NULL, fallback = NULL, county = NULL,
                           date = NULL, fuzzy = FALSE, max_dist = 0.15,
                           expand = FALSE){
  if (length(x) == 0) {
    return(tibble(
      input = character(0), pid = integer(0), geom_id = integer(0),
      name = character(0), match_type = character(0),
      match_column = character(0), distance = numeric(0),
      county_match = logical(0), multiple = logical(0),
      start = integer(0), end = integer(0),
      resolved_name = character(0)
    ))
  }

  if (!is.null(fallback)) {
    if (length(fallback) != length(x))
      stop("'fallback' must be the same length as 'x'")
  }

  env <- environment()
  data(parish_registry, package = "swehist", envir = env)

  # Normalize county constraint
  county_codes <- NULL
  if (!is.null(county)) {
    county_codes <- normalize_county(county, parish_registry)
    if (length(county_codes) == 1) county_codes <- rep(county_codes, length(x))
    if (length(county_codes) != length(x))
      stop("'county' must be length 1 or same length as 'x'")
  }

  # Determine matching strategy
  code_cols <- c("pid", "nadkod", "dedik", "forkod", "dedikscb")

  x_given <- if (is.numeric(x)) ifelse(!is.na(x) & x == round(x), sprintf("%.0f", x), as.character(x)) else
    as.character(x)
  x_given[is.na(x)] <- NA_character_

  # Codes read from files often arrive as text ("248201"): treat an all-digit
  # character vector as codes.
  if (is.character(x) && (is.null(by) || by %in% code_cols)) {
    xt <- trimws(x)
    if (any(!is.na(xt)) && all(is.na(xt) | grepl("^[0-9]+$", xt)))
      x <- as.numeric(xt)
  }

  if (!is.null(by)) {
    by <- match.arg(by, c(code_cols, "name"))
    is_code <- by %in% code_cols
  } else {
    is_code <- is.numeric(x)
    if (is_code) by <- detect_by(x, parish_registry)
  }

  # Build name index once for name matching (or fallback)
  need_name_idx <- !is_code || !is.null(fallback)
  name_idx <- if (need_name_idx) build_name_index(parish_registry) else NULL

  # The registry holds the codes of each parish's latest version only;
  # parish_link holds those of every version
  if (is_code) data(parish_link, package = "swehist", envir = env)

  row_yrs <- if (is.null(date)) rep(NA_integer_, length(x)) else date_years(date, length(x))

  # Fast vectorized path for code matching without county constraint
  if (is_code && is.null(county_codes) && by != "nadkod") {
    result <- match_codes_vec(x, by, parish_registry)
    for (i in which(is.na(result$pid) & !is.na(x)))
      result[i, ] <- match_link_code(x[i], by, NA_integer_, parish_link)
    # every dated code of every version: DDB gives a parish several codes
    for (i in which(is.na(result$pid) & !is.na(x)))
      result[i, ] <- match_code_table(x[i], by, NA_integer_, parish_link, row_yrs[i])
    # A forkod with parish part 00 that no parish carries: the town of that
    # code (in the 1970s codes the municipality's total: Stockholm 018000). In the
    # 1930 census scheme the same code is the town's rural parish (128000 Lockarp),
    # and the code cannot tell which, so the answer is always flagged (below).
    if (by == "forkod")
      for (i in which(is.na(result$pid) & !is.na(x) & x %% 100 == 0))
        result[i, ] <- match_town_code(x[i], row_yrs[i], parish_link)
  } else {
    # Element-wise matching (slower but handles county, nadkod fallback, names)
    results <- lapply(seq_along(x), function(i){
      ci <- if (!is.null(county_codes)) county_codes[i] else NA_integer_
      if (is_code) {
        res <- match_one_code(x[i], by, ci, parish_registry)
        if (is.na(res$pid)) res <- match_link_code(x[i], by, ci, parish_link)
        if (is.na(res$pid)) res <- match_code_table(x[i], by, ci, parish_link, row_yrs[i])
        # Auto forkod fallback for nadkod (>=8 digit) codes that don't match
        if (by %in% c("nadkod", "forkod") && is.na(res$pid) && !is.na(x[i]) &&
            (by == "nadkod" || x[i] %% 100 == 0)) {
          res <- nadkod_fallback(x[i], ci, parish_registry, parish_link)
        }
        # A town or municipality code (parish part 00): its parishes
        if (by %in% c("nadkod", "forkod") && is.na(res$pid) && !is.na(x[i]) &&
            (by == "nadkod" || x[i] %% 100 == 0)) {
          res <- match_town_code(x[i], row_yrs[i], parish_link)
        }
        res
      } else {
        match_one_name(x[i], ci, name_idx, parish_registry, fuzzy, max_dist)
      }
    })
    result <- bind_rows(results)
  }

  # Apply fallback name matching for unmatched rows
  if (!is.null(fallback)) {
    unmatched <- which(is.na(result$pid))
    if (length(unmatched) > 0) {
      for (i in unmatched) {
        ci <- if (!is.null(county_codes)) county_codes[i] else NA_integer_
        fb <- match_one_name(fallback[i], ci, name_idx, parish_registry,
                             fuzzy, max_dist)
        if (!is.na(fb$pid)) {
          # Keep original input but use fallback match results
          fb$input <- result$input[i]
          result[i, ] <- fb
        }
      }
    }
  }

  # A forkod with parish part 00 is a placeholder at municipality level that the
  # sources fill with different parishes (048600: Strängnäs landsförsamling in
  # Skatteverket's registry, Kärnbo in the 1930 census): a match on one is flagged
  if (is_code && identical(by, "forkod")) {
    p00 <- !is.na(x) & x %% 100 == 0 & !is.na(result$pid)
    result$multiple[p00] <- TRUE
  }

  # The input as the user gave it: a code read as text and turned into a number
  # came back as "1.8e+07" for "018000000"
  result$input <- x_given

  # Validate date length
  if (!is.null(date) && length(date) > 1 && length(date) != length(x))
    stop("'date' must be length 1 or same length as 'x'")

  # Resolve temporal geom_id if date provided
  if (!is.null(date)) {
    result <- resolve_date(result, date)
  }

  # A whole town given where the data have one parish of several: a forkod
  # with parish part 00 ("018000", Stockholms stad) or a name "X stad". The
  # answer is one parish standing for the town, so it is flagged. A parish part
  # 00 that is a landsförsamling of the old scheme (Vimmerby lands 088400) is a
  # parish of its own and is not.
  if (!is.null(date)) result <- flag_town_totals(result, x, by, row_yrs, is_code)

  # non-territorial parishes (kbfd, bruk) have a pid but no geometry, so fall back
  # to the parent territorial parish's geom_id
  still_no_geom <- which(!is.na(result$pid) & is.na(result$geom_id))
  if (length(still_no_geom) > 0) {
    env <- environment()
    if (!exists("parish_registry", envir = env))
      data(parish_registry, package = "swehist", envir = env)
    pr_parent <- parish_registry[, c("pid", "parent_pid"), drop = FALSE]
    pr_parent <- pr_parent[!is.na(pr_parent$parent_pid), , drop = FALSE]

    if (!exists("parish_link", envir = env))
      data(parish_link, package = "swehist", envir = env)
    pl <- parish_link[!is.na(parish_link$pid),
                      c("pid", "geom_id", "start", "end"), drop = FALSE]

    miss_df <- data.frame(..idx = still_no_geom,
                          pid = result$pid[still_no_geom])
    miss_df <- merge(miss_df, pr_parent, by = "pid", all.x = TRUE, sort = FALSE)
    has_parent <- miss_df[!is.na(miss_df$parent_pid), , drop = FALSE]

    if (nrow(has_parent) > 0) {
      parent_merged <- merge(has_parent, pl,
                             by.x = "parent_pid", by.y = "pid",
                             all.x = TRUE, sort = FALSE)
      parent_merged <- parent_merged[!is.na(parent_merged$geom_id), , drop = FALSE]
      if (!is.null(date)) {
        # Version closest in time to the requested date
        yr_i <- date_years(date, nrow(result))[parent_merged$..idx]
        gap <- pmax(0L, parent_merged$start - yr_i, yr_i - parent_merged$end)
        parent_merged <- parent_merged[order(gap, -parent_merged$end), , drop = FALSE]
      } else {
        # Latest temporal version (max end year)
        parent_merged <- parent_merged[order(-parent_merged$end), , drop = FALSE]
      }
      parent_merged <- parent_merged[!duplicated(parent_merged$..idx), , drop = FALSE]
      result$geom_id[parent_merged$..idx] <- parent_merged$geom_id
    }
  }

  # Expand to all same-pid geom_ids at target date (only for scalar date)
  if (expand && !is.null(date)) {
    if (length(date) > 1)
      stop("'expand' is only supported with a scalar 'date', not per-row dates")
    town <- which(result$match_type %in% "via_municipality")
    result <- expand_coverage(result, date)
    # A town with several parishes: all of them
    for (i in town) {
      kids <- town_parishes(as.numeric(result$input[i]), get_year(date), parish_link)
      kids <- kids[kids$geom_id != result$geom_id[i], , drop = FALSE]
      if (nrow(kids) == 0) next
      result <- bind_rows(result, tibble(
        input = result$input[i], pid = kids$pid, geom_id = kids$geom_id, name = result$name[i],
        match_type = "expanded", match_column = "ref_code", distance = NA_real_,
        county_match = NA, multiple = TRUE))
    }
  }

  # Add start/end year range and resolved_name from parish_link
  result$start <- NA_integer_
  result$end   <- NA_integer_
  result$resolved_name <- NA_character_
  has_gid <- which(!is.na(result$geom_id))
  if (length(has_gid) > 0) {
    env <- environment()
    if (!exists("parish_link", envir = env))
      data(parish_link, package = "swehist", envir = env)
    pl_info <- parish_link[, c("geom_id", "pid", "name", "start", "end"),
                           drop = FALSE]
    pl_info <- pl_info[!duplicated(pl_info$geom_id), , drop = FALSE]
    gid_start <- setNames(pl_info$start, pl_info$geom_id)
    gid_end   <- setNames(pl_info$end,   pl_info$geom_id)
    gid_pid   <- setNames(pl_info$pid,   pl_info$geom_id)
    gid_name  <- setNames(pl_info$name,  pl_info$geom_id)
    gids_chr <- as.character(result$geom_id[has_gid])
    result$start[has_gid] <- unname(gid_start[gids_chr])
    result$end[has_gid]   <- unname(gid_end[gids_chr])
    # Set resolved_name when geom_id belongs to a different pid
    resolved_pids <- unname(gid_pid[gids_chr])
    differs <- !is.na(resolved_pids) & resolved_pids != result$pid[has_gid]
    if (any(differs)) {
      result$resolved_name[has_gid[differs]] <-
        unname(gid_name[gids_chr[differs]])
    }
  }

  result
}


# --- Internal helpers -------------------------------------------------------

#' Vectorized code matching (fast path for code-only without county)
#' @noRd
match_codes_vec <- function(x, by, registry){
  input_df <- data.frame(..i = seq_along(x), ..val = x, stringsAsFactors = FALSE)

  # Registry lookup table: first row per code value (simulates pick first hit)
  reg_col <- registry[[by]]
  reg_df <- data.frame(
    ..val = reg_col, pid = registry$pid, geom_id = registry$geom_id,
    name = registry$name, stringsAsFactors = FALSE
  )
  reg_df <- reg_df[!is.na(reg_df$..val), , drop = FALSE]
  reg_df <- reg_df[!duplicated(reg_df$..val), , drop = FALSE]

  # Count occurrences for 'multiple' flag
  code_counts <- table(reg_col[!is.na(reg_col)])
  multi_codes <- as.numeric(names(code_counts[code_counts > 1]))

  merged <- merge(input_df, reg_df, by = "..val", all.x = TRUE, sort = FALSE)
  merged <- merged[order(merged$..i), , drop = FALSE]

  tibble(
    input = as.character(x),
    pid = merged$pid,
    geom_id = merged$geom_id,
    name = merged$name,
    match_type = ifelse(is.na(merged$pid), NA_character_, "exact_code"),
    match_column = ifelse(is.na(merged$pid), NA_character_, by),
    distance = NA_real_,
    county_match = NA,
    multiple = !is.na(merged$..val) & merged$..val %in% multi_codes
  )
}


#' Auto-detect the code column for a vector of codes
#'
#' Looks at the whole vector, not just the first value. Codes of 8 or more
#' digits are nadkod. Otherwise the column (forkod, dedikscb, dedik, pid) that
#' matches the most values wins; ties go to that order. forkod has 5 digits
#' for counties 01-09 and 6 digits otherwise, so digit count alone cannot
#' tell forkod from dedik.
#' @noRd
detect_by <- function(x, registry){
  v <- unique(x[!is.na(x)])
  if (length(v) == 0) return("pid")

  digits <- nchar(format(v, scientific = FALSE, trim = TRUE))
  if (stats::median(digits) >= 8) return("nadkod")

  cands <- c("forkod", "dedikscb", "dedik", "pid")
  hits <- vapply(cands, function(cl) sum(v %in% registry[[cl]]), numeric(1))
  if (all(hits == 0)) {
    return(if (stats::median(digits) <= 4) "pid" else "forkod")
  }
  best <- cands[which.max(hits)]
  others <- setdiff(cands[hits >= 0.5 * max(hits)], c(best, "dedikscb"))
  if (best != "dedikscb" && length(others) > 0)
    warning(sprintf("Codes match both '%s' and '%s'; using '%s'. ",
                    best, others[1], best),
            "Set 'by' explicitly to choose.", call. = FALSE)
  best
}


#' Normalize county specification to numeric code
#' @noRd
normalize_county <- function(county, registry){
  if (is.numeric(county)) return(as.integer(county))

  vapply(county, function(c){
    c <- trimws(c)

    # Try numeric parse
    num <- suppressWarnings(as.integer(c))
    if (!is.na(num) && num >= 1 && num <= 27) return(num)

    # Try letter match (exact, case-insensitive)
    idx <- match(toupper(c), toupper(registry$letter))
    if (!is.na(idx)) return(as.integer(registry$county[idx]))

    # name match, fixed = TRUE: the input is a county name, not a pattern, and a stray
    # bracket in it would abort the call. both sides lower case, without "lan".
    key <- tolower(c)
    if (grepl("\\(", key)) key <- sub(".*\\(", "", key)
    key <- trimws(sub("\\s*l\u00e4n(et)?s?$", "", key))
    reg <- sub("\\s*l\u00e4n(et)?s?$", "", tolower(registry$county_name))
    idx <- which(reg == key)
    if (!length(idx)) idx <- which(startsWith(reg, key) | startsWith(key, reg))
    if (length(idx)) return(as.integer(registry$county[idx[1]]))

    # Historical and informal county names the registry does not carry
    hist <- c("stockholms stad" = 1L, "stockholm" = 1L, "malm\u00f6hus" = 12L,
              "kristianstads" = 11L, "g\u00f6teborgs och bohus" = 14L, "g\u00f6teborgs o bohus" = 14L,
              "skaraborgs" = 16L, "\u00e4lvsborgs" = 15L, "kopparbergs" = 20L, "dalarnas" = 20L,
              "g\u00e4vleborgs" = 21L, "v\u00e4sternorrlands" = 22L, "h\u00e4rn\u00f6sands" = 22L,
              "hudiksvalls" = 22L, "n\u00e4rke och v\u00e4rmlands" = 18L, "sk\u00e5ne" = 12L,
              "v\u00e4stra g\u00f6talands" = 14L, "upplands" = 3L)
    if (key %in% names(hist)) return(unname(hist[key]))

    warning("Could not resolve county: ", c, call. = FALSE)
    NA_integer_
  }, integer(1), USE.NAMES = FALSE)
}


#' Normalize Swedish parish name for matching
#' @noRd
normalize_parish_name <- function(x){
  x <- tolower(trimws(x))
  # Strip "stadsforsamling(s)", "landsforsamling(s)",
  # "domkyrkoforsamling(s)", or plain "forsamling(s)"
  x <- sub("\\s*(stads|lands|domkyrko)?f\u00f6rsamlings?$", "", x)
  # Also catch ASCII-degraded "forsamling"
  x <- sub("\\s*(stads|lands|domkyrko)?forsamlings?$", "", x)
  # Strip trailing parenthetical like "(X-lan)", "(Enonteki)"
  x <- sub("\\s*\\([^)]*\\)\\s*$", "", x)
  # A registration district carries its parish's name (Fjallbacka kbfd)
  x <- sub("\\s+(kbfd|kyrkoomr\u00e5de|kyrkobokf\u00f6ringsdistrikt)$", "", x)
  # Strip trailing stads/lands/domkyrko (if not already stripped)
  x <- sub("\\s+(stads|lands|domkyrko)$", "", x)
  # Historical w -> v (pre-1906 Swedish spelling convention)
  x <- chartr("w", "v", x)
  # Fold Swedish diacritics to ASCII (a/a -> a, o -> o, e -> e)
  x <- chartr("\u00e5\u00e4\u00f6\u00e9", "aaoe", x)
  # Strip genitive "s" (conservative: after vowel + consonant)
  x <- sub("([aeiouy][bcdfghjklmnpqrstvwxz])s$", "\\1", x)
  # Collapse whitespace
  x <- gsub("\\s+", " ", trimws(x))
  x
}


#' Build long-format name index from parish_registry
#' @noRd
build_name_index <- function(registry){
  simple_cols <- c("name", "socken", "name_full", "name_scb", "name_ddb")
  multi_cols <- c("alias", "name_previous")
  all_cols <- c(simple_cols, multi_cols)

  parts <- list()

  # Simple columns (one value per row)
  for (col in simple_cols) {
    vals <- registry[[col]]
    ok <- !is.na(vals) & nzchar(vals)
    if (!any(ok)) next
    parts[[length(parts) + 1]] <- data.frame(
      lower = tolower(vals[ok]),
      normalized = normalize_parish_name(vals[ok]),
      historical = hist_key(normalize_parish_name(vals[ok])),
      column = col,
      pid = registry$pid[ok],
      county = registry$county[ok],
      priority = match(col, all_cols),
      stringsAsFactors = FALSE
    )
  }

  # Comma-separated columns (split and expand)
  for (col in multi_cols) {
    vals <- registry[[col]]
    ok <- !is.na(vals) & nzchar(vals)
    if (!any(ok)) next

    split_vals <- strsplit(vals[ok], ",")
    lens <- vapply(split_vals, length, integer(1))
    flat <- trimws(unlist(split_vals))
    pids <- rep(registry$pid[ok], lens)
    counties <- rep(registry$county[ok], lens)

    ok2 <- !is.na(flat) & nzchar(flat)
    if (!any(ok2)) next

    parts[[length(parts) + 1]] <- data.frame(
      lower = tolower(flat[ok2]),
      normalized = normalize_parish_name(flat[ok2]),
      historical = hist_key(normalize_parish_name(flat[ok2])),
      column = col,
      pid = pids[ok2],
      county = counties[ok2],
      priority = match(col, all_cols),
      stringsAsFactors = FALSE
    )
  }

  idx <- do.call(rbind, parts)

  # Clean up parsed name variants: dates left on previous names ("1953
  # Soderkoping"), and double names ("Kungsholm eller Ulrika Eleonora",
  # "Kungsholmen / Ulrika Eleonora") also match each of their parts
  lead_date <- "^-?[0-9]{4}(-[0-9]{2}(-[0-9]{2})?)?-?\\s+"
  dated <- idx[grepl(lead_date, idx$lower), , drop = FALSE]
  dated$lower <- sub("\\.$", "", sub(lead_date, "", dated$lower))
  dated$normalized <- normalize_parish_name(dated$lower)
  dated$historical <- hist_key(dated$normalized)
  idx <- rbind(idx, dated)
  two <- grepl(" eller | / ", idx$lower)
  if (any(two)) {
    sp <- strsplit(idx$lower[two], " eller | / ")
    extra <- idx[rep(which(two), lengths(sp)), , drop = FALSE]
    extra$lower <- trimws(unlist(sp))
    extra$normalized <- normalize_parish_name(extra$lower)
    extra$historical <- hist_key(extra$normalized)
    idx <- rbind(idx, extra[nzchar(extra$lower), , drop = FALSE])
  }
  idx
}


#' Match a single code value
#' @noRd
match_one_code <- function(val, by, county_code, registry){
  if (is.na(val)) return(empty_match_result(val))

  hits <- registry[!is.na(registry[[by]]) & registry[[by]] == val, , drop = FALSE]
  if (nrow(hits) == 0) return(empty_match_result(val))

  pick_best_match(hits, val, "exact_code", by, county_code)
}


#' Match a code against parish_link (codes of every version of a parish).
#' A code shared by several versions of one parish is not ambiguous.
#' @noRd
match_link_code <- function(val, by, county_code, link){
  if (is.na(val) || !by %in% names(link)) return(empty_match_result(val))
  hits <- link[!is.na(link[[by]]) & link[[by]] == val & !is.na(link$pid), , drop = FALSE]
  if (nrow(hits) == 0) return(empty_match_result(val))
  hits <- hits[order(-hits$end), , drop = FALSE]
  res <- pick_best_match(hits, val, "exact_code", by, county_code)
  res$multiple <- length(unique(hits$pid)) > 1
  res
}


#' Flag answers that are one parish standing for a whole town
#' @noRd
flag_town_totals <- function(result, x, by, yrs, is_code) {
  env <- environment()
  data(parish_link, package = "swehist", envir = env)
  xs <- as.character(x)
  cand <- if (is_code && identical(by, "forkod")) {
    v <- suppressWarnings(as.numeric(xs))
    !is.na(v) & v %% 100 == 0
  } else if (!is_code) {
    grepl("\\bstad\\b", xs, ignore.case = TRUE) & !grepl("stadsf", xs, ignore.case = TRUE)
  } else rep(FALSE, length(xs))
  idx <- which(cand & !is.na(result$geom_id) & !is.na(yrs))
  for (i in idx) {
    g <- match(result$geom_id[i], parish_link$geom_id)
    if (is.na(g) || grepl("lands", parish_link$name[g], ignore.case = TRUE)) next
    fk <- parish_link$forkod[g]
    if (is.na(fk)) next
    town <- parish_link$forkod %/% 100 == fk %/% 100 & parish_link$start <= yrs[i] &
      parish_link$end >= yrs[i]
    if (sum(town, na.rm = TRUE) > 1) result$multiple[i] <- TRUE
  }
  result
}


#' Match a code against parish_codes: every dated code of every parish
#' version (a parish has had several DDB codes, and SCB recoded parishes in
#' 1952-1990). With a year, the version whose code period is nearest to it.
#' @noRd
match_code_table <- function(val, by, county_code, link, yr = NA_integer_) {
  if (is.na(val) || !by %in% c("dedik", "forkod", "nadkod")) return(empty_match_result(val))
  env <- environment()
  data(parish_codes, package = "swehist", envir = env)
  h <- parish_codes[parish_codes$system == by & parish_codes$code == val & !is.na(parish_codes$pid), ,
                    drop = FALSE]
  if (nrow(h) == 0) return(empty_match_result(val))
  if (!is.na(yr)) {
    gap <- pmax(0L, h$start - yr, yr - h$end)
    h <- h[order(gap, -h$end), , drop = FALSE]
    pids_near <- unique(h$pid[gap[order(gap, -h$end)] == min(gap)])
  } else {
    h <- h[order(-h$end), , drop = FALSE]
    pids_near <- unique(h$pid)
  }
  hits <- link[match(h$geom_id, link$geom_id), , drop = FALSE]
  hits <- hits[!is.na(hits$geom_id), , drop = FALSE]
  if (nrow(hits) == 0) return(empty_match_result(val))
  res <- pick_best_match(hits, val, "exact_code", by, county_code)
  res$multiple <- length(pids_near) > 1
  res
}


#' Match a single name value through the cascade
#' @noRd
match_one_name <- function(val, county_code, name_idx, registry,
                           fuzzy, max_dist){
  if (is.na(val) || !nzchar(trimws(val))) return(empty_match_result(val))

  val_full <- expand_parish_abbrev(val)
  val_lower <- tolower(trimws(val_full))
  val_norm <- normalize_parish_name(val_full)

  # With a county, a step whose hits all lie in other counties does not end the
  # cascade: "Östra Ryds församling" is stored under that exact name only for
  # the parish in Stockholms län (the one in Östergötland is "... (E-län)"),
  # so stopping at the exact step returned Stockholm's for county = 5, with
  # multiple = FALSE. The first such step is kept and returned, flagged, only
  # if no later step finds the parish in the county.
  in_county <- function(h) {
    if (is.na(county_code)) return(TRUE)
    cc <- registry$county[match(h$pid, registry$pid)]
    any(!is.na(cc) & cc == county_code)
  }
  first_out <- NULL
  step <- function(h, type, distance = NA_real_) {
    if (nrow(h) == 0) return(NULL)
    r <- resolve_name_hits(h, val, type, county_code, registry, distance = distance)
    if (in_county(h)) return(r)
    if (is.null(first_out)) first_out <<- r
    NULL
  }
  out_of_county <- function() {
    if (is.null(first_out)) return(empty_match_result(val))
    first_out$multiple <- TRUE
    first_out$county_match <- FALSE
    first_out
  }

  # Step 1: Exact name match (case-insensitive)
  r <- step(name_idx[name_idx$lower == val_lower, , drop = FALSE], "exact_name")
  if (!is.null(r)) return(r)

  # Step 2: Normalized match
  r <- step(name_idx[name_idx$normalized == val_norm, , drop = FALSE], "normalized")
  if (!is.null(r)) return(r)

  # Step 2b: Pre-1906 spelling (Hjelmseryd, Qvidinge, Hvetlanda, ...)
  r <- step(name_idx[name_idx$historical == hist_key(val_norm), , drop = FALSE], "historical")
  if (!is.null(r)) return(r)

  # Step 3: Fuzzy match (if enabled)
  if (fuzzy) {
    fuzzy_rows <- fuzzy_candidates(val_norm, name_idx$normalized, max_dist)
    if (length(fuzzy_rows) > 0) {
      fuzz <- name_idx[fuzzy_rows, , drop = FALSE]
      dists <- as.integer(utils::adist(val_norm, fuzz$normalized))
      # Order by distance then priority
      ord <- order(dists, fuzz$priority)
      fuzz <- fuzz[ord, , drop = FALSE]
      dists <- dists[ord]
      # within the county, the nearest name there
      if (!is.na(county_code)) {
        cc <- registry$county[match(fuzz$pid, registry$pid)]
        inc <- !is.na(cc) & cc == county_code
        if (any(inc) && is.null(first_out)) {
          return(resolve_name_hits(fuzz[inc, , drop = FALSE], val, "fuzzy", county_code, registry,
                                   distance = dists[inc][1]))
        }
      }
      r <- step(fuzz, "fuzzy", distance = dists[1])
      if (!is.null(r)) return(r)
    }
  }

  out_of_county()
}


#' Resolve name index hits to a single best match
#' @noRd
resolve_name_hits <- function(idx_hits, input_val, match_type, county_code,
                              registry, distance = NA_real_){
  # Deduplicate: best column per pid (lowest priority = highest confidence)
  idx_hits <- idx_hits[order(idx_hits$priority), , drop = FALSE]
  idx_hits <- idx_hits[!duplicated(idx_hits$pid), , drop = FALSE]

  multiple <- length(unique(idx_hits$pid)) > 1

  # Get registry info for matched pids
  reg <- registry[registry$pid %in% idx_hits$pid,
                  c("pid", "geom_id", "name", "county"), drop = FALSE]

  # Merge column info
  merged <- merge(idx_hits[, c("pid", "column", "priority"), drop = FALSE],
                  reg, by = "pid")

  county_match <- NA

  # Apply county soft constraint
  if (!is.na(county_code)) {
    if (nrow(merged) > 1) {
      county_rows <- !is.na(merged$county) & merged$county == county_code
      if (any(county_rows)) {
        merged <- merged[county_rows, , drop = FALSE]
        county_match <- TRUE
      } else {
        county_match <- FALSE
      }
    } else {
      county_match <- isTRUE(merged$county[1] == county_code)
    }
  }

  # Among several, a parish with geometry before one without (Arboga
  # landsforsamling before Arboga kbfd)
  if (nrow(merged) > 1 && any(!is.na(merged$geom_id)))
    merged <- merged[!is.na(merged$geom_id), , drop = FALSE]

  # Pick best by priority
  best <- merged[which.min(merged$priority), , drop = FALSE]

  tibble(
    input = as.character(input_val),
    pid = best$pid,
    geom_id = best$geom_id,
    name = best$name,
    match_type = match_type,
    match_column = best$column,
    distance = distance,
    county_match = county_match,
    multiple = multiple
  )
}


#' Apply county soft constraint to code-matched hits
#' @noRd
pick_best_match <- function(hits, input_val, match_type, match_column,
                            county_code){
  multiple <- nrow(hits) > 1
  county_match <- NA

  if (!is.na(county_code)) {
    if (nrow(hits) > 1) {
      county_rows <- !is.na(hits$county) & hits$county == county_code
      if (any(county_rows)) {
        hits <- hits[county_rows, , drop = FALSE]
        county_match <- TRUE
      } else {
        county_match <- FALSE
      }
    } else {
      county_match <- isTRUE(hits$county[1] == county_code)
    }
  }

  hit <- hits[1, , drop = FALSE]

  tibble(
    input = as.character(input_val),
    pid = hit$pid,
    geom_id = hit$geom_id,
    name = hit$name,
    match_type = match_type,
    match_column = match_column,
    distance = NA_real_,
    county_match = county_match,
    multiple = multiple
  )
}


#' Years for a scalar or per-row date vector
#' @noRd
date_years <- function(date, n){
  if (length(date) == 1) return(rep(get_year(date), n))
  vapply(seq_along(date), function(j) get_year(date[j]), integer(1))
}


#' Rows of \code{candidates} within \code{max_dist} of \code{val}: edit
#' distance over the whole string, relative to the longer of the two.
#' (agrep() alone matches \code{val} anywhere inside a candidate, so long
#' names matched unrelated short ones.)
#' @noRd
fuzzy_candidates <- function(val, candidates, max_dist){
  pre <- agrep(val, candidates, max.distance = max_dist, ignore.case = TRUE)
  # agrep's bound is relative to the pattern; also look at candidates it
  # cannot see because they are longer than the pattern
  near <- which(abs(nchar(candidates) - nchar(val)) <= ceiling(max_dist * nchar(candidates)))
  rows <- union(pre, near)
  if (length(rows) == 0) return(integer(0))
  d <- as.numeric(utils::adist(val, candidates[rows], ignore.case = TRUE))
  rel <- d / pmax(nchar(val), nchar(candidates[rows]), 1)
  rows[rel <= max_dist]
}


#' Expand abbreviations in names from censuses and NAPP: "fors",
#' "landsfors", "hospitalsfors" -> "...forsamling"; "X stad" -> "X
#' stadsforsamling". Only "fors" with o: place names end in "-fors" (Nianfors).
#' @noRd
expand_parish_abbrev <- function(x){
  x <- sub("(\\s|[a-z\u00e5\u00e4\u00f6])f\u00f6rs\\.?$", "\\1f\u00f6rsamling", x,
           ignore.case = TRUE, perl = TRUE)
  sub("\\s+stad$", " stadsf\u00f6rsamling", x, ignore.case = TRUE, perl = TRUE)
}


#' Key for matching pre-1906 spellings: historical transforms, then fold
#' diacritics again (the transforms can introduce \u00e4, e.g. hjelm -> hj\u00e4lm)
#' @noRd
hist_key <- function(x){
  chartr("\u00e5\u00e4\u00f6\u00e9", "aaoe", normalize_historical_spelling(x))
}


#' One parish version per input row
#'
#' A pid can have several versions at one date (Glostorp and Lockarp are both
#' versions of Oxie's pid in 1900). Prefer the version whose name starts with
#' the input name (or, for a code, the matched registry name), then the one
#' sharing most territory with the unit the input matched, then the lowest
#' geom_id, so the result does not depend on row
#' order. ..several is TRUE when the name did not decide.
#' @noRd
pick_version <- function(active, result){
  if (nrow(active) == 0) {
    active$..several <- logical(0)
    return(active)
  }
  key <- function(x) gsub("s\\b", "", hist_key(tolower(trimws(as.character(x)))), perl = TRUE)
  # the input, or for a code the name of the matched registry entry
  starts <- function(nm){
    k <- key(nm)
    !is.na(k) & nchar(k) > 1 & startsWith(key(active$name), k)
  }
  # (the input first: "Askersunds stadsfors" is matched to the registry
  # entry Askersund, which both versions start with)
  active$..pref <- 2L * starts(result$input[active$..idx]) +
    starts(result$name[active$..idx])
  active$..shared <- 0
  n_all <- table(active$..idx)
  multi <- active$..idx %in% as.integer(names(n_all)[n_all > 1]) &
    !is.na(result$geom_id[active$..idx])
  if (any(multi)) {
    env <- environment()
    data(boundaries, package = "swehist", envir = env)
    g <- sf::st_geometry(boundaries)
    cand <- g[match(active$geom_id[multi], boundaries$geom_id)]
    orig <- g[match(result$geom_id[active$..idx[multi]], boundaries$geom_id)]
    active$..shared[multi] <- vapply(seq_along(cand), function(k)
      sum(as.numeric(sf::st_area(suppressMessages(sf::st_intersection(cand[k], orig[k]))))),
      numeric(1))
  }
  active <- active[order(active$..idx, -active$..pref, -active$..shared, active$geom_id), ,
                   drop = FALSE]
  best <- tapply(active$..pref, active$..idx, max)
  n_best <- tapply(active$..pref, active$..idx, function(p) sum(p == max(p)))
  first <- active[!duplicated(active$..idx), , drop = FALSE]
  k <- as.character(first$..idx)
  first$..several <- ifelse(best[k] > 0, n_best[k] > 1, n_all[k] > 1)
  first
}


#' Resolve geom_id to temporal version via parish_link
#'
#' When the matched pid has a parish_link entry active at the target date, use
#' that geom_id. Otherwise, walk relations towards the date (resolve_temporal())
#' to find the geom_id that was active at the target date.
#' @noRd
resolve_date <- function(result, date){
  yrs <- date_years(date, nrow(result))

  env <- environment()
  data(parish_link, package = "swehist", envir = env)

  matched <- which(!is.na(result$pid))
  if (length(matched) == 0) return(result)

  # A matched version active at the date stays (a code can name one version
  # of a pid that has several at that date)
  own <- match(result$geom_id[matched], parish_link$geom_id)
  own_active <- !is.na(own) & parish_link$start[own] <= yrs[matched] &
    parish_link$end[own] >= yrs[matched]
  matched <- matched[!own_active]
  if (length(matched) == 0) return(result)

  # Vectorized: join matched rows with parish_link on pid + year range
  m_pid <- result$pid[matched]
  m_yr  <- yrs[matched]

  # Build lookup: parish_link rows with non-NA pid, keyed by pid
  pl <- parish_link[!is.na(parish_link$pid),
                    c("pid", "geom_id", "name", "start", "end"), drop = FALSE]

  # For each matched row, find active parish_link entry via merge
  lookup_df <- data.frame(..idx = matched, pid = m_pid, ..yr = m_yr)
  merged <- merge(lookup_df, pl, by = "pid", all.x = TRUE, sort = FALSE)
  # Filter to active entries (start <= yr & end >= yr)
  active <- merged[!is.na(merged$geom_id) &
                   merged$start <= merged$..yr &
                   merged$end   >= merged$..yr, , drop = FALSE]
  active <- pick_version(active, result)
  result$geom_id[active$..idx] <- active$geom_id
  result$multiple[active$..idx] <- result$multiple[active$..idx] | active$..several

  # Rows whose pid has no version active at the date: walk relations towards
  # the date (predecessors for an earlier date, successors for a later one;
  # Skelleftea in 1880 is a version with another pid)
  resolved_idx <- active$..idx
  unresolved <- setdiff(matched, resolved_idx)
  # Also exclude rows where parish_link had no entry at all
  has_pl <- matched[m_pid %in% pl$pid]
  need_walk <- intersect(unresolved, has_pl)

  if (length(need_walk) > 0) {
    sp_parish <- parish_link[, c("geom_id", "name", "start", "end"), drop = FALSE]
    for (i in need_walk) {
      resolved <- resolve_temporal(result$geom_id[i], yrs[i], "parish", sp_parish)
      if (!is.null(resolved)) {
        result$geom_id[i] <- resolved$geom_id
        # several versions at the date (Strangnas stads- and landsforsamling
        # both became the domkyrkoforsamling of 1966)
        if (isTRUE(attr(resolved, "several"))) result$multiple[i] <- TRUE
      }
    }
  }

  # Still a version that did not exist at the date, and no relation leads to
  # one that did: the parish that held its territory then, flagged. Returning
  # the later parish drew it over its mother parish on the map (Kågedalen's
  # code in 1761 over Skellefteå).
  g <- match(result$geom_id, parish_link$geom_id)
  stale <- which(!is.na(g) & !is.na(yrs) &
                   (parish_link$start[g] > yrs | parish_link$end[g] < yrs))
  if (length(stale) > 0) {
    data(boundaries, package = "swehist", envir = env)
    par_sf <- boundaries[boundaries$type == "Parish", c("geom_id", "start", "end")]
    for (i in stale) {
      g0 <- par_sf[par_sf$geom_id == result$geom_id[i], ]
      act <- par_sf[par_sf$start <= yrs[i] & par_sf$end >= yrs[i], ]
      if (nrow(g0) == 0 || nrow(act) == 0) next
      pt <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(g0)))
      hit <- sf::st_intersects(pt, act)[[1]]
      if (length(hit) == 1) {
        result$geom_id[i] <- act$geom_id[hit]
        result$multiple[i] <- TRUE
      }
    }
  }

  # Parent fallback: non-territorial parishes without geom_id can inherit
  # from their parent_pid's geom_id via parish_registry
  still_missing <- which(!is.na(result$pid) & is.na(result$geom_id))
  if (length(still_missing) > 0) {
    data(parish_registry, package = "swehist", envir = env)
    pr_parent <- parish_registry[, c("pid", "parent_pid"), drop = FALSE]
    pr_parent <- pr_parent[!is.na(pr_parent$parent_pid), , drop = FALSE]

    miss_df <- data.frame(..idx = still_missing, pid = result$pid[still_missing],
                          ..yr = yrs[still_missing])
    # Join to get parent_pid
    miss_df <- merge(miss_df, pr_parent, by = "pid", all.x = TRUE, sort = FALSE)
    has_parent <- miss_df[!is.na(miss_df$parent_pid), , drop = FALSE]

    if (nrow(has_parent) > 0) {
      # Look up parent's active geom_id in parish_link
      parent_merged <- merge(has_parent, pl,
                             by.x = "parent_pid", by.y = "pid",
                             all.x = TRUE, sort = FALSE)
      parent_active <- parent_merged[!is.na(parent_merged$geom_id) &
                                     parent_merged$start <= parent_merged$..yr &
                                     parent_merged$end   >= parent_merged$..yr, ,
                                     drop = FALSE]
      parent_active <- pick_version(parent_active, result)
      result$geom_id[parent_active$..idx] <- parent_active$geom_id
      result$multiple[parent_active$..idx] <- result$multiple[parent_active$..idx] |
        parent_active$..several
    }
  }

  result
}


#' Expand match results to fill map holes
#'
#' For each matched pid, finds ALL parish_link geom_ids active at the target
#' date (not just the one returned by resolve_date). Also walks relations
#' to find uncovered geom_ids whose predecessor/successor is matched.
#' @noRd
expand_coverage <- function(result, date){
  yr <- get_year(date)

  env <- environment()
  data(parish_link, package = "swehist", envir = env)
  data(relations, package = "swehist", envir = env)

  matched_pids <- unique(result$pid[!is.na(result$pid)])
  matched_gids <- unique(result$geom_id[!is.na(result$geom_id)])

  # Active parish_link entries at target date
  pl_yr <- parish_link[parish_link$start <= yr & parish_link$end >= yr, , drop = FALSE]

  # Build primary match lookup: first result row per pid
  primary_idx <- which(!is.na(result$pid))
  primary_idx <- primary_idx[!duplicated(result$pid[primary_idx])]
  primary_lookup <- as.data.frame(result[primary_idx, , drop = FALSE])
  rownames(primary_lookup) <- as.character(primary_lookup$pid)

  # Step 1: Vectorized sibling expansion -- all same-pid geom_ids at target date
  sibling_df <- pl_yr[!is.na(pl_yr$pid) & pl_yr$pid %in% matched_pids &
                       !pl_yr$geom_id %in% matched_gids,
                       c("pid", "geom_id", "name"), drop = FALSE]

  new_rows <- NULL
  if (nrow(sibling_df) > 0) {
    prim <- primary_lookup[as.character(sibling_df$pid), , drop = FALSE]
    new_rows <- tibble(
      input = prim$input,
      pid = sibling_df$pid,
      geom_id = sibling_df$geom_id,
      name = sibling_df$name,
      match_type = "expanded",
      match_column = prim$match_column,
      distance = NA_real_,
      county_match = prim$county_match,
      multiple = TRUE
    )
    matched_gids <- c(matched_gids, sibling_df$geom_id)
  }

  # Step 2: Vectorized relation walk -- find uncovered geom_ids linked to matched pids
  rels <- relations[relations$type_id == "parish" & relations$relation == "successor",
                         c("parent_id", "child_id"), drop = FALSE]

  # Build geom_id -> pid lookup from ALL parish_link (not just target year)
  gid_to_pid <- parish_link[!is.na(parish_link$pid),
                            c("geom_id", "pid"), drop = FALSE]
  gid_to_pid <- gid_to_pid[!duplicated(gid_to_pid$geom_id), , drop = FALSE]
  gid_pid_map <- setNames(gid_to_pid$pid, gid_to_pid$geom_id)

  uncovered <- pl_yr$geom_id[!pl_yr$geom_id %in% matched_gids]
  if (length(uncovered) > 0) {
    # Walk relations (successors + predecessors) up to 3 hops in batch
    covered_new <- data.frame(geom_id = integer(0), pid = integer(0),
                              stringsAsFactors = FALSE)
    frontier <- uncovered

    for (hop in 1:3) {
      if (length(frontier) == 0) break
      # Find neighbors: children and parents of frontier
      children <- rels$child_id[rels$parent_id %in% frontier]
      parents  <- rels$parent_id[rels$child_id %in% frontier]
      neighbors <- unique(c(children, parents))
      # Map neighbors to pids
      nbr_pids <- gid_pid_map[as.character(neighbors)]
      # Keep only neighbors with matched pids
      hit_idx <- which(!is.na(nbr_pids) & nbr_pids %in% matched_pids)
      if (length(hit_idx) > 0) {
        hit_gids <- neighbors[hit_idx]
        hit_pids <- unname(nbr_pids[hit_idx])
        # Map back: which frontier geom_ids are linked to these hits?
        # For each uncovered gid still in frontier, check if any neighbor matched
        for_child <- rels[rels$parent_id %in% frontier & rels$child_id %in% hit_gids,
                          c("parent_id", "child_id"), drop = FALSE]
        for_parent <- rels[rels$child_id %in% frontier & rels$parent_id %in% hit_gids,
                           c("child_id", "parent_id"), drop = FALSE]
        names(for_parent) <- c("parent_id", "child_id")  # normalize columns
        links <- rbind(for_child, for_parent)
        if (nrow(links) > 0) {
          links$pid <- gid_pid_map[as.character(links$child_id)]
          links <- links[!is.na(links$pid) & links$pid %in% matched_pids, , drop = FALSE]
          links <- links[!duplicated(links$parent_id), , drop = FALSE]
          covered_new <- rbind(covered_new,
            data.frame(geom_id = links$parent_id, pid = links$pid,
                       stringsAsFactors = FALSE))
        }
      }
      # Advance frontier: uncovered gids that weren't resolved yet
      frontier <- frontier[!frontier %in% covered_new$geom_id]
    }

    if (nrow(covered_new) > 0) {
      # Only keep those actually active at target year
      covered_new <- covered_new[covered_new$geom_id %in% pl_yr$geom_id, , drop = FALSE]
      covered_new <- covered_new[!duplicated(covered_new$geom_id), , drop = FALSE]
      if (nrow(covered_new) > 0) {
        prim <- primary_lookup[as.character(covered_new$pid), , drop = FALSE]
        pl_names <- setNames(pl_yr$name, pl_yr$geom_id)
        rel_rows <- tibble(
          input = prim$input,
          pid = covered_new$pid,
          geom_id = covered_new$geom_id,
          name = unname(pl_names[as.character(covered_new$geom_id)]),
          match_type = "expanded",
          match_column = prim$match_column,
          distance = NA_real_,
          county_match = prim$county_match,
          multiple = TRUE
        )
        new_rows <- if (is.null(new_rows)) rel_rows else bind_rows(new_rows, rel_rows)
      }
    }
  }

  if (!is.null(new_rows) && nrow(new_rows) > 0) {
    result <- bind_rows(result, new_rows)
  }

  result
}


#' Manual NAPP PARSE parent mappings for non-territorial parishes
#'
#' Maps 24 NAPP PARSE codes (non-territorial parishes like military, mosaiska,
#' katolska, garrison, bruksforsamling, kapellforsamling) to their parent
#' territorial parish's forkod. These codes have no geometry in boundaries.
#' @noRd
napp_parent_mappings <- function(){
  # nadkod -> parent forkod
  c(
    # Stockholm city -> Stockholms domkyrkofors (forkod 18001)
    "18002000"  = 18001L,  # Klara
    "18003000"  = 18001L,  # Jakob/Johannes area
    "18003001"  = 18001L,  # Jakob och Johannes
    "18070000"  = 18001L,  # Svea livgarde
    "18071000"  = 18001L,  # Gota livgarde
    "18072000"  = 18001L,  # Military
    "18073000"  = 18001L,  # Military
    "18075000"  = 18001L,  # Livgardet till hast
    "18078000"  = 18001L,  # Skeppsholm
    "18090999"  = 18001L,  # Hovforsamlingen
    "18092999"  = 18001L,  # Tyska Sankta Gertrud
    "18093999"  = 18001L,  # Stockholms finska
    # Gothenburg city -> Domkyrkofors i Goteborg (forkod 148001)
    "148002000" = 148001L, # Goteborgs Kristine
    "148070000" = 148001L, # Goteborgs garnisonsfors
    "148090000" = 148001L, # Goteborgs Tyska
    "148092000" = 148001L, # Goteborgs mosaiska
    "148093000" = 148001L, # Goteborgs katolska
    # Other non-territorial
    "108002000" = 108001L, # Karlskrona amiralitetsfors -> Karlskrona stadsfors
    "88290000"  = 88201L,  # Oskarshamns mosaiska -> Oskarshamn
    "218090000" = 218001L, # Gavle katolska -> Gavle forsamling
    "206290000" = 206202L, # Vamhus baptistfors -> Vamhus
    # Territorial parishes without geometry in swehist
    "228310000" = 228371L, # Galsjo bruksfors -> Botea
    "143501000" = 143571L, # Grebbestad -> Kville
    "143507000" = 143571L  # Fjallbacka -> Kville
  )
}


#' Fallback matching for unmatched nadkod values
#'
#' When a nadkod (>=8 digit code) doesn't match directly, this function tries:
#' 1. Manual NAPP parent mapping (for non-territorial parishes)
#' 2. Forkod fallback (first 6 digits of the nadkod): the parish the code
#'    lies in. Only when the forkod names one parish with geometry: a parish
#'    part of 00 is a town or municipality code (Motala stad 058300002), and
#'    a forkod shared by several parishes cannot tell which one is meant.
#' @noRd
nadkod_fallback <- function(val, county_code, registry, link){
  val_str <- as.character(as.integer(val))
  parent_map <- napp_parent_mappings()

  # Step 1: Check manual NAPP parent mappings
  if (val_str %in% names(parent_map)) {
    fk <- parent_map[[val_str]]
    res <- match_one_code(fk, "forkod", county_code, registry)
    if (!is.na(res$pid)) {
      res$input <- as.character(val)
      res$match_type <- "napp_parent"
      res$match_column <- "forkod"
      return(res)
    }
  }

  # Step 2: Try forkod (first 6 digits of nadkod)
  # nadkod is CCMMPPVVV (9 digits); first 6 = CCMMPP = forkod
  nadkod_str <- sprintf("%09d", as.integer(val))
  fk <- as.integer(substr(nadkod_str, 1, 6))
  if (fk %% 100L == 0L) return(empty_match_result(val))
  hits <- registry[!is.na(registry$forkod) & registry$forkod == fk & !is.na(registry$geom_id), , drop = FALSE]
  if (nrow(hits) == 0) {
    hits <- link[!is.na(link$forkod) & link$forkod == fk & !is.na(link$pid), , drop = FALSE]
    hits <- hits[order(-hits$end), , drop = FALSE]
  }
  if (!is.na(county_code) && length(unique(hits$pid)) > 1) {
    in_county <- !is.na(hits$county) & hits$county == county_code
    if (any(in_county)) hits <- hits[in_county, , drop = FALSE]
  }
  if (length(unique(hits$pid)) != 1) return(empty_match_result(val))
  res <- pick_best_match(hits, val, "forkod_fallback", "forkod", county_code)
  res$multiple <- FALSE
  res
}


#' Parishes of a town or municipality given by its code
#'
#' A nadkod-like code with parish part 00 (Vaxjo stad 078000002) is
#' Riksarkivet's reference code of a municipality unit (towns, kopingar and
#' rural municipalities from 1863). Returns the parishes the hierarchy puts
#' inside the unit at \code{yr}, largest first; when the unit did not exist
#' at \code{yr} (before 1863), those of its nearest version.
#' @noRd
town_parishes <- function(val, yr, link){
  none <- data.frame(geom_id = integer(0), pid = integer(0), town = character(0))
  if (is.na(val)) return(none)
  env <- environment()
  data(boundaries, package = "swehist", envir = env)
  data(hierarchy, package = "swehist", envir = env)
  v <- as.numeric(val)
  if (v >= 1e7) {
    ref <- sprintf("SE/%09.0f", v)
    m <- boundaries[!is.na(boundaries$ref_code) & boundaries$ref_code == ref &
                      boundaries$type_id != "parish", , drop = FALSE]
  } else {
    # a forkod with parish part 00 ("018000"): the town among the municipalities
    # of that code (Stockholms stad, not Bromma or Spånga kommun)
    pre <- sprintf("SE/%06.0f", v)
    m <- boundaries[!is.na(boundaries$ref_code) & startsWith(boundaries$ref_code, pre) &
                      boundaries$type_id == "municipality", , drop = FALSE]
    m <- m[grepl("\\bstad$", m$name), , drop = FALSE]
    if (length(unique(m$ref_code)) > 1) return(none)
  }
  if (nrow(m) == 0) return(none)
  if (is.na(yr)) yr <- max(m$end)
  gap <- pmax(0L, m$start - yr, yr - m$end)
  m <- m[which.min(gap), , drop = FALSE]
  y <- min(max(yr, m$start), m$end)
  h <- hierarchy[hierarchy$parent_geom_id %in% m$geom_id & hierarchy$child_type == "Parish" &
                   hierarchy$start <= y & hierarchy$end >= y, , drop = FALSE]
  kids <- unique(h$child_geom_id)
  if (length(kids) == 0) return(none)
  area <- as.numeric(sf::st_area(sf::st_geometry(boundaries)[match(kids, boundaries$geom_id)]))
  # first the parish named after the town (Stockholms domkyrkoförsamling for Stockholms stad, not
  # Brännkyrka, the largest by area since 1913), then by area
  stem <- tolower(sub("s?\\s+(stad|k\u00f6ping|kommun|landskommun)$", "", m$name))
  named <- startsWith(tolower(boundaries$name[match(kids, boundaries$geom_id)]), stem)
  kids <- kids[order(!named, -area)]
  data.frame(geom_id = kids, pid = link$pid[match(kids, link$geom_id)], town = m$name)
}


#' Match a town or municipality code to its (largest) parish
#' @noRd
match_town_code <- function(val, yr, link){
  kids <- town_parishes(val, yr, link)
  if (nrow(kids) == 0) return(empty_match_result(val))
  tibble(
    input = as.character(val),
    pid = kids$pid[1],
    geom_id = kids$geom_id[1],
    name = kids$town[1],
    match_type = "via_municipality",
    match_column = "ref_code",
    distance = NA_real_,
    county_match = NA,
    multiple = nrow(kids) > 1
  )
}


#' Create an empty match result tibble (single NA row)
#' @noRd
empty_match_result <- function(input_val = NA_character_){
  tibble(
    input = as.character(input_val),
    pid = NA_integer_,
    geom_id = NA_integer_,
    name = NA_character_,
    match_type = NA_character_,
    match_column = NA_character_,
    distance = NA_real_,
    county_match = NA,
    multiple = FALSE
  )
}


# --- match_units ----------------------------------------------------------

#' Match unit names or codes to boundaries
#'
#' The primary function for matching administrative unit names or codes to
#' entries in \code{\link{boundaries}}, supporting all 11 administrative types.
#' Uses a knowledge base (\code{\link{unit_variants}}) of name variants,
#' historical spellings, and code mappings.
#'
#' For parishes, also supports numeric code matching (pid, nadkod, forkod,
#' dedik, dedikscb) with the same cascade as the internal
#' \code{match_parishes()} function.
#'
#' @param x character or numeric vector of unit names or codes to match.
#'   For parishes, numeric input is auto-detected by digit count.
#'   For all types, ref_codes (e.g. \code{"SE/150000000"}) and county
#'   letters/numbers are matched via the knowledge base.
#' @param type type of unit to match against
#' @param by matching column for parish codes: NULL (auto-detect), "pid",
#'   "nadkod", "dedik", "forkod", "dedikscb", or "name". Auto-detection looks
#'   at the whole vector and picks the code column with the most matches.
#'   Parish only; for other types codes are matched via the knowledge base.
#'   A dedik, forkod or nadkod is looked up among every code a parish has had
#'   (\code{\link{parish_codes}}), not only its latest: DDB gives a parish
#'   several codes, and a chapel's or lappförsamling's code reaches the parish
#'   it lies in. A town's name ("Stockholms stad") or a nadkod with parish part
#'   00 returns the town's parish named after it, with
#'   \code{match_type = "via_municipality"} and \code{multiple = TRUE};
#'   \code{expand = TRUE} returns all the town's parishes. A forkod with parish
#'   part 00 is always flagged (\code{multiple = TRUE}): it is a placeholder at
#'   municipality level that the sources fill differently (048600 is Strängnäs
#'   landsförsamling in Skatteverket's registry and Kärnbo in the 1930 census;
#'   018000 is Stockholm's total in the codes of the 1970s). The parish that
#'   carries the code is returned, or else the town of that code, always flagged.
#' @param county optional county constraint (numeric code 1-25, letter like
#'   "BD", or county name). Used as a soft constraint to disambiguate
#'   multiple matches: for parishes via the registry county, for other types
#'   by whether the unit lies in that county (at \code{date}, or 1900 when no
#'   date is given). For parish names, a step of the cascade whose hits all lie
#'   in other counties does not end the search; if no step finds the parish in
#'   the county, the best hit elsewhere is returned with \code{multiple = TRUE}.
#'   Length 1 or same length as \code{x}.
#' @param fallback optional character vector of names to try when the primary
#'   match on \code{x} fails. Must be the same length as \code{x}. Only
#'   used for parish code matching.
#' @param date optional year or date. When provided, the returned
#'   \code{geom_id} is resolved to the temporal version active at that date,
#'   walking \code{\link{relations}} if needed.
#' @param fuzzy logical; if TRUE, attempt approximate matching for names
#'   that fail the other steps. Default FALSE.
#' @param max_dist numeric; maximum edit distance for fuzzy matching, as a
#'   share of the longer of the two names (0.15: at most 1 edit in a name of
#'   7 letters, 2 in one of 14). Default 0.15.
#' @param expand logical; if TRUE and \code{type = "parish"}, expand matched
#'   parishes to include all \code{geom_id}s sharing the same \code{pid} at
#'   the target date (fills coverage holes). Ignored for non-parish types.
#'   Default FALSE.
#'
#' @return A tibble with one row per element of \code{x} (more with
#'   \code{expand = TRUE}):
#' \describe{
#'   \item{input}{The original input value}
#'   \item{geom_id}{Matched geometry ID from \code{boundaries}. When \code{date}
#'     is provided, resolved to the version active at that date where
#'     possible; check \code{active_at_date}.}
#'   \item{name}{Name of the matched unit (for parishes, the registry name)}
#'   \item{geom_name}{Name of the \code{geom_id} feature in \code{boundaries}.
#'     Differs from \code{name} when a parish without its own geometry, or
#'     one merged into another, is resolved to another parish's polygon.}
#'   \item{start}{Start year of the \code{geom_id} feature}
#'   \item{end}{End year of the \code{geom_id} feature}
#'   \item{match_type}{One of \code{"exact"}, \code{"normalized"},
#'     \code{"historical"}, \code{"kb_variant"}, \code{"kb_historical"},
#'     \code{"fuzzy"}, \code{"exact_code"}, \code{"forkod_fallback"},
#'     \code{"napp_parent"}, \code{"via_municipality"} or \code{"expanded"}}
#'   \item{distance}{Edit distance for fuzzy matches, NA otherwise}
#'   \item{multiple}{TRUE if the input matched more than one unit, or the
#'     answer is uncertain: a parish outside the given county, one parish
#'     standing for a town of several, or a parish that held the territory at
#'     the date of a code whose own parish did not exist yet}
#'   \item{active_at_date}{TRUE if the \code{geom_id} feature exists at
#'     \code{date}, FALSE if no version active at that date was found (the
#'     nearest version is returned), NA when no date is given}
#' }
#'
#' @details
#' Name matching proceeds as:
#' \enumerate{
#'   \item \strong{Exact} (case-insensitive): matches against unit names
#'     in \code{boundaries}.
#'   \item \strong{Normalized}: strips common Swedish administrative suffixes
#'     (lan, kommun, stad, harad, stift, etc.), folds diacritics,
#'     and converts w to v before matching.
#'   \item \strong{KB variant}: looks up the \code{\link{unit_variants}}
#'     knowledge base, which contains name aliases, registry names,
#'     ref_codes, county codes/letters, and manually curated entries.
#'   \item \strong{KB historical}: applies pre-1906 Swedish spelling
#'     transforms (qv to kv, w to v, carl to karl, etc.) and matches
#'     against the knowledge base.
#'   \item \strong{Fuzzy} (if \code{fuzzy = TRUE}): the nearest name by edit
#'     distance over the whole name, within \code{max_dist}.
#' }
#' Parish names also go through the pre-1906 spelling transforms (Hvetlanda,
#' Vesterhaninge), and the abbreviations "fors", "landsfors" and "stadsfors"
#' are read as "forsamling", "landsforsamling" and "stadsforsamling".
#'
#' For parish type with numeric input, code matching uses the same cascade as
#' \code{match_parishes()}: auto-detect by digit count, exact code lookup
#' (codes of every version of a parish, from \code{\link{parish_link}}),
#' nadkod-to-forkod fallback, town codes, and optional name fallback. A code
#' with parish part 00 names a town or municipality (Vaxjo stad 078000002, as
#' in SwedPop): it is matched to the parishes the hierarchy puts inside that
#' unit at \code{date} (\code{match_type = "via_municipality"}, \code{name} =
#' the town; the largest parish, all of them with \code{expand = TRUE}).
#' For other types, bare 8-9 digit codes are read as Riksarkivet reference
#' codes (\code{match_units(78000002, "municipality")} is Vaxjo stad).
#' The forkod fallback
#' returns the parish a sub-code (kbfd, bruks- or hospital congregation) lies
#' in, and only when the forkod names one parish: a town or municipality code
#' (parish part 00) or a forkod shared by several parishes gives no match.
#'
#' @export
#' @import dplyr
#' @examples
#' # Match municipality names
#' match_units(c("Lulea", "Malmo"), "municipality", date = 1900)
#'
#' # Match county by letter code
#' match_units("BD", "county")
#'
#' # Match by ref_code
#' match_units("SE/150000000", "county")
#'
#' # Match parish by forkod (auto-detected)
#' match_units(248201, "parish")
#'
#' # Fuzzy matching for hundreds
#' match_units("Goinge", "hundred", fuzzy = TRUE)
#'
match_units <- function(x,
                        type = c("parish", "county", "municipality",
                                 "pastorship", "contract", "diocese",
                                 "hundred", "magistrates_court",
                                 "district_court", "court_of_appeal",
                                 "bailiwick"),
                        by = NULL, county = NULL, fallback = NULL,
                        date = NULL, fuzzy = FALSE, max_dist = 0.15,
                        expand = FALSE){
  typed <- match.arg(type)

  if (length(x) == 0) return(finish_unit_matches(empty_unit_result(character(0)), date))

  if (!is.null(date) && length(date) > 1 && length(date) != length(x))
    stop("'date' must be length 1 or same length as 'x'")

  if (typed != "parish") {
    if (!is.null(by) || !is.null(fallback) || isTRUE(expand))
      warning("'by', 'fallback' and 'expand' only apply to type = \"parish\"; ignored",
              call. = FALSE)
    # Bare 8-9 digit codes are Riksarkivet reference codes (078000002 = SE/078000002)
    xin <- x
    bare <- grepl("^[0-9]{8,9}$", trimws(as.character(x)))
    if (any(bare)) {
      x <- as.character(x)
      x[bare] <- sprintf("SE/%09d", as.integer(trimws(x[bare])))
    }
    res <- match_units_kb(x, typed, county, date, fuzzy, max_dist)
    res$input <- as.character(xin)
    return(finish_unit_matches(res, date))
  }

  # Parish: ref_codes ("SE/...") go through the knowledge base, everything
  # else through the registry cascade (codes, names, county, date).
  is_ref <- is.character(x) & grepl("^SE/", x, ignore.case = TRUE)
  if (!any(is_ref)) {
    res <- match_parishes(x, by = by, fallback = fallback, county = county,
                          date = date, fuzzy = fuzzy, max_dist = max_dist,
                          expand = expand)
    return(finish_unit_matches(res, date))
  }
  if (isTRUE(expand))
    stop("'expand' cannot be combined with ref_code input")

  pick <- function(v, idx) if (is.null(v) || length(v) <= 1) v else v[idx]
  out <- vector("list", length(x))
  ref_idx <- which(is_ref)
  oth_idx <- which(!is_ref)
  ref_res <- match_units_kb(x[ref_idx], "parish", pick(county, ref_idx),
                            pick(date, ref_idx), fuzzy, max_dist)
  for (k in seq_along(ref_idx)) out[[ref_idx[k]]] <- ref_res[k, ]
  if (length(oth_idx) > 0) {
    oth <- match_parishes(x[oth_idx], by = by, fallback = pick(fallback, oth_idx),
                          county = pick(county, oth_idx), date = pick(date, oth_idx),
                          fuzzy = fuzzy, max_dist = max_dist)
    oth <- oth[, c("input", "geom_id", "name", "match_type", "distance", "multiple")]
    for (k in seq_along(oth_idx)) out[[oth_idx[k]]] <- oth[k, ]
  }
  finish_unit_matches(bind_rows(out), date)
}


#' Knowledge-base matching for any type (all non-parish types, parish ref_codes)
#' @noRd
match_units_kb <- function(x, typed, county, date, fuzzy, max_dist){
  env <- environment()
  data(boundaries, package = "swehist", envir = env)

  # Full registry for this type (unfiltered, for temporal resolution)
  all_reg <- sf::st_drop_geometry(boundaries[boundaries$type_id == typed, , drop = FALSE])
  all_reg <- all_reg[, c("geom_id", "name", "start", "end"), drop = FALSE]
  if (nrow(all_reg) == 0) {
    warning(sprintf("No %s features found", typed))
    return(empty_unit_result(x))
  }

  yrs <- if (is.null(date)) rep(list(NULL), length(x)) else as.list(date_years(date, length(x)))

  # County constraint: geom_ids of this type lying in each requested county
  in_county <- NULL
  if (!is.null(county)) {
    if (length(county) != 1 && length(county) != length(x))
      stop("'county' must be length 1 or same length as 'x'")
    in_county <- county_members(county, typed, if (is.null(date)) 1900L else unlist(yrs),
                                boundaries, length(x))
  }

  data(unit_variants, package = "swehist", envir = env)
  kb <- unit_variants[unit_variants$type_id == typed, , drop = FALSE]
  kb$historical <- hist_key(kb$variant_normalized)

  make_idx <- function(reg) data.frame(
    lower = tolower(reg$name), normalized = normalize_unit_name(reg$name),
    geom_id = reg$geom_id, reg_name = reg$name, start = reg$start, end = reg$end,
    stringsAsFactors = FALSE)
  idx_all <- make_idx(all_reg)

  results <- lapply(seq_along(x), function(i){
    yr <- yrs[[i]]
    idx <- if (is.null(yr)) idx_all else idx_all[idx_all$start <= yr & idx_all$end >= yr, , drop = FALSE]
    keep <- if (is.null(in_county)) NULL else in_county[[i]]
    match_one_unit(x[i], idx, kb, yr, all_reg, fuzzy, max_dist, keep)
  })
  bind_rows(results)
}


#' geom_ids of a type that lie in a county, per input element
#' @noRd
county_members <- function(county, typed, yrs, boundaries, n){
  data(parish_registry, package = "swehist", envir = environment())
  codes <- normalize_county(county, parish_registry)
  if (length(codes) == 1) codes <- rep(codes, n)
  if (length(yrs) == 1) yrs <- rep(yrs, n)
  lookup <- unique(parish_registry[!is.na(parish_registry$county),
                                   c("county", "county_name")])
  counties <- boundaries[boundaries$type_id == "county", , drop = FALSE]
  units <- boundaries[boundaries$type_id == typed, , drop = FALSE]
  pts <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(units)))
  cache <- list()
  lapply(seq_len(n), function(i){
    key <- paste(codes[i], yrs[i])
    if (!is.null(cache[[key]])) return(cache[[key]])
    cname <- lookup$county_name[match(codes[i], lookup$county)]
    if (is.na(cname)) return(NULL)
    cty <- counties[startsWith(counties$name, cname) &
                    counties$start <= yrs[i] & counties$end >= yrs[i], ]
    if (nrow(cty) == 0) return(NULL)
    inside <- lengths(sf::st_intersects(pts, sf::st_union(sf::st_geometry(cty)))) > 0
    cache[[key]] <<- units$geom_id[inside]
    cache[[key]]
  })
}


#' Empty unit match rows (one per input)
#' @noRd
empty_unit_result <- function(x){
  tibble(
    input = as.character(x), geom_id = rep(NA_integer_, length(x)),
    name = NA_character_, match_type = NA_character_,
    distance = NA_real_, multiple = FALSE
  )
}


#' Add geom_name, start, end and active_at_date from boundaries
#' @noRd
finish_unit_matches <- function(res, date){
  env <- environment()
  data(boundaries, package = "swehist", envir = env)
  b <- sf::st_drop_geometry(boundaries)
  i <- match(res$geom_id, b$geom_id)
  res$geom_name <- b$name[i]
  res$start <- b$start[i]
  res$end <- b$end[i]
  res$active_at_date <- if (is.null(date) || nrow(res) == 0) {
    rep(NA, nrow(res))
  } else {
    yr <- if (length(date) == 1) rep(get_year(date), nrow(res)) else date_years(date, nrow(res))
    ifelse(is.na(res$geom_id), NA, res$start <= yr & res$end >= yr)
  }
  res[, c("input", "geom_id", "name", "geom_name", "start", "end",
          "match_type", "distance", "multiple", "active_at_date")]
}


#' Normalize Swedish administrative unit name for matching
#' @noRd
normalize_unit_name <- function(x){
  x <- tolower(trimws(x))
  # strip date suffixes (" -1823", " 1680-1780", " 1971-"), but only after a space or
  # hyphen so codes like "se/258401000" stay intact. the open-ended form matters.
  x <- sub("(\\s+-?|-)\\d{4}(-\\d{0,4})?\\s*$", "", x)
  # Strip admin type suffixes
  x <- sub("\\s+l\u00e4n$", "", x)
  x <- sub("\\s+kommun$", "", x)

  x <- sub("\\s+stad$", "", x)
  x <- sub("\\s+k\u00f6ping$", "", x)
  x <- sub("\\s+h\u00e4rad$", "", x)
  x <- sub("\\s+stift$", "", x)
  x <- sub("\\s+pastorat$", "", x)
  x <- sub("\\s+kontrakt$", "", x)
  x <- sub("\\s+f\u00f6gderi$", "", x)
  x <- sub("\\s+domsaga$", "", x)
  x <- sub("\\s+tingslag$", "", x)
  x <- sub("\\s+tingsr\u00e4tt$", "", x)
  x <- sub("\\s+hovr\u00e4tt$", "", x)
  # Forsamling variants
  x <- sub("\\s*(stads|lands|domkyrko)?f\u00f6rsamlings?$", "", x)
  x <- sub("\\s*(stads|lands|domkyrko)?forsamlings?$", "", x)
  # Strip trailing parenthetical
  x <- sub("\\s*\\([^)]*\\)\\s*$", "", x)
  # Strip trailing stads/lands/domkyrko
  x <- sub("\\s+(stads|lands|domkyrko)$", "", x)
  # w -> v
  x <- chartr("w", "v", x)
  # Fold Swedish diacritics
  x <- chartr("\u00e5\u00e4\u00f6\u00e9", "aaoe", x)
  # Strip genitive "s" (conservative: after vowel + consonant)
  x <- sub("([aeiouy][bcdfghjklmnpqrstvwxz])s$", "\\1", x)
  # Collapse whitespace
  gsub("\\s+", " ", trimws(x))
}


#' Normalize pre-1906 Swedish spelling
#'
#' Applies historical spelling transforms ported from geocodeortnamn.
#' @param x character vector (should be lowercased)
#' @return character vector with modern spelling
#' @noRd
normalize_historical_spelling <- function(x){
  # Multi-character patterns (longest first)
  x <- gsub("fv", "v", x, fixed = TRUE)              # tofva -> tova
  x <- gsub("qv", "kv", x, fixed = TRUE)             # qvarn -> kvarn
  x <- gsub("\\bhv", "v", x, perl = TRUE)            # hvetlanda -> vetlanda (1906)
  x <- gsub("\\bth", "t", x, perl = TRUE)            # thors -> tors (word-initial)
  x <- gsub("sch", "sk", x, fixed = TRUE)            # scheleftea -> skelleftea
  x <- gsub("dh", "d", x, fixed = TRUE)              # fredh -> fred
  x <- gsub("dt\\b", "t", x, perl = TRUE)            # arndt -> arnt (word-final)
  x <- gsub("gh\\b", "g", x, perl = TRUE)            # bergh -> berg (word-final)
  # Single-character substitutions
  x <- gsub("w", "v", x, fixed = TRUE)               # wiken -> viken
  x <- gsub("q", "k", x, fixed = TRUE)               # remaining q (after qv handled)
  x <- gsub("\\bvest(er|ra)", "v\u00e4st\\1", x, perl = TRUE)  # vesterhaninge -> vasterhaninge
  x <- gsub("\\bvestre\\b", "v\u00e4stra", x, perl = TRUE)     # vestre -> vastra
  # ij->i must run AFTER w->v so "wijka" -> "vijka" -> "vika"
  x <- gsub("ij", "i", x, fixed = TRUE)              # vijka -> vika
  # Context-dependent
  x <- gsub("([\u00e5\u00e4\u00f6aeiouy])ck([\u00e5\u00e4\u00f6aeiouy]|\\b)",
            "\\1k\\2", x, perl = TRUE)                # backen -> baken (not stockholm)
  x <- gsub("([a-z\u00e5\u00e4\u00f6])f([\u00e5\u00e4\u00f6aeiouy])",
            "\\1v\\2", x, perl = TRUE)                # lofas -> lovas
  # Name-specific word-initial patterns
  x <- gsub("\\bcarl", "karl", x, perl = TRUE)       # carlsberg -> karlsberg
  x <- gsub("\\bchrist", "krist", x, perl = TRUE)    # christineberg -> kristineberg
  x <- gsub("\\baf(?!v)", "av", x, perl = TRUE)      # afsta -> avsta (1906)
  x <- gsub("ki\u00f6", "k\u00f6", x, fixed = TRUE)  # kioping -> koping
  # 1889 SAOL reform: e->a in whitelisted stems
  x <- gsub("\\bjern", "j\u00e4rn", x, perl = TRUE)      # jernberga -> jarnberga
  x <- gsub("\\bhjelm", "hj\u00e4lm", x, perl = TRUE)    # hjelmseryd -> hjalmseryd
  x <- gsub("\\bstjern", "stj\u00e4rn", x, perl = TRUE)  # stjerneberg -> stjarneberg
  x <- gsub("\\belf", "\u00e4lv", x, perl = TRUE)         # elfkarleby -> alvkarleby
  x <- gsub("\\belm", "\u00e4lm", x, perl = TRUE)         # elmarsrum -> almarsrum
  x
}


#' Resolve a geom_id to the temporal version active at a target year
#'
#' Walks relations towards the date (up to 12 hops) to find the version
#' of an administrative unit active at a given year.
#' @param geom_id integer geom_id from a KB match
#' @param yr integer target year
#' @param type_id character type identifier
#' @param sp_data data.frame with geom_id, name, start, end (from boundaries, no geometry)
#' @return data.frame row from sp_data if found, or NULL
#' @noRd
resolve_temporal <- function(geom_id, yr, type_id, sp_data){
  # Check if already active
  row <- sp_data[sp_data$geom_id == geom_id, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  if (row$start[1] <= yr && row$end[1] >= yr) {
    return(row[1, , drop = FALSE])
  }

  # Walk relations towards the date only: predecessors for an earlier date,
  # successors for a later one (walking both ways reached Karesuando from
  # Kiruna stad through the 1971 Kiruna kommun)
  env <- environment()
  data(relations, package = "swehist", envir = env)
  rels <- relations[relations$type_id == type_id & relations$relation == "successor",
                         c("parent_id", "child_id"), drop = FALSE]
  back <- yr < row$start[1]

  current_ids <- geom_id
  visited <- geom_id
  # 12 hops: a code used long before its parish was founded walks back through every
  # version in between (Kågedalen's DDB code in 1761 -> Skellefteå)
  for (hop in 1:12) {
    next_ids <- if (back) rels$parent_id[rels$child_id %in% current_ids]
                else rels$child_id[rels$parent_id %in% current_ids]
    next_ids <- setdiff(unique(next_ids), visited)
    if (length(next_ids) == 0) break

    # Check if any is active at yr
    candidates <- sp_data[sp_data$geom_id %in% next_ids, , drop = FALSE]
    active <- candidates[candidates$start <= yr & candidates$end >= yr, , drop = FALSE]
    # the unit under its own name can be a hop further than an active neighbour, so look for
    # a name match at every hop and keep the first hop's answer only if none is found
    if (nrow(active) == 1) {
      # the only unit active at the date, but under another name: a guess, so flag it
      out <- active
      attr(out, "several") <- TRUE
      return(out)
    }
    if (nrow(active) > 1) {
      # the one that keeps the name (Aby, not Backebo), else the one sharing most
      # territory. names compared without the type word, since "X stad" becomes "X kommun"
      same <- active[active$name == row$name[1], , drop = FALSE]
      if (nrow(same) == 0) {
        nm <- normalize_unit_name(row$name[1])
        if (nzchar(nm)) same <- active[normalize_unit_name(active$name) == nm, , drop = FALSE]
      }
      if (nrow(same) == 1) return(same)
      if (nrow(same) > 1) active <- same
      several <- TRUE
      data(boundaries, package = "swehist", envir = env)
      g0 <- sf::st_geometry(boundaries)[boundaries$geom_id == geom_id]
      ga <- sf::st_geometry(boundaries)[match(active$geom_id, boundaries$geom_id)]
      shared <- vapply(seq_along(ga), function(k)
        sum(as.numeric(sf::st_area(suppressMessages(sf::st_intersection(ga[k], g0))))), numeric(1))
      out <- active[which.max(shared), , drop = FALSE]
      attr(out, "several") <- several
      return(out)
    }

    visited <- c(visited, next_ids)
    current_ids <- next_ids
  }

  NULL
}


#' Match a single unit name against name index
#' @noRd
match_one_unit <- function(val, idx, kb, yr, sp_data, fuzzy, max_dist,
                           keep = NULL){
  empty <- empty_unit_result(val)

  if (is.na(val) || !nzchar(trimws(val))) return(empty)

  # County soft constraint: among several candidates, prefer those in the county
  prefer <- function(h, id_col = "geom_id"){
    if (is.null(keep) || nrow(h) <= 1) return(h)
    k <- h[[id_col]] %in% keep
    if (any(k)) h[k, , drop = FALSE] else h
  }

  val_lower <- tolower(trimws(val))
  val_norm <- normalize_unit_name(val)

  # Step 1: Exact match (case-insensitive) against boundaries name index
  exact <- idx[idx$lower == val_lower, , drop = FALSE]
  if (nrow(exact) > 0) {
    return(unit_match_result(val, prefer(exact), "exact", multiple = nrow(exact) > 1))
  }

  # Step 2: Normalized match against boundaries name index
  norm <- idx[idx$normalized == val_norm, , drop = FALSE]
  if (nrow(norm) > 0) {
    return(unit_match_result(val, prefer(norm), "normalized", multiple = nrow(norm) > 1))
  }

  # Step 3: KB variant lookup (ref_codes only match exactly)
  if (!is.null(kb) && nrow(kb) > 0) {
    kb_hits <- if (grepl("^se/", val_lower)) {
      kb[kb$variant == val_lower, , drop = FALSE]
    } else {
      # tolower on the stored variant as well: variants keep their capitals for display
      # ("Norra Angermanland"), and the raw comparison silently missed every one of them
      kb[tolower(kb$variant) == val_lower | kb$variant_normalized == val_norm, , drop = FALSE]
    }
    if (nrow(kb_hits) > 0) {
      result <- resolve_kb_hit(val, prefer(kb_hits), yr, sp_data, "kb_variant")
      if (!is.null(result)) return(result)
    }

    # Step 4: KB + historical spelling normalization
    val_hist <- hist_key(val_norm)
    if (val_hist != val_norm) {
      kb_hist <- kb[kb$historical == val_hist, , drop = FALSE]
      if (nrow(kb_hist) > 0) {
        result <- resolve_kb_hit(val, prefer(kb_hist), yr, sp_data, "kb_historical")
        if (!is.null(result)) return(result)
      }
    }
  }

  # Step 5: Fuzzy match against boundaries name index
  if (fuzzy) {
    fuzzy_rows <- fuzzy_candidates(val_norm, idx$normalized, max_dist)
    if (length(fuzzy_rows) > 0) {
      fuzz <- idx[fuzzy_rows, , drop = FALSE]
      dists <- as.integer(utils::adist(val_norm, fuzz$normalized))
      fuzz <- prefer(fuzz[dists == min(dists), , drop = FALSE])
      return(unit_match_result(val, fuzz, "fuzzy", distance = min(dists),
                               multiple = nrow(fuzz) > 1))
    }
  }

  empty
}


#' Resolve a KB hit to a match result, with temporal resolution if needed
#' @noRd
resolve_kb_hit <- function(input_val, kb_hits, yr, sp_data, match_type){
  # Sort by priority (lower = better)
  kb_hits <- kb_hits[order(kb_hits$priority), , drop = FALSE]

  if (!is.null(yr)) {
    # Prefer hits already active at requested year
    active <- kb_hits[kb_hits$start <= yr & kb_hits$end >= yr, , drop = FALSE]
    if (nrow(active) > 0) {
      multiple <- length(unique(active$geom_id)) > 1
      best <- active[1, , drop = FALSE]
      return(tibble(
        input = as.character(input_val),
        geom_id = best$geom_id,
        name = best$canonical_name,
        match_type = match_type,
        distance = NA_real_,
        multiple = multiple
      ))
    }

    # No active hit -- try temporal resolution via relations
    best <- kb_hits[1, , drop = FALSE]
    resolved <- resolve_temporal(best$geom_id, yr, best$type_id, sp_data)
    if (!is.null(resolved)) {
      return(tibble(
        input = as.character(input_val),
        geom_id = resolved$geom_id,
        name = resolved$name,
        match_type = match_type,
        distance = NA_real_,
        # the walk says whether the name decided or the territory did; the unit path used to drop
        # that and report every walked answer as certain
        multiple = isTRUE(attr(resolved, "several"))
      ))
    }
  }

  # No date filter or temporal resolution failed -- return best KB hit
  multiple <- length(unique(kb_hits$geom_id)) > 1
  best <- kb_hits[1, , drop = FALSE]
  tibble(
    input = as.character(input_val),
    geom_id = best$geom_id,
    name = best$canonical_name,
    match_type = match_type,
    distance = NA_real_,
    multiple = multiple
  )
}


#' Create match result from unit name index hits
#' @noRd
unit_match_result <- function(input_val, hits, match_type, distance = NA_real_,
                              multiple = nrow(hits) > 1){
  best <- hits[1, , drop = FALSE]
  tibble(
    input = as.character(input_val),
    geom_id = best$geom_id,
    name = best$reg_name,
    match_type = match_type,
    distance = distance,
    multiple = multiple
  )
}
