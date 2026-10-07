# Property tests on the shipped data (VALIDATION.md step 2). Heavier than unit
# tests (spatial unions for every type across the period), so skipped on CRAN.

context("data properties")

b <- NULL
setup_data <- function() {
  if (is.null(b)) {
    data(boundaries, package = "swehist", envir = environment())
    b <<- boundaries
  }
  b
}

test_that("every type tiles without overlaps and matches the coverage table", {
  skip_on_cran()
  b <- setup_data()
  cov <- swehist:::.coverage
  swe <- sf::st_union(sf::st_geometry(swehist::sweden))
  swe_area <- as.numeric(sf::st_area(swe))
  for (tid in unique(b$type_id)) {
    for (yr in seq(1650, 1990, by = 40)) {
      x <- b[b$type_id == tid & b$start <= yr & b$end >= yr, ]
      if (nrow(x) == 0) next
      tot <- sum(as.numeric(sf::st_area(x)))
      uni <- as.numeric(sf::st_area(sf::st_union(sf::st_geometry(x))))
      expect_lt((tot - uni) / uni, 0.01, label = sprintf("%s %d overlap", tid, yr))
      # coverage = area inside the parish territory (`sweden`) over its area
      tot_in <- sum(as.numeric(sf::st_area(suppressWarnings(sf::st_intersection(sf::st_geometry(x), swe)))))
      expected <- cov$share[cov$type_id == tid & cov$year == yr]
      expect_equal(min(1, tot_in / swe_area), expected, tolerance = 0.01,
                   label = sprintf("%s %d coverage", tid, yr))
    }
  }
})

test_that("assign_parent and get_children agree with the hierarchy and each other", {
  skip_on_cran()
  data(hierarchy, package = "swehist", envir = environment())
  set.seed(42)
  lookup <- c("County" = "county", "Municipality" = "municipality", "Bailiwick" = "bailiwick",
              "Hundred" = "hundred", "Magistrates Court" = "magistrates_court",
              "District Court" = "district_court", "Court of Appeal" = "court_of_appeal",
              "Diocese" = "diocese", "Contract" = "contract", "Pastorship" = "pastorship")
  child_lookup <- c(lookup, "Parish" = "parish")
  for (pt in names(lookup)) {
    # the API covers 1634-1990
    links <- hierarchy[hierarchy$parent_type == pt & hierarchy$end >= 1634, ]
    links <- links[sample(nrow(links), min(40, nrow(links))), ]
    for (k in seq_len(nrow(links))) {
      l <- links[k, ]
      yr <- max(1634L, l$start) + (l$end - max(1634L, l$start)) %/% 2
      up <- assign_parent(l$child_geom_id, yr, lookup[[pt]])
      expect_equal(up$parent_geom_id, l$parent_geom_id,
                   label = sprintf("assign_parent %s -> %s (%d at %d)", l$child_type, pt, l$child_geom_id, yr))
      pname <- b$name[b$geom_id == l$parent_geom_id]
      kids <- suppressWarnings(get_children(yr, lookup[[pt]], pname, child_lookup[[l$child_type]],
                                            recursive = FALSE, format = "meta", exact = TRUE))
      expect_true(l$child_geom_id %in% kids$geom_id,
                  label = sprintf("get_children %s '%s' has %d at %d", pt, pname, l$child_geom_id, yr))
    }
  }
})

test_that("period map groups are exactly the relation-connected units in the range", {
  skip_on_cran()
  data(relations, package = "swehist", envir = environment())
  set.seed(7)
  for (tid in c("parish", "county", "municipality", "pastorship", "bailiwick", "hundred", "diocese")) {
    for (k in 1:3) {
      y <- sample(1640:1980, 1); x <- y + sample(5:60, 1); x <- min(x, 1990)
      m <- suppressWarnings(get_boundaries(c(y, x), tid))
      lk <- m$lookup
      expect_false(anyDuplicated(lk$geom_id) > 0, label = sprintf("%s %d-%d unique", tid, y, x))
      active <- b$geom_id[b$type_id == tid & b$start <= x & b$end >= y]
      expect_setequal(lk$geom_id, active)
      r <- relations[relations$type_id == tid & relations$relation == "successor" &
                     relations$transition_year >= y & relations$transition_year < x, ]
      expected <- swehist:::create_block(c(r$parent_id, active), c(r$child_id, active))
      grp_expected <- tapply(c(r$parent_id, active), expected, function(v) paste(sort(unique(v)), collapse = ","))
      grp_actual <- tapply(lk$geom_id, lk$geomid, function(v) paste(sort(v), collapse = ","))
      expect_setequal(unname(grp_actual), unname(grp_expected))
      expect_equal(nrow(m$map), length(grp_expected))
    }
  }
})

test_that("unit names, codes and ref_codes match back to their own unit", {
  skip_on_cran()
  data(parish_link, package = "swehist", envir = environment())
  set.seed(3)
  # Parishes by forkod (codes unique among the versions of one pid)
  pl <- parish_link[!is.na(parish_link$forkod), ]
  one <- pl[!duplicated(pl$forkod) & !pl$forkod %in% pl$forkod[duplicated(pl$forkod)], ]
  s <- one[sample(nrow(one), 300), ]
  res <- match_units(s$forkod)
  got_pid <- parish_link$pid[match(res$geom_id, parish_link$geom_id)]
  expect_gt(mean(got_pid == s$pid, na.rm = TRUE), 0.99)
  expect_gt(mean(!is.na(res$geom_id)), 0.99)
  # Any type by ref_code
  s <- b[sample(nrow(b), 300), ]
  res <- match_units(s$ref_code[s$type_id == "parish"], "parish")
  expect_true(all(b$ref_code[match(res$geom_id, b$geom_id)] == s$ref_code[s$type_id == "parish"]))
  # Non-parish names that are unique within their type match themselves
  for (tid in setdiff(unique(b$type_id), "parish")) {
    m <- sf::st_drop_geometry(b[b$type_id == tid, ])
    u <- m[!m$name %in% m$name[duplicated(m$name)], ]
    if (nrow(u) == 0) next
    u <- u[sample(nrow(u), min(60, nrow(u))), ]
    res <- match_units(u$name, tid)
    expect_equal(res$geom_id, u$geom_id, label = sprintf("%s names", tid))
  }
})

test_that("geom_ids match the frozen lookup", {
  lk_file <- testthat::test_path("..", "..", "data-raw", "geom_id_lookup.csv")
  skip_if_not(file.exists(lk_file))
  b <- setup_data()
  lk <- utils::read.csv(lk_file, stringsAsFactors = FALSE)
  m <- sf::st_drop_geometry(b)
  j <- merge(m, lk, by = "geom_id", suffixes = c("", ".lk"))
  expect_equal(nrow(j), nrow(m))
  # topo_id is NA for a unit that only SCB or the statskalender has, and the lookup is a CSV, where
  # that NA is an empty field
  blank_na <- function(x) { if (is.character(x)) x[!is.na(x) & !nzchar(x)] <- NA; x }
  same <- function(a, b) { a <- blank_na(a); b <- blank_na(b)
    (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b) }
  expect_true(all(same(j$type_id, j$type_id.lk) & same(j$topo_id, j$topo_id.lk) &
                  same(j$start, j$start.lk) & same(j$end, j$end.lk)))
})
