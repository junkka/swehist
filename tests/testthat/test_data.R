# Invariants of the shipped data (see data-raw/model/m4_derive.R and VALIDATION.md)

context("data invariants")

test_that("all spatial datasets share EPSG:3006", {
  data(boundaries, package = "swehist")
  data(sweden, package = "swehist")
  data(hist_town, package = "swehist")
  expect_equal(sf::st_crs(boundaries)$epsg, 3006L)
  expect_true(sf::st_crs(boundaries) == sf::st_crs(sweden))
  expect_true(sf::st_crs(boundaries) == sf::st_crs(hist_town))
  # Overlays work without transforming (a few town points lie just outside
  # the generalised coastline)
  j <- sf::st_join(county_towns(1900), get_boundaries(1900, "county"))
  expect_true(mean(!is.na(j$name)) > 0.8)
})

test_that("no double-encoded names", {
  data(boundaries, package = "swehist")
  data(hierarchy, package = "swehist")
  data(unit_variants, package = "swehist")
  expect_false(any(grepl("Ã", boundaries$name)))
  expect_false(any(grepl("Ã", hierarchy$parent_name)))
  expect_false(any(grepl("Ã", unit_variants$canonical_name)))
  expect_true(nrow(get_children(1900, "diocese", "Västerås", "contract")) > 0)
})

test_that("relations link existing units of one type, year after year", {
  data(boundaries, package = "swehist")
  data(relations, package = "swehist")
  b <- sf::st_drop_geometry(boundaries)
  p <- b[match(relations$parent_id, b$geom_id), ]
  c <- b[match(relations$child_id, b$geom_id), ]
  expect_false(anyNA(p$geom_id) || anyNA(c$geom_id))
  expect_true(all(p$type_id == c$type_id))
  expect_true(all(c$start == p$end + 1L))
  expect_true(any(relations$type_id == "diocese"))
})

test_that("hierarchy periods lie within both units' lifetimes", {
  data(boundaries, package = "swehist")
  data(hierarchy, package = "swehist")
  b <- sf::st_drop_geometry(boundaries)
  ch <- b[match(hierarchy$child_geom_id, b$geom_id), ]
  pa <- b[match(hierarchy$parent_geom_id, b$geom_id), ]
  expect_true(all(hierarchy$start >= ch$start & hierarchy$end <= ch$end))
  ok <- is.na(hierarchy$parent_geom_id) |
    (hierarchy$start >= pa$start & hierarchy$end <= pa$end)
  expect_true(all(ok))
})

test_that("assign_parent county agrees with spatial containment", {
  p <- get_boundaries(1900, "parish")
  res <- assign_parent(p$geom_id, 1900, "county")
  c1900 <- get_boundaries(1900, "county")
  pts <- suppressWarnings(sf::st_point_on_surface(p))
  sp <- sf::st_join(pts, c1900[, c("geom_id")], join = sf::st_within)
  both <- !is.na(res$parent_geom_id) & !is.na(sp$geom_id.y)
  expect_true(mean(res$parent_geom_id[both] == sp$geom_id.y[both]) > 0.99)
  expect_true(mean(!is.na(res$parent_geom_id)) > 0.95)
  # after the 1971 reform too
  p90 <- get_boundaries(1990, "parish")
  expect_true(mean(!is.na(assign_parent(p90$geom_id, 1990, "county")$parent_geom_id)) > 0.95)
})

test_that("hundreds cover the country, courts of appeal are several", {
  h <- suppressWarnings(get_boundaries(1800, "hundred"))
  expect_true(nrow(h) > 250)
  karlstad <- h[h$name == "Karlstads stad", ]
  expect_true(all(as.numeric(sf::st_area(karlstad)) < 200e6))
  expect_true(any(grepl("^Vadsbo", h$name)))
  # härader, tingslag and the towns outside them are all in this type, and together they cover
  # the country: no partial-coverage warning, which the earlier build gave in every year (33% coverage)
  expect_true(any(h$kind == "tingslag"))
  expect_gt(sum(as.numeric(sf::st_area(h))) / as.numeric(sf::st_area(sweden)), 0.95)
  coa <- get_boundaries(1900, "court_of_appeal")
  expect_true(nrow(coa) >= 3)
})

test_that("county history does not chain unrelated counties", {
  h <- unit_history("^Malmöhus län$", "county")
  expect_true(nrow(h) < 15)
})

test_that("no single-year map has same-type overlaps above 1% of area", {
  for (tid in c("parish", "county", "municipality", "pastorship", "contract",
                "bailiwick", "hundred", "magistrates_court")) {
    x <- suppressWarnings(get_boundaries(1850, tid))
    if (nrow(x) < 2) next
    tot <- sum(as.numeric(sf::st_area(x)))
    uni <- as.numeric(sf::st_area(sf::st_union(x)))
    expect_lt((tot - uni) / uni, 0.01, label = tid)
  }
})

test_that("Gotland is under Svea hovrätt (source correction)", {
  p <- get_boundaries(1900, "parish")
  got <- p[grepl("^Visby", p$name), ]
  res <- assign_parent(got$geom_id, 1900, "court_of_appeal")
  expect_true(all(res$parent_name == "Svea hovrätt"))
  coa <- get_boundaries(1900, "court_of_appeal")
  gota <- coa[coa$name == "Göta hovrätt", ]
  expect_equal(lengths(sf::st_intersects(sf::st_point_on_surface(sf::st_geometry(got)[1]), gota)), 0L)
})

test_that("Norrland parishes reach their court of appeal", {
  p <- get_boundaries(1900, "parish")
  nb <- p[grepl("^(Luleå|Umeå|Östersunds|Sundsvalls)", p$name), ]
  expect_true(nrow(nb) >= 3)
  mc <- assign_parent(nb$geom_id, 1900, "magistrates_court")
  coa <- assign_parent(nb$geom_id, 1900, "court_of_appeal")
  expect_false(anyNA(mc$parent_geom_id))
  expect_true(all(coa$parent_name == "Svea hovrätt"))
})

test_that("every hierarchy link names its source and its grade", {
  data(hierarchy, package = "swehist")
  expect_true(all(c("source", "grade") %in% names(hierarchy)))
  expect_false(anyNA(hierarchy$source))
  expect_false(anyNA(hierarchy$grade))
  # A link is either direct (the register, SCB, SFGT, the statskalender, a correction, or the
  # geometry) or reached through one intermediate unit, which is recorded as via_<type>_<source>.
  direct <- c("register_dated", "register_undated", "register", "scb", "sfgt", "statskalender",
              "correction", "containment", "derived")
  # a source with "+gap" filled a hole a source's own dating left (m3b)
  hierarchy$source <- sub("\\+gap$", "", hierarchy$source)
  expect_true(all(hierarchy$source %in% direct | grepl("^via_[a-z_]+_", hierarchy$source)))
  expect_true(all(hierarchy$grade %in% c("corrected", "sourced", "chained_sourced", "chained",
                                         "asserted", "derived")))
  src <- function(pt, ct) unique(hierarchy$source[hierarchy$parent_type == pt & hierarchy$child_type == ct])
  grd <- function(pt, ct) hierarchy$grade[hierarchy$parent_type == pt & hierarchy$child_type == ct]
  # The county of a parish comes from SCB's codes, from SFGT or through its municipality or
  # bailiwick, never from geometry alone
  expect_true(all(src("County", "Parish") %in% c("scb", "sfgt", "correction", "register_dated",
                                                 "register_undated") |
                    grepl("^via_(municipality|bailiwick)_", src("County", "Parish"))))
  # Since the chain grade became weakest-link (2026-09-29) a county reached through a bailiwick
  # whose own parish link is containment is graded derived, as it should be. It is a tail, not a
  # route: assert that rather than "never", which only held while the chain grade read the upper
  # step alone. 3 links, 188 parish-years at the time of writing.
  cy <- hierarchy[hierarchy$parent_type == "County" & hierarchy$child_type == "Parish", ]
  expect_lt(sum((cy$end - cy$start + 1)[cy$grade == "derived"]) /
              sum(cy$end - cy$start + 1), 0.001)
  # A court of appeal is reached through a court for the great majority of parish-periods
  coa <- hierarchy$source[hierarchy$parent_type == "Court of Appeal" &
                            hierarchy$child_type == "Parish"]
  expect_gt(mean(grepl("^via_(magistrates_court|district_court)_", coa)), 0.8)
})

test_that("transfers are recorded but do not merge period maps", {
  data(relations, package = "swehist")
  expect_true(all(relations$relation %in% c("successor", "transfer")))
  expect_true(any(relations$relation == "transfer"))
  h1 <- unit_history("^Malmöhus län$", "county")
  h2 <- unit_history("^Malmöhus län$", "county", transfers = TRUE)
  expect_true(nrow(h1) < 15)
  expect_true(nrow(h2) >= nrow(h1))
})

test_that("Skåne and Blekinge courts reach their court of appeal", {
  p <- get_boundaries(1900, "parish")
  sk <- p[grepl("^(Lunds domkyrko|Malmö Sankt Petri|Karlskrona stads)", p$name), ]
  expect_true(nrow(sk) >= 2)
  res <- assign_parent(sk$geom_id, 1900, "court_of_appeal")
  expect_true(all(res$parent_name == "Hovrätten över Skåne och Blekinge"))
  all_p <- assign_parent(p$geom_id, 1900, "court_of_appeal")
  expect_gt(mean(!is.na(all_p$parent_geom_id)), 0.95)
})

test_that("Finnerödja lies in Göta hovrätt and Vadsbo domsaga only (source correction)", {
  p <- get_boundaries(1800, "parish")
  f <- p[grepl("^Finnerödja", p$name), ]
  pt <- sf::st_point_on_surface(sf::st_geometry(f))
  mc <- get_boundaries(1800, "magistrates_court")
  coa <- get_boundaries(1800, "court_of_appeal")
  expect_equal(mc$name[lengths(sf::st_intersects(mc, pt)) > 0], "Vadsbo domsaga")
  expect_equal(coa$name[lengths(sf::st_intersects(coa, pt)) > 0], "Göta hovrätt")
})

test_that("Kalmar stift exists 1603-1914 (source correction)", {
  d <- function(y) sort(get_boundaries(y, "diocese")$name)
  # 13 in 1900: the 11 of the source, plus Kalmar stift and Stockholms stads konsistorium, both
  # corrections (the earlier build had 12, without the konsistorium)
  expect_length(d(1900), 13)
  expect_true("Stockholms stads konsistorium" %in% d(1900))
  expect_true("Kalmar stift" %in% d(1800))
  expect_false("Kalmar stift" %in% d(1950))
  p <- get_boundaries(1900, "parish")
  k <- p[grepl("^(Kalmar domkyrko|Borgholms|Mönsterås)", p$name), ]
  expect_true(nrow(k) >= 2)
  expect_true(all(assign_parent(k$geom_id, 1900, "diocese")$parent_name == "Kalmar stift"))
  expect_true(all(assign_parent(k$geom_id, 1950, "diocese")$parent_name %in% c("Växjö stift", NA)))
})

test_that("courts do not overlap (Finnerödja, town courts) and stay within Sweden", {
  for (y in c(1700, 1800, 1850, 1900)) {
    x <- suppressWarnings(get_boundaries(y, "court_of_appeal"))
    tot <- sum(as.numeric(sf::st_area(x)))
    uni <- as.numeric(sf::st_area(sf::st_union(x)))
    expect_lt((tot - uni) / 1e6, 1, label = sprintf("court of appeal overlap %d (km2)", y))
    swe <- as.numeric(sf::st_area(sf::st_union(sweden)))
    expect_lt(uni / swe, 1.005, label = sprintf("court of appeal coverage %d", y))
  }
  mc <- suppressWarnings(get_boundaries(1850, "magistrates_court"))
  town <- mc[grepl("^(Hudiksvalls|Karlstads|Piteå) rådhusrätt", mc$name), ]
  rural <- mc[!grepl("rådhusrätt", mc$name), ]
  shared <- suppressWarnings(sf::st_intersection(sf::st_geometry(town), sf::st_union(sf::st_geometry(rural))))
  expect_lt(sum(as.numeric(sf::st_area(shared))) / 1e6, 1)
})

test_that("the national outline is the parish territory", {
  data(sweden, package = "swehist")
  expect_equal(nrow(sweden), 1L)
  p <- sf::st_union(sf::st_geometry(boundaries[boundaries$type_id == "parish", ]))
  # the union, with gaps under 10 km2 filled (15 gaps, about 10 km2 in all)
  extra <- as.numeric(sf::st_area(sweden)) - as.numeric(sf::st_area(p))
  expect_gte(extra, 0)
  expect_lt(extra, 50e6)
  expect_lt(sum(as.numeric(sf::st_area(sf::st_difference(p, sf::st_geometry(sweden))))), 1e3)
  # every type's features lie in it, except a few early Lapland units (a lake near Arjeplog)
  cov <- swehist:::.coverage
  expect_true(all(cov$share[cov$type_id == "parish" & cov$year >= 1650] > 0.995))
})

test_that("older parish versions carry the pid of their own county's namesake", {
  data(parish_link, package = "swehist")
  data(parish_registry, package = "swehist")
  pid_letter <- function(nm, start) {
    pid <- parish_link$pid[parish_link$name == nm & parish_link$start == start]
    parish_registry$letter[parish_registry$pid == pid]
  }
  expect_equal(pid_letter("Hede församling (Z-län)", 1600), "Z")
  expect_equal(pid_letter("Aspö församling (D-län)", 1600), "D")
  expect_equal(pid_letter("Ljungby församling (H-län)", 1600), "H")
  expect_equal(pid_letter("Lindesbergs landsförsamling", 1643), "T")
})
