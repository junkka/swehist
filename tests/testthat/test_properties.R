# Property tests for fixed behaviour (borders, period maps, dates, matching)

context("borders and period maps")

test_that("get_borders keeps internal borders", {
  counties <- get_boundaries(1900, "county")
  b <- get_borders(1900, "county")
  outline <- sum(sf::st_length(sf::st_boundary(sf::st_union(counties))))
  expect_equal(nrow(b), 1)
  expect_true(as.numeric(sf::st_length(b)) > 1.3 * as.numeric(outline))
})

test_that("period maps do not merge units changed after the range ends", {
  # Hjo stads- and landsförsamling merged in 1989
  m <- get_boundaries(c(1987, 1988), "parish")
  hjo <- m$lookup[m$lookup$geom_id %in%
                    get_boundaries(1988, "parish")$geom_id[grepl("^Hjo", get_boundaries(1988, "parish")$name)], ]
  expect_equal(length(unique(hjo$geomid)), nrow(hjo))

  # The same municipalities exist in 1969 and 1970, so extending the range
  # from 1969 to 1970 adds no transitions and must not merge more units
  a <- get_boundaries(c(1950, 1969), "municipality")
  b <- get_boundaries(c(1950, 1970), "municipality")
  expect_equal(nrow(a$map), nrow(b$map))
  # ...while extending into the 1971 reform does merge
  c71 <- get_boundaries(c(1950, 1971), "municipality")
  expect_true(nrow(c71$map) <= nrow(b$map))
})

test_that("period map lookup only contains ids active in the range", {
  m <- get_boundaries(c(1850, 1862), "parish")
  data(boundaries, package = "swehist")
  b <- boundaries[boundaries$geom_id %in% m$lookup$geom_id, ]
  expect_true(all(b$start <= 1862 & b$end >= 1850))
})

test_that("period maps are uniform MULTIPOLYGON and empty ranges work", {
  m <- get_boundaries(c(1880, 1890), "county")$map
  expect_true(all(sf::st_geometry_type(m) == "MULTIPOLYGON"))
  e <- suppressWarnings(get_boundaries(c(1634, 1640), "municipality"))
  expect_equal(nrow(e$map), 0)
})

context("dates and names")

test_that("get_year reads the common date formats", {
  expect_equal(get_year(1866L), 1866L)
  expect_equal(get_year(19000101), 1900L)
  expect_equal(get_year(19000000), 1900L)
  expect_equal(get_year("  1866 "), 1866L)
  expect_equal(get_year("1866-06-06"), 1866L)
  expect_equal(get_year("06/06/1866"), 1866L)
  expect_equal(get_year(as.Date("1866-06-06")), 1866L)
  expect_error(get_boundaries(NA, "parish"), "could not read a year")
  expect_error(get_boundaries(1:3, "parish"), "range of two")
})

test_that("names that are not valid regexes are matched as text", {
  h <- unit_history("(I-l", "parish")
  expect_true(all(grepl("(I-l", h$name, fixed = TRUE) | !is.na(h$from_ids) | !is.na(h$to_ids)))
  h <- unit_history("Luleå domkyrkoförsamling", "parish", exact = TRUE)
  expect_true(nrow(h) >= 1)
})

test_that("create_block keeps NA rows apart", {
  expect_equal(create_block(c(1, NA, 3, NA), c(2, 5, NA, 6)), 1:4)
  expect_equal(create_block(c(1, 2, 3), c(2, 3, 9)), c(1, 1, 1))
})

context("matching fixes")

test_that("5-digit forkods and codes given as text match", {
  res <- match_units(c(68011, 58123, 88006))
  expect_false(anyNA(res$geom_id))
  expect_true(all(res$match_type == "exact_code"))
  res2 <- match_units(c("068011", "248201"))
  expect_false(anyNA(res2$geom_id))
})

test_that("pre-1906 spellings match parishes", {
  res <- match_units(c("Hjelmseryd", "Elfkarleby", "Qvidinge", "Hvetlanda"))
  expect_false(anyNA(res$geom_id))
  expect_equal(res$name, c("Hjälmseryd", "Älvkarleby", "Kvidinge", "Vetlanda"))
  # Elfkarleby is also a recorded previous name, so it can match exactly
  expect_true(all(res$match_type %in% c("historical", "exact_name")))
})

test_that("date-resolved matches say whether the unit exists at the date", {
  # Bergsjön did not exist in 1800: the parish that held its territory then, flagged, not
  # Bergsjön itself drawn over its mother parish
  res <- match_units("Bergsjön", date = 1800)
  expect_true(res$active_at_date)
  expect_true(res$multiple)
  expect_false(grepl("^Bergsjön", res$geom_name))
  res <- match_units(c("Luleå", "Malmö"), "municipality", date = 1900)
  expect_true(all(res$active_at_date))
  expect_true(all(res$start <= 1900 & res$end >= 1900))
  expect_true(all(is.na(match_units("Luleå")$active_at_date)))
})

test_that("county constraint works for non-parish types", {
  res <- match_units("Bro", "municipality", county = "S", date = 1900)
  data(boundaries, package = "swehist")
  cty <- boundaries[boundaries$type_id == "county" & grepl("^Värmlands", boundaries$name) &
                    boundaries$start <= 1900 & boundaries$end >= 1900, ]
  u <- boundaries[boundaries$geom_id == res$geom_id, ]
  inside <- lengths(sf::st_intersects(suppressWarnings(sf::st_point_on_surface(u)), cty)) > 0
  expect_true(inside)
  expect_warning(match_units("Bro", "municipality", by = "name"), "only apply")
})

test_that("assign_parent flags multiple parents", {
  p <- get_boundaries(1900, "parish")$geom_id[1:20]
  res <- assign_parent(p, 1900, "county")
  expect_true("multiple" %in% names(res))
  expect_type(res$multiple, "logical")
})

test_that("fuzzy parish matches stay within max_dist", {
  res <- suppressWarnings(match_units(c("Trosa landsförs", "Umeå landsförs", "Säters stadsförs",
                                        "Skelefteå"), date = 1880, fuzzy = TRUE))
  expect_false(any(grepl("Skellefteå", res$geom_name[1:3])))
  expect_match(res$geom_name[2], "^Umeå lands")
  ok <- res$match_type == "fuzzy" & !is.na(res$match_type)
  expect_true(all(res$distance[ok] / nchar(res$input[ok]) <= 0.2))
})

test_that("Vester/Vestra are read as Väster/Västra", {
  res <- suppressWarnings(match_units(c("Vesterhaninge", "Vestra Tollstads"), date = 1880, fuzzy = TRUE))
  expect_equal(res$geom_name, c("Västerhaninge församling", "Västra Tollstads församling"))
})

test_that("codes of older parish versions match their own version", {
  res <- suppressWarnings(match_units(c(48023002, 128014002, 156606002), by = "nadkod", date = 1900))
  expect_equal(res$geom_name, c("Bogsta församling", "Glostorps församling", "Tarsleds församling"))
  expect_true(all(res$match_type == "exact_code"))
})

test_that("forkod fallback needs one parish", {
  # Motala stad: a town code, parish part 00. It used to fall back to forkod
  # 58300 and return Sten; it now goes through the town unit
  res <- suppressWarnings(match_units(58300002, by = "nadkod", date = 1900))
  expect_equal(res$match_type, "via_municipality")
  expect_equal(res$geom_name, "Motala församling")
})

test_that("town codes (parish part 00) match the town's parishes", {
  res <- suppressWarnings(match_units(c(78000002, 18000008), by = "nadkod", date = 1900))
  expect_equal(res$match_type, c("via_municipality", "via_municipality"))
  expect_equal(res$name, c("Växjö stad", "Stockholms stad"))
  expect_equal(res$geom_name[1], "Växjö stadsförsamling")
  expect_true(res$multiple[2])
  all_st <- suppressWarnings(match_units(18000008, by = "nadkod", date = 1900, expand = TRUE))
  kids <- get_children(1900, "municipality", "^Stockholms stad$", "parish", format = "meta")
  expect_setequal(all_st$geom_id, kids$geom_id)
  # bare codes are reference codes for other types
  m <- match_units(c(78000002, "SE/078000002"), "municipality", date = 1900)
  expect_equal(m$geom_name, c("Växjö stad", "Växjö stad"))
})

test_that("temporal resolution walks towards the date (Kiruna stad 1900)", {
  res <- match_units("Kiruna stad", "municipality", date = 1900)
  expect_equal(res$geom_name, "Jukkasjärvi kommun")
  expect_true(res$active_at_date)
})

test_that("parish dates resolve forwards to the version that keeps the name", {
  # Skellefteå's pid ends in 1837; the 1838 version carries another pid
  res <- match_units(c("Skellefteå", "Åby"), date = 1880)
  expect_equal(res$geom_name, c("Skellefteå församling", "Åby församling"))
  expect_equal(res$start, c(1838L, 1733L))
  expect_true(all(res$active_at_date))
})

test_that("a pid with several versions at the date gives the one named", {
  # Glostorp and Lockarp are both versions of Oxie's pid in 1900; the
  # result must not depend on the other inputs of the call
  one <- match_units("Glostorp", date = 1900, county = 12)
  two <- match_units(c("Lövånger", "Lockarp", "Glostorp"), date = 1900)
  expect_equal(one$geom_name, "Glostorps församling")
  expect_equal(two$geom_name[2:3], c("Lockarps församling", "Glostorps församling"))
  # The input decides before the registry name (both versions start with
  # "Askersund"); a code prefers the version named like its registry entry
  a <- match_units(c("Askersunds stadsförs", "Askersunds landsförs"), date = 1900)
  expect_equal(a$geom_name, c("Askersunds stadsförsamling", "Askersunds landsförsamling"))
  v <- match_units(c(58401, 158201), by = "forkod", date = 1930)
  expect_equal(v$geom_name, c("Vadstena församling", "Alingsås stadsförsamling"))
  # 48601 is Strangnas: SCB gives the code to one unit, dated 1967-1990, and the registry entry
  # carries it undated, so a 1930 lookup resolves to that entry's 1930 version. It is no longer
  # flagged `multiple` -- in the rebuilt data a code sits on one unit at any date (b2 checks it for
  # every code and date), where the earlier build had the code on two registry entries at once.
  s <- match_units(48601, by = "forkod", date = 1930)
  expect_true(s$active_at_date)
  expect_false(s$multiple)
  expect_equal(s$geom_name, "Strängnäs stadsförsamling")
  expect_true(s$start <= 1930 && s$end >= 1930)
})
