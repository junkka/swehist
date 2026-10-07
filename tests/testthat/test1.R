context("parish map")

test_that("get parish sf", {
  res <- get_boundaries(1800, "parish")
  expect_s3_class(res, "sf")
  expect_true(nrow(res) > 0)

  res2 <- get_boundaries(1900, "parish")
  expect_s3_class(res2, "sf")
  expect_true(nrow(res2) > 0)

  expect_error(get_boundaries(2000))
  expect_error(get_boundaries(1400))
})

test_that("get parish meta", {
  res <- get_boundaries(1800, "parish", "meta")
  expect_s3_class(res, "tbl_df")
  expect_true(nrow(res) > 0)
  expect_true("geom_id" %in% colnames(res))
})

test_that("get parish period", {
  expect_error(get_boundaries(c(1900, 1890)))

  res <- get_boundaries(c(1800, 1900))

  expect_type(res, "list")
  expect_s3_class(res$map, "sf")
  expect_s3_class(res$lookup, "data.frame")

  expect_true(nrow(res$map) == length(unique(res$lookup$geomid)))
  expect_true(all(res$map$geomid %in% unique(res$lookup$geomid)))
  expect_true(all(unique(res$lookup$geomid) %in% res$map$geomid))
  # Period map should include rich metadata
  expect_true(all(c("name", "start", "end", "geom_ids") %in% colnames(res$map)))
  expect_true(all(res$map$start <= 1900))
  expect_true(all(res$map$end >= 1800))

  res2 <- get_boundaries(c(1900, 1920))

  expect_type(res2, "list")
  expect_s3_class(res2$map, "sf")
  expect_s3_class(res2$lookup, "data.frame")

  expect_true(nrow(res2$map) == length(unique(res2$lookup$geomid)))
  expect_true(all(res2$map$geomid %in% unique(res2$lookup$geomid)))
  expect_true(all(unique(res2$lookup$geomid) %in% res2$map$geomid))
})

test_that("municipality sf", {
  res <- get_boundaries(1900, "municipality")
  expect_s3_class(res, "sf")
  expect_true(nrow(res) > 100)
})

test_that("municipality period map", {
  res <- get_boundaries(c(1900, 1950), "municipality")
  expect_type(res, "list")
  expect_s3_class(res$map, "sf")
  expect_s3_class(res$lookup, "data.frame")
  expect_true(nrow(res$map) > 0)
})

test_that("diocese sf", {
  res <- get_boundaries(1900, "diocese")
  expect_s3_class(res, "sf")
  expect_true(nrow(res) >= 5)
})

test_that("hundred sf", {
  # no partial-coverage warning any more: härader, tingslag and the towns outside them cover
  # 96.9% of the country from 1600 to 1970 (the earlier build had 33% and warned in every year)
  res <- suppressWarnings(get_boundaries(1800, "hundred"))
  expect_s3_class(res, "sf")
  expect_true(nrow(res) > 0)
})

# --- get_borders tests ---

test_that("get_borders with date and type", {
  res <- get_borders(1900, "county")
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1)
  geom_type <- as.character(sf::st_geometry_type(res))
  expect_true(geom_type %in% c("MULTILINESTRING", "LINESTRING", "GEOMETRYCOLLECTION"))
})

test_that("get_borders preserves CRS", {
  counties <- get_boundaries(1900, "county")
  res <- get_borders(data = counties)
  expect_equal(sf::st_crs(res), sf::st_crs(counties))
})

test_that("get_borders with data argument", {
  parishes <- get_boundaries(1900, "parish")
  res <- get_borders(data = parishes)
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1)
})

test_that("get_borders with get_children output", {
  children <- get_children(1880, "county", "Malmöhus", "parish")
  res <- get_borders(data = children)
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1)
})

test_that("get_borders handles period maps", {
  res <- get_borders(c(1800, 1900), "county")
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1)
})

test_that("get_borders input validation", {
  expect_error(get_borders(), "Provide either")
  expect_error(get_borders(date = 1900), "Provide either")
  expect_error(get_borders(data = data.frame(x = 1)), "'data' must be an sf")
})

# --- create_block tests ---

test_that("create_block groups connected components", {
  # Chain: 1-2, 2-3 => all one group
  res <- create_block(c(1, 2), c(2, 3))
  expect_equal(length(res), 2)
  expect_equal(res[1], res[2])
})

test_that("create_block separates disjoint components", {
  # Two disjoint edges: 1-2, 3-4

  res <- create_block(c(1, 3), c(2, 4))
  expect_equal(length(res), 2)
  expect_true(res[1] != res[2])
})

test_that("create_block handles self-edges", {
  res <- create_block(c(1, 2), c(1, 2))
  expect_equal(length(res), 2)
  expect_true(res[1] != res[2])  # 1-1 and 2-2 are separate
})

test_that("create_block handles transitive merging", {
  # 1-2, 3-4, 2-3 => all same group
  res <- create_block(c(1, 3, 2), c(2, 4, 3))
  expect_equal(length(res), 3)
  expect_equal(length(unique(res)), 1)
})
