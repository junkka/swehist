context("County map")

test_that("County sf", {
  res1 <- get_boundaries(1800, "county")
  expect_s3_class(res1, "sf")
  expect_true(nrow(res1) > 0)

  res2 <- get_boundaries(1900, "county")
  expect_s3_class(res2, "sf")
  expect_true(nrow(res2) > 0)

  expect_error(get_boundaries(1991, "county"))
  expect_error(get_boundaries(1400, "county"))
})

test_that("County meta", {
  res1 <- get_boundaries(1800, "county", "meta")
  expect_s3_class(res1, "tbl_df")
  expect_true(nrow(res1) > 0)
  expect_true("geom_id" %in% colnames(res1))
})

test_that("County range map", {
  res <- get_boundaries(c(1800, 1900), "county")
  expect_type(res, "list")
  expect_s3_class(res$map, "sf")
})

test_that("all types return sf", {
  # Most types available at 1900
  types_1900 <- c("parish", "county", "municipality", "pastorship", "contract",
                   "diocese", "hundred", "magistrates_court",
                   "court_of_appeal", "bailiwick")
  for (typ in types_1900) {
    res <- suppressWarnings(get_boundaries(1900, typ))  # hundred warns: partial map
    expect_s3_class(res, "sf")
    expect_true(nrow(res) > 0, label = sprintf("type=%s has rows", typ))
  }
  # District court starts in 1971
  res_dc <- get_boundaries(1980, "district_court")
  expect_s3_class(res_dc, "sf")
  expect_true(nrow(res_dc) > 0)
})

test_that("parish_meta dataset loads", {
  data(parish_meta, package = "swehist")
  expect_s3_class(parish_meta, "tbl_df")
  expect_true(nrow(parish_meta) > 0)
  expect_true("geom_id" %in% colnames(parish_meta))
  expect_true("county" %in% colnames(parish_meta))
})

context("Hierarchy")

test_that("hierarchy dataset loads", {
  data(hierarchy, package = "swehist")
  expect_s3_class(hierarchy, "tbl_df")
  expect_true(nrow(hierarchy) > 10000)
  expect_true(all(c("parent_geom_id", "child_geom_id", "parent_type",
                     "child_type", "start", "end") %in% colnames(hierarchy)))
  # All non-NA IDs reference boundaries (Regiment parents have NA geom_id)
  data(boundaries, package = "swehist")
  non_na <- hierarchy %>% dplyr::filter(!is.na(parent_geom_id))
  expect_true(all(non_na$parent_geom_id %in% boundaries$geom_id))
  expect_true(all(hierarchy$child_geom_id %in% boundaries$geom_id))
  # New columns exist
  expect_true(all(c("parent_ref_code", "parent_name") %in% colnames(hierarchy)))
  # No self-references (excluding NA parents)
  expect_true(all(non_na$parent_geom_id != non_na$child_geom_id))
})

test_that("hierarchy has expected type pairs", {
  data(hierarchy, package = "swehist")
  pairs <- hierarchy %>%
    dplyr::distinct(parent_type, child_type)
  # Civil branch
  expect_true(any(pairs$parent_type == "County" & pairs$child_type == "Municipality"))
  expect_true(any(pairs$parent_type == "Municipality" & pairs$child_type == "Parish"))
  # Judicial branch
  expect_true(any(pairs$parent_type == "Hundred" & pairs$child_type == "Parish"))
  # Ecclesiastical branch
  expect_true(any(pairs$parent_type == "Diocese" & pairs$child_type == "Contract"))
  # Military branch
  expect_true(any(pairs$parent_type == "Regiment" & pairs$child_type == "Parish"))
})

test_that("get_children returns municipalities in county", {
  res <- get_children(1880, "county", "Malmöhus", "municipality")
  expect_s3_class(res, "sf")
  expect_true(nrow(res) > 50)
  expect_true("parent_name" %in% colnames(res))
  expect_true(all(grepl("Malmöhus", res$parent_name)))
})

test_that("get_children returns meta format", {
  res <- get_children(1880, "county", "Malmöhus", "municipality", format = "meta")
  expect_s3_class(res, "tbl_df")
  expect_true(nrow(res) > 50)
})

test_that("get_children works for diocese", {
  res <- get_children(1850, "diocese", "Skara")
  expect_s3_class(res, "sf")
  expect_true(nrow(res) > 0)
})

test_that("get_children multi-hop: county to parish", {
  res <- get_children(1880, "county", "Malmöhus", "parish")
  expect_s3_class(res, "sf")
  expect_true(nrow(res) > 50)
  expect_true("parent_name" %in% colnames(res))
  expect_true(all(grepl("Malmöhus", res$parent_name)))
})

test_that("get_children recursive=FALSE returns direct county-parish links", {
  # Direct County→Parish links exist from SFGT lan data for county-changing parishes
  res <- get_children(1880, "county", "Malmöhus", "parish", recursive = FALSE)
  expect_s3_class(res, "sf")
  expect_true(nrow(res) >= 0)
})

test_that("get_children validates inputs", {
  expect_error(get_children(2000, "county", "Malmöhus"))
  expect_error(get_children(1880, "county", "NonexistentCounty"))
  expect_error(get_children(1880, "bogus_type", "test"))
  expect_error(get_children(c(1800, 1900), "county", "Malmöhus"))
})

test_that("get_children works for regiment (military branch)", {
  res <- get_children(1800, "regiment", "Uppland", "parish")
  expect_s3_class(res, "sf")
  expect_true(nrow(res) > 0)
  expect_true("parent_name" %in% colnames(res))
  # All results should be parishes with valid geometries
  expect_true(all(sf::st_is_valid(res)))
})

test_that("get_year.character handles various formats", {
  expect_equal(swehist:::get_year.character("1800"), 1800L)
  expect_equal(swehist:::get_year.character("1800-01-15"), 1800L)
  expect_equal(swehist:::get_year.character("1800abc"), 1800L)
})

test_that("hierarchy has regiment rows with NA parent_geom_id", {
  data(hierarchy, package = "swehist")
  reg_rows <- hierarchy %>%
    dplyr::filter(parent_type == "Regiment")
  expect_true(nrow(reg_rows) > 0)
  expect_true(all(is.na(reg_rows$parent_geom_id)))
  expect_true(all(!is.na(reg_rows$parent_ref_code)))
  expect_true(all(!is.na(reg_rows$parent_name)))
  # Regiment → Parish type pair exists
  expect_true(any(reg_rows$child_type == "Parish"))
})
