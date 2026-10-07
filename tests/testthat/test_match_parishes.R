context("match_parishes")

# --- Return structure ---

test_that("return tibble has correct columns", {
  res <- match_parishes("Luleå")
  expect_s3_class(res, "tbl_df")
  expected_cols <- c("input", "pid", "geom_id", "name", "match_type",
                     "match_column", "distance", "county_match", "multiple")
  expect_true(all(expected_cols %in% colnames(res)))
})

test_that("empty input returns 0-row tibble", {
  res <- match_parishes(character(0))
  expect_equal(nrow(res), 0)
  expect_s3_class(res, "tbl_df")
})

test_that("NA input returns NA result", {
  res <- match_parishes(NA_character_)
  expect_equal(nrow(res), 1)
  expect_true(is.na(res$pid))
  expect_true(is.na(res$match_type))
})

test_that("no match returns NA", {
  res <- match_parishes("NonexistentParish12345")
  expect_equal(nrow(res), 1)
  expect_true(is.na(res$pid))
  expect_true(is.na(res$geom_id))
  expect_true(is.na(res$match_type))
  expect_equal(res$input, "NonexistentParish12345")
})

# --- Exact name matching ---

test_that("exact name match works", {
  res <- match_parishes("Luleå")
  expect_equal(res$match_type, "exact_name")
  expect_false(is.na(res$pid))
  expect_false(is.na(res$geom_id))
  expect_equal(res$input, "Luleå")
})

test_that("case-insensitive name match", {
  res1 <- match_parishes("luleå")
  res2 <- match_parishes("LULEÅ")
  expect_equal(res1$pid, res2$pid)
  expect_equal(res1$match_type, "exact_name")
  expect_equal(res2$match_type, "exact_name")
})

test_that("multiple names vectorized", {
  res <- match_parishes(c("Lövånger", "Burträsk", "Skellefteå"))
  expect_equal(nrow(res), 3)
  expect_true(all(!is.na(res$pid)))
  expect_true(all(res$match_type == "exact_name"))
  # All distinct pids
  expect_equal(length(unique(res$pid)), 3)
})

# --- Alias matching ---

test_that("alias matching works", {
  # "Kiruna" is an alias for Jukkasjärvi
  res <- match_parishes("Kiruna")
  expect_false(is.na(res$pid))
  res2 <- match_parishes("Jukkasjärvi")
  expect_equal(res$pid, res2$pid)
  expect_equal(res$match_column, "alias")
})

# --- Normalized name matching ---

test_that("normalized match strips församling", {
  res <- match_parishes("Luleå församling")
  expect_false(is.na(res$pid))
  # the registry carries the full name now, so this is an exact match; before the rebuild the
  # stem had to be normalised away
  expect_true(res$match_type %in% c("exact_name", "normalized"))
  res2 <- match_parishes("Luleå")
  expect_equal(res$pid, res2$pid)
})

test_that("normalized match strips stadsförsamling", {
  # Find a parish with stads in its name
  data(parish_registry, package = "swehist")
  stads <- parish_registry[grep("stadsförsamling", parish_registry$name_full,
                                 ignore.case = TRUE), ]
  if (nrow(stads) > 0) {
    base_name <- sub("\\s*stadsförsamling$", "", stads$name_full[1],
                     ignore.case = TRUE)
    res <- match_parishes(paste0(base_name, " stads"))
    expect_false(is.na(res$pid))
  }
})

test_that("normalized match handles trailing parenthetical", {
  # name_ddb often has "(X)" suffixes like "KARESUANDO (ENONTEKI)"
  data(parish_registry, package = "swehist")
  paren <- parish_registry[grep("\\(", parish_registry$name_ddb), ]
  if (nrow(paren) > 0) {
    # Try matching with the parenthetical
    res <- match_parishes(paren$name_ddb[1])
    expect_false(is.na(res$pid))
  }
})

# --- Fuzzy matching ---

test_that("fuzzy match works for misspellings", {
  res <- match_parishes("Jukkasjärv", fuzzy = TRUE)
  expect_equal(res$match_type, "fuzzy")
  expect_false(is.na(res$pid))
  expect_false(is.na(res$distance))

  # Verify it matches the correct parish
  res2 <- match_parishes("Jukkasjärvi")
  expect_equal(res$pid, res2$pid)
})

test_that("fuzzy match not triggered when disabled", {
  res <- match_parishes("Jukkasjärv", fuzzy = FALSE)
  expect_true(is.na(res$pid))
})

# --- Code matching ---

test_that("code match by pid", {
  data(parish_registry, package = "swehist")
  known <- parish_registry[1, ]

  res <- match_parishes(known$pid, by = "pid")
  expect_equal(res$match_type, "exact_code")
  expect_equal(res$match_column, "pid")
  expect_equal(res$pid, known$pid)
  expect_equal(res$name, known$name)
})

test_that("code match by forkod", {
  data(parish_registry, package = "swehist")
  known <- parish_registry[!is.na(parish_registry$forkod), ][1, ]

  res <- match_parishes(known$forkod, by = "forkod")
  expect_equal(res$match_type, "exact_code")
  expect_equal(res$match_column, "forkod")
  expect_equal(res$pid, known$pid)
})

test_that("code match by nadkod", {
  data(parish_registry, package = "swehist")
  known <- parish_registry[!is.na(parish_registry$nadkod), ][1, ]

  res <- match_parishes(known$nadkod, by = "nadkod")
  expect_equal(res$match_type, "exact_code")
  expect_equal(res$pid, known$pid)
})

test_that("code match by dedik", {
  data(parish_registry, package = "swehist")
  known <- parish_registry[!is.na(parish_registry$dedik), ][1, ]

  res <- match_parishes(known$dedik, by = "dedik")
  expect_equal(res$match_type, "exact_code")
  expect_equal(res$pid, known$pid)
})

test_that("auto-detect forkod by 6-digit count", {
  data(parish_registry, package = "swehist")
  known <- parish_registry[!is.na(parish_registry$forkod), ][1, ]

  res <- match_parishes(known$forkod)
  expect_equal(res$match_type, "exact_code")
  expect_equal(res$match_column, "forkod")
})

test_that("auto-detect pid by <=4-digit count", {
  data(parish_registry, package = "swehist")
  known <- parish_registry[parish_registry$pid < 10000, ][1, ]

  res <- match_parishes(known$pid)
  expect_equal(res$match_type, "exact_code")
  expect_equal(res$match_column, "pid")
  expect_equal(res$pid, known$pid)
})

test_that("auto-detect picks the code column with most hits", {
  data(parish_registry, package = "swehist")
  # 5-digit forkods (counties 01-09) are forkod, not dedik
  fk5 <- parish_registry$forkod[!is.na(parish_registry$forkod) &
                                parish_registry$forkod < 1e5][1:20]
  res <- match_parishes(fk5)
  expect_true(all(res$match_column == "forkod"))
  expect_equal(res$pid, parish_registry$pid[match(fk5, parish_registry$forkod)])

  # A vector of dedik codes is detected as dedik
  dk <- unique(parish_registry$dedik[!is.na(parish_registry$dedik)])[1:30]
  res <- suppressWarnings(match_parishes(dk))
  expect_true(all(res$match_column == "dedik"))

  # Mixed 5- and 6-digit forkods are one column
  res <- match_parishes(c(68011, 248201))
  expect_true(all(res$match_column == "forkod"))
  expect_false(anyNA(res$pid))
})

test_that("unmatched code returns NA", {
  res <- match_parishes(999999, by = "forkod")
  expect_true(is.na(res$pid))
})

# --- County constraint ---

test_that("county by letter disambiguates", {
  # "husby" appears in counties AB, W, C, D, E — use "W" (county 20)
  res_all <- match_parishes("husby")
  res_w <- match_parishes("husby", county = "W")

  expect_false(is.na(res_w$pid))
  expect_true(res_w$county_match)
})

test_that("county by name works", {
  # Lövånger is in Västerbottens (AC, county 24)
  res <- match_parishes("Lövånger", county = "Västerbottens")
  expect_false(is.na(res$pid))
  expect_true(res$county_match)
})

test_that("county by numeric code works", {
  res <- match_parishes("Lövånger", county = 24)
  expect_false(is.na(res$pid))
})

test_that("county mismatch flagged but match kept", {
  # Match a known parish, use wrong county
  res <- match_parishes("Jukkasjärvi", county = 1)
  expect_false(is.na(res$pid))
  expect_false(res$county_match)
})

test_that("county constraint with multiple inputs", {
  # Both Lövånger and Burträsk are in Västerbottens (AC, county 24)
  res <- match_parishes(c("Lövånger", "Burträsk"), county = "AC")
  expect_equal(nrow(res), 2)
  expect_true(all(res$county_match))
})

# --- Date resolution ---

test_that("date resolves temporal geom_id", {
  res_no_date <- match_parishes("Jukkasjärvi")
  res_date <- match_parishes("Jukkasjärvi", date = 1900)
  expect_false(is.na(res_no_date$geom_id))
  expect_false(is.na(res_date$geom_id))
  # The pid should be the same
  expect_equal(res_no_date$pid, res_date$pid)
})

test_that("date as string works", {
  res <- match_parishes("Luleå", date = "1900-01-01")
  expect_false(is.na(res$geom_id))
})

# --- Multiple matches ---

test_that("multiple flag set for ambiguous names", {
  # "Husby" exists in multiple counties
  res <- match_parishes("Husby")
  expect_true(res$multiple)
})

test_that("unique name has multiple=FALSE", {
  res <- match_parishes("Jukkasjärvi")
  expect_false(res$multiple)
})

# --- Edge cases ---

test_that("by='name' forces name matching", {
  res <- match_parishes("Luleå", by = "name")
  expect_equal(res$match_type, "exact_name")
  expect_false(is.na(res$pid))
})

test_that("SCB uppercase name matches", {
  data(parish_registry, package = "swehist")
  known <- parish_registry[!is.na(parish_registry$name_scb), ][1, ]

  res <- match_parishes(known$name_scb)
  expect_false(is.na(res$pid))
  expect_equal(res$match_type, "exact_name")
})

test_that("name_previous matches", {
  data(parish_registry, package = "swehist")
  prev <- parish_registry[!is.na(parish_registry$name_previous) &
                           nchar(parish_registry$name_previous) > 0, ]
  if (nrow(prev) > 0) {
    # Use first word of first name_previous entry
    prev_name <- trimws(strsplit(prev$name_previous[1], ",")[[1]][1])
    res <- match_parishes(prev_name)
    # Should match (possibly via exact or normalized)
    expect_false(is.na(res$pid))
  }
})

# --- Fallback matching ---

test_that("fallback fills in unmatched codes with name match", {
  data(parish_registry, package = "swehist")
  known <- parish_registry[!is.na(parish_registry$forkod), ][1:2, ]

  # First code valid, second invalid — fallback should rescue second
  codes <- c(known$forkod[1], 999999)
  names <- c("should_not_be_used", known$name[2])

  res <- match_parishes(codes, fallback = names)
  expect_equal(nrow(res), 2)
  # First matched by code
  expect_equal(res$match_type[1], "exact_code")
  expect_equal(res$pid[1], known$pid[1])
  # Second matched by fallback name
  expect_false(is.na(res$pid[2]))
  expect_equal(res$pid[2], known$pid[2])
  expect_equal(res$match_type[2], "exact_name")
  # Input should be the original code, not the fallback name
  expect_equal(res$input[2], "999999")
})

test_that("fallback not used when primary matches", {
  data(parish_registry, package = "swehist")
  known <- parish_registry[!is.na(parish_registry$forkod), ][1, ]

  res <- match_parishes(known$forkod, fallback = "NonexistentParish")
  expect_equal(res$match_type, "exact_code")
  expect_equal(res$pid, known$pid)
})

test_that("fallback with county constraint", {
  # Invalid code with name fallback + county
  res <- match_parishes(999999, fallback = "Husby", county = "W")
  expect_false(is.na(res$pid))
  expect_true(res$county_match)
})

test_that("fallback with fuzzy matching", {
  res <- match_parishes(999999, fallback = "Jukkasjärv", fuzzy = TRUE)
  expect_false(is.na(res$pid))
  expect_equal(res$match_type, "fuzzy")
})

test_that("fallback length mismatch errors", {
  expect_error(
    match_parishes(c(1, 2, 3), fallback = c("a", "b")),
    "same length"
  )
})

test_that("fallback works for name-to-name matching", {
  # Name primary fails, fallback name succeeds
  res <- match_parishes("NonexistentParish", fallback = "Luleå")
  expect_false(is.na(res$pid))
  expect_equal(res$input, "NonexistentParish")
})
