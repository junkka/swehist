# Cases from the user-path tests: real identifiers from
# POPLINK, Myrdal, Folkräkningen 1930 and the death book that an earlier build answered wrongly or
# not at all.

one <- function(x, ...) suppressWarnings(match_units(x, ...))

test_that("a parish has every DDB code it has had (A)", {
  r <- one(c("82983", "82990"), by = "dedik", date = 1900)
  expect_true(all(grepl("^Byske", r$geom_name)))
  r <- one(c("54130", "85590", "85600", "70320", "64720"), by = "dedik", date = 1900)
  expect_match(r$geom_name[1], "Linköpings domkyrkoförsamling", ignore.case = TRUE)
  expect_match(r$geom_name[2], "^Katarina")
  expect_match(r$geom_name[3], "^Maria Magdalena")
  expect_match(r$geom_name[4], "^Lunds domkyrkoförsamling")
  expect_match(r$geom_name[5], "^Västerås domkyrkoförsamling")
  r <- one(c("56820", "56822"), by = "dedik", date = 1880)
  expect_true(all(grepl("^Vikingstad", r$geom_name)))
})

test_that("a lappförsamling's DDB code reaches the parish it lies in (B)", {
  r <- one(c("83944", "83952"), by = "dedik", date = 1850)
  expect_match(r$geom_name[1], "^Föllinge")
  expect_match(r$geom_name[2], "^Hotagen")
})

test_that("the rural parish of a town keeps its code of the old scheme (C)", {
  r <- one(c("088400", "158500", "108300"), by = "forkod", date = 1930)
  expect_equal(r$geom_name, c("Vimmerby landsförsamling", "Åmåls landsförsamling",
                              "Sölvesborgs landsförsamling"))
  # a part-00 code is a placeholder the sources fill differently: always flagged
  expect_true(all(r$multiple))
})

test_that("a code is not left on two registry entries (D)", {
  r <- one(c("84410", "59744", "74200"), by = "dedik", date = 1900)
  expect_false(anyNA(r$geom_id))
})

test_that("the county holds across the steps of the name cascade (E)", {
  r <- one(c("Östra Ryds församling", "Vänge församling"), by = "name", county = c(5, 3),
           date = 1800)
  expect_match(r$geom_name[1], "E-län")
  expect_match(r$geom_name[2], "C-län")
})

test_that("a congregation's parent lies in its own county (F)", {
  r <- one(c("Bromma kbfd", "Ekeby norra kbfd", "Hotagens lappförs"), by = "name",
           county = c(1, 5, 23), date = c(1950, 1930, 1900))
  expect_match(r$geom_name[1], "AB-län")
  expect_match(r$geom_name[2], "E-län")
  expect_match(r$geom_name[3], "^Hotagen")
})

test_that("a code used before its parish existed gives the parish of that date (G)", {
  r <- one("82986", by = "dedik", date = 1761)
  expect_true(r$active_at_date)
  expect_true(r$multiple)
})

test_that("a town's name or a part-00 forkod gets a parish only with a flag (H)", {
  r <- one("Stockholms stad", by = "name", county = 1, date = 1900)
  expect_true(r$multiple)
  # a forkod with parish part 00 is a placeholder the sources fill differently (048600 is
  # Strängnäs landsförsamling in the registry, Kärnbo in the 1930 census): never unflagged
  r <- one(c("048600", "128000", "018000"), by = "forkod", date = 1930)
  expect_true(all(r$multiple[!is.na(r$geom_id)]))
  expect_equal(r$match_type[3], "via_municipality")
  expect_match(r$geom_name[3], "^Stockholms domkyrkoförsamling")
  expect_equal(r$input, c("048600", "128000", "018000"))
})

test_that("parish_codes holds dated codes of every system", {
  data(parish_codes, package = "swehist", envir = environment())
  expect_true(all(c("geom_id", "pid", "system", "code", "start", "end", "precision", "source") %in%
                    names(parish_codes)))
  expect_setequal(unique(parish_codes$system), c("dedik", "forkod", "nadkod"))
  data(parish_link, package = "swehist", envir = environment())
  expect_true(all(parish_codes$geom_id %in% parish_link$geom_id))
  expect_true(all(parish_codes$start <= parish_codes$end))
})
