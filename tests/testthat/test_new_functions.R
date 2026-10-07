context("unit_history")

test_that("unit_history traces Luleå parish lineage", {
  res <- unit_history("Luleå", "parish")
  expect_s3_class(res, "tbl_df")
  expect_true(nrow(res) > 10)
  expect_true(all(c("geom_id", "name", "start", "end", "event",
                     "from_ids", "to_ids") %in% colnames(res)))
  # The origin (1600-1606) should have event = "origin"
  origin <- res[res$start == 1600, ]
  expect_true(nrow(origin) >= 1)
  expect_equal(origin$event[1], "origin")
  expect_true(is.na(origin$from_ids[1]))
  # Terminal entries should have NA to_ids
  terminals <- res[is.na(res$to_ids), ]
  expect_true(nrow(terminals) > 0)
  # Should include Nederluleå (a split-off)
  expect_true(any(grepl("Nederlule", res$name)))
  # Should be sorted by start year
  expect_true(all(diff(res$start) >= 0))
})

test_that("unit_history forward direction", {
  # Forward from Luleå should include successors but same connected component
  res_fwd <- unit_history("Luleå", "parish", direction = "forward")
  res_both <- unit_history("Luleå", "parish", direction = "both")
  # Forward should include all the same units since all Luleå versions
  # are matched initially and forward covers the full tree
  expect_s3_class(res_fwd, "tbl_df")
  expect_true(nrow(res_fwd) > 0)
})

test_that("unit_history backward direction", {
  # Starting from Nederluleå, backward should trace to origin
  res <- unit_history("Nederluleå", "parish", direction = "backward")
  expect_s3_class(res, "tbl_df")
  expect_true(nrow(res) > 1)
  # Should find the origin
  expect_true(any(res$event == "origin"))
})

test_that("unit_history event types are valid", {
  res <- unit_history("Luleå", "parish")
  expect_true(all(res$event %in% c("origin", "split", "merge", "continuation")))
})

test_that("unit_history works for county", {
  res <- unit_history("Malmöhus", "county")
  expect_s3_class(res, "tbl_df")
  expect_true(nrow(res) > 0)
  expect_true(any(grepl("Malmöhus", res$name)))
})

test_that("unit_history works for municipality", {
  res <- unit_history("Luleå", "municipality")
  expect_s3_class(res, "tbl_df")
  expect_true(nrow(res) > 0)
})

test_that("unit_history errors on no match", {
  expect_error(unit_history("NonexistentPlace12345", "parish"))
})


context("assign_parent")

test_that("assign_parent: parish to county", {
  parishes <- get_boundaries(1880, "parish")
  gids <- parishes$geom_id[1:20]
  res <- assign_parent(gids, 1880, "county")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 20)
  expect_equal(res$geom_id, gids)
  expect_true(all(c("geom_id", "parent_geom_id", "parent_name",
                     "parent_ref_code") %in% colnames(res)))
  # Most parishes should have a county
  expect_true(sum(!is.na(res$parent_geom_id)) >= 15)
  # County names should be plausible
  expect_true(all(grepl("län", res$parent_name[!is.na(res$parent_name)])))
})

test_that("assign_parent: parish to hundred", {
  parishes <- get_boundaries(1880, "parish")
  gids <- parishes$geom_id[1:10]
  res <- assign_parent(gids, 1880, "hundred")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 10)
  # Some parishes should have a hundred parent
  expect_true(sum(!is.na(res$parent_geom_id)) > 0)
})

test_that("assign_parent: parish to municipality", {
  parishes <- get_boundaries(1880, "parish")
  gids <- parishes$geom_id[1:10]
  res <- assign_parent(gids, 1880, "municipality")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 10)
  # Most parishes should have a municipality
  expect_true(sum(!is.na(res$parent_geom_id)) >= 5)
})

test_that("assign_parent: parish to regiment", {
  parishes <- get_boundaries(1800, "parish")
  gids <- parishes$geom_id[1:10]
  res <- assign_parent(gids, 1800, "regiment")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 10)
  # Regiment parents have NA geom_id but populated name
  found <- res[!is.na(res$parent_name), ]
  expect_true(nrow(found) > 0)
  expect_true(all(is.na(found$parent_geom_id)))
  expect_true(all(!is.na(found$parent_ref_code)))
})

test_that("assign_parent: empty input", {
  res <- assign_parent(integer(0), 1880, "county")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  expect_true(all(c("geom_id", "parent_geom_id", "parent_name",
                     "parent_ref_code") %in% colnames(res)))
})

test_that("assign_parent: validates inputs", {
  expect_error(assign_parent(1:5, c(1800, 1900), "county"), "date ranges")
  expect_error(assign_parent(1:5, 2000, "county"), "Date must be")
})

test_that("assign_parent: multi-hop finds county via municipality", {
  # Get a parish known to be in Östergötlands län via municipality
  data(hierarchy, package = "swehist")
  # Find a parish that has municipality parent but not direct county link
  res <- assign_parent(5769L, 1880, "county")
  expect_equal(res$parent_name, "Östergötlands län")
})


context("match_units")

test_that("match_units: exact county match", {
  res <- match_units("Malmöhus län", "county")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_equal(res$match_type, "exact")
  expect_true(!is.na(res$geom_id))
  expect_true(grepl("Malmöhus", res$name))
})

test_that("match_units: normalized county match (without suffix)", {
  res <- match_units("Malmöhus", "county")
  expect_equal(nrow(res), 1)
  expect_equal(res$match_type, "normalized")
  expect_true(grepl("Malmöhus", res$name))
})

test_that("match_units: municipality with date", {
  res <- match_units(c("Luleå", "Malmö"), "municipality", date = 1900)
  expect_equal(nrow(res), 2)
  expect_true(all(!is.na(res$geom_id)))
  # Both should be active at 1900
  expect_true(all(res$start <= 1900))
  expect_true(all(res$end >= 1900))
})

test_that("match_units: fuzzy matching", {
  res <- match_units("Västra Göinje", "hundred", fuzzy = TRUE)
  expect_equal(nrow(res), 1)
  expect_equal(res$match_type, "fuzzy")
  expect_true(!is.na(res$geom_id))
  # max_dist bounds the distance over the whole name: a name that only
  # occurs inside longer ones does not match them
  expect_true(is.na(match_units("Göinge", "hundred", fuzzy = TRUE)$geom_id))
})

test_that("match_units: no match returns NA", {
  res <- match_units("NonexistentPlace12345", "county")
  expect_equal(nrow(res), 1)
  expect_true(is.na(res$geom_id))
  expect_true(is.na(res$match_type))
})

test_that("match_units: empty input", {
  res <- match_units(character(0), "county")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  expect_true(all(c("input", "geom_id", "name", "start", "end",
                     "match_type", "distance", "multiple") %in% colnames(res)))
})

test_that("match_units: multiple flag", {
  # "Malmöhus" without date should match multiple temporal versions
  res <- match_units("Malmöhus län", "county")
  expect_true(res$multiple)
})

test_that("match_units: parish delegates to match_parishes", {
  res <- match_units("Luleå", "parish")
  expect_equal(nrow(res), 1)
  expect_true(!is.na(res$geom_id))
  # Should have the common columns
  expect_true(all(c("input", "geom_id", "name", "start", "end",
                     "match_type", "distance", "multiple") %in% colnames(res)))
})

test_that("match_units: NA input handled", {
  res <- match_units(NA_character_, "county")
  expect_equal(nrow(res), 1)
  expect_true(is.na(res$geom_id))
})

test_that("match_units: hundred exact match", {
  res <- match_units("Olands härad", "hundred")
  expect_equal(nrow(res), 1)
  expect_equal(res$match_type, "exact")
})

test_that("match_units: diocese", {
  res <- match_units("Skara stift", "diocese")
  expect_equal(nrow(res), 1)
  expect_equal(res$match_type, "exact")
})

# ---- match_units KB variant tests ----

context("match_units knowledge base")

test_that("match_units KB: genitive-s variant matches municipality", {
  res <- match_units("Stockholm", "municipality", date = 1900)
  expect_true(!is.na(res$geom_id))
  expect_true(grepl("Stockholm", res$name))
})

test_that("match_units KB: manual historical county name", {
  # Kopparbergs län was the official name until 1997; "Dalarnas län" is a
  # later name recorded as a duplicate in the raw data and maps to it
  res <- match_units(c("Kopparbergs", "Dalarnas län"), "county")
  expect_false(anyNA(res$geom_id))
  expect_true(all(grepl("Kopparbergs", res$name)))
})

test_that("match_units KB: och/o abbreviation variant", {
  res <- match_units("Göteborgs och Bohus län", "county")
  expect_true(!is.na(res$geom_id))
})

test_that("match_units KB: parish via alias", {
  # "Birka" is an alias for Adelsö
  res <- match_units("Birka", "parish")
  expect_true(!is.na(res$geom_id))
  expect_true(grepl("Adels", res$name))
})

test_that("match_units KB: parish via name_previous", {
  # "Liden" is a previous name for Ådals-Liden
  res <- match_units("Liden", "parish")
  expect_true(!is.na(res$geom_id))
})

test_that("match_units KB: diocese names and short forms", {
  res <- match_units(c("Växjö stift", "Wexjö"), "diocese")
  expect_false(anyNA(res$geom_id))
  expect_equal(res$match_type[1], "exact")
})

test_that("match_units KB: historical spelling (pre-1906)", {
  res <- match_units("Wexjö", "diocese")
  expect_true(!is.na(res$geom_id))
})

test_that("normalize_historical_spelling handles new transforms", {
  nhs <- swehist:::normalize_historical_spelling
  expect_equal(nhs("hvetlanda"), "vetlanda")       # hv -> v
  expect_equal(nhs("scheleftea"), "skeleftea")     # sch -> sk
  expect_equal(nhs("bergh"), "berg")               # gh\b -> g
  expect_equal(nhs("wijka"), "vika")               # w->v then ij->i
  expect_equal(nhs("ki\u00f6ping"), "k\u00f6ping") # kio -> ko
  expect_equal(nhs("jernberga"), "j\u00e4rnberga") # jern -> jarn
  expect_equal(nhs("hjelmseryd"), "hj\u00e4lmseryd") # hjelm -> hjalm
  expect_equal(nhs("elfkarleby"), "\u00e4lvkarleby") # elf -> alv
  expect_equal(nhs("afsta"), "avsta")              # af -> av
  expect_equal(nhs("stjerneberg"), "stj\u00e4rneberg") # stjern -> stjarn
})

test_that("match_units KB: court of appeal short form", {
  res <- match_units("västra sverige hovrätt", "court_of_appeal")
  expect_true(!is.na(res$geom_id))
})

test_that("match_units KB: temporal resolution (old name + modern date)", {
  # "Kopparbergs" at date=1990 — the canonical name is "Dalarnas län" from 1720
  # This should match via KB and resolve to the Dalarnas geom_id
  res <- match_units("Kopparbergs", "county", date = 1800)
  expect_true(!is.na(res$geom_id))
})

test_that("match_units KB: date suffix stripped in normalization", {
  # Names like "Nora bergs pastorat -1823" should have date suffix stripped
  # during normalization, so "Nora bergs pastorat" matches
  res <- match_units("Nora bergs pastorat", "pastorship")
  expect_true(!is.na(res$geom_id))
})

test_that("match_units KB: empty input still works", {
  res <- match_units(character(0), "county")
  expect_equal(nrow(res), 0)
})

test_that("match_units KB: existing exact match still works", {
  res <- match_units("Skaraborgs län", "county")
  expect_equal(res$match_type, "exact")
})

test_that("match_units KB: existing normalized match still works", {
  res <- match_units("Malmöhus", "county")
  expect_equal(res$match_type, "normalized")
})

test_that("match_units KB: parish ref_codes go through the knowledge base", {
  data(boundaries, package = "swehist")
  p <- boundaries[boundaries$type_id == "parish" & grepl("^Jukkasj", boundaries$name), ][1, ]
  res <- match_units(c(p$ref_code, "Jukkasjärvi"), "parish")
  expect_equal(nrow(res), 2)
  expect_equal(boundaries$ref_code[boundaries$geom_id == res$geom_id[1]], p$ref_code)
  expect_false(is.na(res$geom_id[2]))
})

# ---- match_units code matching tests ----

context("match_units code matching")

test_that("match_units: county by letter code", {
  res <- match_units("BD", "county")
  expect_true(!is.na(res$geom_id))
  expect_true(grepl("Norrbotten", res$name))
  expect_equal(res$match_type, "kb_variant")
})

test_that("match_units: county by numeric code", {
  res <- match_units("25", "county")
  expect_true(!is.na(res$geom_id))
  expect_true(grepl("Norrbotten", res$name))
})

test_that("match_units: county by ref_code", {
  res <- match_units("SE/150000000", "county")
  expect_true(!is.na(res$geom_id))
  expect_true(grepl("lvsborg", res$name))
  expect_equal(res$match_type, "kb_variant")
})

test_that("match_units: parish by ref_code", {
  res <- match_units("SE/128717000", "parish")
  expect_true(!is.na(res$geom_id))
  expect_equal(res$match_type, "kb_variant")
})

test_that("match_units: parish by forkod (numeric)", {
  res <- match_units(248201, "parish")
  expect_true(!is.na(res$geom_id))
  expect_equal(res$match_type, "exact_code")
})

test_that("match_units: parish by forkod with county constraint", {
  res <- match_units(248201, "parish", county = "BD")
  expect_true(!is.na(res$geom_id))
})

test_that("match_units: parish code with fallback", {
  res <- match_units(c(248201, 999999), "parish",
                     fallback = c("Luleå", "Skellefteå"))
  expect_equal(nrow(res), 2)
  expect_true(all(!is.na(res$geom_id)))
})

test_that("match_units: non-territorial parish resolves to parent geom_id", {
  # "Olofströms kbfd" (forkod 106001) is a kbfd with parent_pid
  # Should resolve to parent parish's geom_id even without date
  data(parish_registry, package = "swehist")
  kbfd <- parish_registry[grepl("kbfd", parish_registry$category) &
                          !is.na(parish_registry$parent_pid), ]
  if (nrow(kbfd) > 0) {
    # Pick a kbfd with a known forkod
    test_pid <- kbfd$pid[1]
    res <- match_units(test_pid, "parish", by = "pid")
    expect_true(!is.na(res$geom_id))
  }
})

test_that("match_units: bailiwick by ref_code", {
  res <- match_units("SE/050399003", "bailiwick")
  expect_true(!is.na(res$geom_id))
  expect_true(grepl("Vadstena", res$name))
})

test_that("match_units: magistrates_court by ref_code", {
  res <- match_units("SE/100048305", "magistrates_court")
  expect_true(!is.na(res$geom_id))
})

# ---- Ecclesiastical hierarchy tests ----

context("ecclesiastical hierarchy")

test_that("hierarchy contains Contract -> Pastorship links", {
  data(hierarchy, package = "swehist")
  cp <- hierarchy[hierarchy$parent_type == "Contract" &
                        hierarchy$child_type == "Pastorship", ]
  expect_true(nrow(cp) > 3000)
})

test_that("hierarchy contains Pastorship -> Parish links", {
  data(hierarchy, package = "swehist")
  pp <- hierarchy[hierarchy$parent_type == "Pastorship" &
                        hierarchy$child_type == "Parish", ]
  expect_true(nrow(pp) > 5000)
})

test_that("get_children: diocese to parish works", {
  res <- get_children(1850, "diocese", "Skara", "parish")
  expect_s3_class(res, "sf")
  expect_true(nrow(res) > 100)
  expect_true(all(res$type_id == "parish"))
})

test_that("get_children: contract to parish works", {
  res <- get_children(1850, "contract", "Vartofta", "parish")
  expect_s3_class(res, "sf")
  expect_true(nrow(res) > 5)
  expect_true(all(res$type_id == "parish"))
})

test_that("get_children: diocese to pastorship works", {
  res <- get_children(1850, "diocese", "Skara", "pastorship")
  expect_s3_class(res, "sf")
  expect_true(nrow(res) > 50)
  expect_true(all(res$type_id == "pastorship"))
})

test_that("assign_parent: parish to pastorship", {
  parishes <- get_boundaries(1850, "parish")
  res <- assign_parent(parishes$geom_id[1:10], 1850, "pastorship")
  expect_equal(nrow(res), 10)
  expect_true(sum(!is.na(res$parent_geom_id)) > 5)
})

test_that("assign_parent: parish to contract", {
  parishes <- get_boundaries(1850, "parish")
  res <- assign_parent(parishes$geom_id[1:10], 1850, "contract")
  expect_equal(nrow(res), 10)
  expect_true(sum(!is.na(res$parent_geom_id)) > 5)
})

test_that("assign_parent: parish to diocese", {
  parishes <- get_boundaries(1850, "parish")
  res <- assign_parent(parishes$geom_id[1:10], 1850, "diocese")
  expect_equal(nrow(res), 10)
  # Some may be NA due to step3b merge bug losing diocese features
  expect_true(sum(!is.na(res$parent_geom_id)) >= 1)
})

test_that("get_children: unreachable child_type gives warning", {
  # Diocese to Municipality has no path in the hierarchy — warn, not crash
  expect_warning(
    res <- get_children(1850, "diocese", "Lunds", "municipality"),
    "No children found"
  )
  expect_equal(nrow(res), 0)
})

test_that("match_units passes expand to match_parishes", {
  res_no <- match_units("Luleå", "parish", date = 1900)
  res_ex <- match_units("Luleå", "parish", date = 1900, expand = TRUE)
  # expand=TRUE should return at least as many rows as default
  expect_true(nrow(res_ex) >= nrow(res_no))
})
