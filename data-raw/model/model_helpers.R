## Helpers for the redesign build (data-raw/model/)

dir.create("data-raw/model/out", showWarnings = FALSE, recursive = TRUE)

TYPE_PREFIX <- c(parish = "P", county = "L", municipality = "K", pastorship = "PA",
                 contract = "KO", diocese = "S", hundred = "H", magistrates_court = "D",
                 district_court = "T", court_of_appeal = "HR", bailiwick = "F", regiment = "R")

year_start <- function(y) as.Date(sprintf("%04d-01-01", as.integer(y)))
year_end   <- function(y) as.Date(sprintf("%04d-12-31", as.integer(y)))

# The file carries a comment block describing its kinds, so `comment = "#"` is required
# (base read.csv() needs comment.char = "#"). Dates may be a year or a full date.
# corrections.csv plus any corrections_<topic>.csv beside it, so separate topics (and separate
# workers) never write the same file.
read_corrections <- function(f = "data-raw/model/corrections.csv"){
  files <- c(f, list.files(dirname(f), "^corrections_.*\\.csv$", full.names = TRUE))
  files <- files[file.exists(files)]
  x <- dplyr::bind_rows(lapply(files, function(fi)
    readr::read_csv(fi, comment = "#", na = c(""), col_types = readr::cols(.default = "c"))))
  yr <- function(v) as.integer(substr(v, 1, 4))
  x$start <- yr(x$start); x$end <- yr(x$end)
  x
}

# Polygon part of each feature, as MULTIPOLYGON. A GEOMETRYCOLLECTION (polygons plus stray
# lines from st_make_valid) keeps the union of its polygons. the earlier pipeline's step 3 extracted the
# polygons of all collections at once, which returns one element per polygon, and assigned
# the first n back: Kalmar landsförsamling and Norra Möre domsaga lost their polygons.
polygons_only <- function(g){
  gc <- which(as.character(st_geometry_type(g)) == "GEOMETRYCOLLECTION")
  for (i in gc) {
    p <- st_collection_extract(g[i], "POLYGON")
    g[i] <- st_union(p)
  }
  st_cast(st_cast(g, "MULTIPOLYGON"), "MULTIPOLYGON")
}

# Merge records of one topo_id that follow each other or overlap in time and have the same
# polygon (symmetric difference < tol_m2). Returns one record per run, period min-max.
merge_identical_versions <- function(rec, tol_m2 = 1000){
  m <- st_drop_geometry(rec) %>% mutate(i = row_number())
  multi <- m %>% group_by(topo_id) %>% filter(n() > 1) %>% arrange(start, .by_group = TRUE) %>%
    mutate(nxt = lead(i), nxt_start = lead(start)) %>% ungroup() %>%
    filter(!is.na(nxt), nxt_start <= end + 1L)
  g <- st_geometry(rec)
  d <- mapply(function(a, b){
    x <- tryCatch(st_area(st_sym_difference(g[a], g[b])), error = function(e) Inf)
    if (length(x)) as.numeric(sum(x)) else 0
  }, multi$i, multi$nxt)
  pairs <- multi[d < tol_m2, ]
  grp <- create_block(c(m$i, pairs$i), c(m$i, pairs$nxt))[seq_len(nrow(m))]
  rec$grp <- grp
  keep <- !duplicated(grp)
  out <- rec[keep, ]
  per <- st_drop_geometry(rec) %>% group_by(grp) %>%
    summarise(start = min(start), end = max(end), .groups = "drop")
  out$start <- per$start[match(out$grp, per$grp)]
  out$end   <- per$end[match(out$grp, per$grp)]
  out %>% select(-grp)
}

# IoU of record pairs (row indices a, b of an sf); 0 when they don't intersect
## `a` and `b` are rec_id VALUES, not row positions. m1 assigns rec_id with seq_len() and then the
## `period` corrections delete rows, so from the first deleted row on, id and position diverge by
## one or two. Indexing positionally there compared the wrong polygons: 158 of ~233 renames were
## missed and 3 units were fused that share no territory, which silently changes unit identity,
## geom_ids, hierarchy and events. Resolving the ids here means a caller cannot get it wrong.
pair_iou <- function(x, a, b){
  if (!is.null(x$rec_id)) {
    a <- match(a, x$rec_id); b <- match(b, x$rec_id)
    if (anyNA(a) || anyNA(b)) stop("pair_iou: rec_id not found in x", call. = FALSE)
  }
  g <- st_geometry(x); ar <- as.numeric(st_area(g))
  vapply(seq_along(a), function(k){
    i <- a[k]; j <- b[k]
    if (!lengths(st_intersects(g[i], g[j]))) return(0)
    inter <- tryCatch(as.numeric(sum(st_area(st_intersection(g[i], g[j])))), error = function(e) 0)
    inter / (ar[i] + ar[j] - inter)
  }, numeric(1))
}

# Simple name key for joining source names to SFGT/SCB names: lower case, drop parentheses,
# type words and a genitive s, fold diacritics. Not for matching across counties — namesakes
# must be separated by county or period by the caller.
name_key_simple <- function(x){
  x <- tolower(trimws(x))
  x <- gsub("\\([^)]*\\)", " ", x)
  x <- gsub("s:t ", "sankt ", x, fixed = TRUE)
  x <- gsub("\\b(församling|forsamling|pastorat|kontrakt|socken|sn)\\b", " ", x)
  x <- gsub("[^a-zåäöéü ]", " ", x)
  x <- gsub("\\s+", " ", trimws(x))
  sub("s$", "", x)
}

## source_file(): where a build input lives. `data-raw/sources/` holds the inputs the repo carries
## itself (SCB's code lists and change list, the municipality list), so a clone can run the build;
## the other paths are where the file was originally prepared, kept as a fallback so an existing
## working copy still builds. A missing file returns NA and the caller says what it means.
source_file <- function(name, alt = character()){
  cand <- c(file.path("data-raw/sources", name), alt,
            file.path("data-raw/validation/external/cache/scb", name),
            file.path("data-raw/validation/external/cache/ddb", name),
            file.path(Sys.getenv("SWEHIST_RPROJ", "/home/rstudiojunkka/rproj"),
                      "Samuel/reports/data", name),
            file.path(Sys.getenv("SWEHIST_RPROJ", "/home/rstudiojunkka/rproj"),
                      "Samuel/data-raw", name))
  hit <- cand[file.exists(cand)]
  if (!length(hit)) NA_character_ else hit[1]
}

## clean_unit_name(): repairs in the source's own names that nothing else fixes. The register has
## "Enångers kommune", "Ore kommun kommun", "Åmåls fögderi fögderi" and a few double spaces; they
## reach maps, labels and the matching index unless they are corrected where names are used. Only
## whitespace, a repeated word and the one misspelt type word are touched: nothing that could
## change which unit a name denotes.
clean_unit_name <- function(x){
  y <- gsub("[[:space:]]+", " ", trimws(x))
  y <- gsub("(*UCP)\\bkommune\\b", "kommun", y, perl = TRUE)
  y <- gsub("(*UCP)(?i)\\b(\\p{L}+) \\1\\b", "\\1", y, perl = TRUE)   # "kommun kommun"
  y
}

## fold_spelling(): the historical spellings of a Swedish place name, folded to one form. The same
## rules as normalize_historical_spelling() in R/match_parishes.R, kept here because the build must
## not depend on the package: a printed source of 1866 writes Elfkarleby, Qvidinge, Hvetlanda and
## Hjelmseryd where the register writes Älvkarleby, Kvidinge, Vetlanda and Hjälmseryd. Input is
## lower case; the caller folds the diacritics afterwards if it wants them folded.
fold_spelling <- function(x){
  x <- gsub("fv", "v", x, fixed = TRUE)
  x <- gsub("qv", "kv", x, fixed = TRUE)
  x <- gsub("\\bhv", "v", x, perl = TRUE)
  x <- gsub("\\bth", "t", x, perl = TRUE)
  x <- gsub("sch", "sk", x, fixed = TRUE)
  x <- gsub("dh", "d", x, fixed = TRUE)
  x <- gsub("dt\\b", "t", x, perl = TRUE)
  x <- gsub("gh\\b", "g", x, perl = TRUE)
  x <- gsub("w", "v", x, fixed = TRUE)
  x <- gsub("q", "k", x, fixed = TRUE)
  x <- gsub("\\bvest(er|ra)", "v\u00e4st\\1", x, perl = TRUE)
  x <- gsub("\\bvestre\\b", "v\u00e4stra", x, perl = TRUE)
  x <- gsub("ij", "i", x, fixed = TRUE)
  x <- gsub("([\u00e5\u00e4\u00f6aeiouy])ck([\u00e5\u00e4\u00f6aeiouy]|\\b)", "\\1k\\2", x, perl = TRUE)
  x <- gsub("([a-z\u00e5\u00e4\u00f6])f([\u00e5\u00e4\u00f6aeiouy])", "\\1v\\2", x, perl = TRUE)
  x <- gsub("\\bcarl", "karl", x, perl = TRUE)
  x <- gsub("\\bchrist", "krist", x, perl = TRUE)
  x <- gsub("\\baf(?!v)", "av", x, perl = TRUE)
  x <- gsub("ki\u00f6", "k\u00f6", x, fixed = TRUE)
  x <- gsub("\\bjern", "j\u00e4rn", x, perl = TRUE)
  x <- gsub("\\bhjelm", "hj\u00e4lm", x, perl = TRUE)
  x <- gsub("\\bstjern", "stj\u00e4rn", x, perl = TRUE)
  x <- gsub("\\belf", "\u00e4lv", x, perl = TRUE)
  x <- gsub("\\belm", "\u00e4lm", x, perl = TRUE)
  x <- gsub("helsing", "h\u00e4lsing", x, fixed = TRUE)   # Helsingland -> Hälsingland
  x <- gsub("\\bjemt", "j\u00e4mt", x, perl = TRUE)          # Jemtland -> Jämtland
  x <- gsub("\\bverml", "v\u00e4rml", x, perl = TRUE)        # Vermland -> Värmland
  x
}

## fold_name_key(): fold_spelling plus the diacritics and a final genitive s, for comparing a
## printed name with a register name ("Hjelmseryd" -> hjalmseryd; "Hjälmseryds församling" -> the
## same). Used by m3d.
fold_name_key <- function(x){
  y <- fold_spelling(tolower(trimws(x)))
  y <- chartr("\u00e5\u00e4\u00f6\u00e9\u00fc", "aaoeu", y)
  sub("s$", "", trimws(gsub("\\s+", " ", y)))
}

## fold_unit_key(): fold_name_key, then the words sorted and each one's genitive s dropped, because
## the printed name and the register's differ in order as well as in form: "Södra Vadsbo" is printed
## where the register has "Vadsbo södra kontrakt", "Helsinglands Norra" where it has "Norra
## Hälsinglands kontrakt". Sorting makes the two meet; a collision it creates shows up as an
## ambiguous match and is dropped rather than guessed.
fold_unit_key <- function(x){
  y <- fold_name_key(x)
  vapply(strsplit(y, " ", fixed = TRUE), function(w){
    w <- sub("s$", "", w[nzchar(w)])
    paste(sort(unique(w)), collapse = " ")
  }, character(1))
}
