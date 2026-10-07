## Export the datasets for a release (GeoPackage + CSV), for users outside R.
## The zip is attached to the GitHub release and archived by Zenodo.
## Run from the package root:  Rscript data-raw/export_release.R [out_dir]

suppressMessages({library(sf); library(dplyr)})
version <- read.dcf("DESCRIPTION", fields = "Version")[1, 1]
args <- commandArgs(TRUE)
out_dir <- if (length(args)) args[1] else tempdir()
name <- paste0("swehist-", version, "-data")
dir <- file.path(out_dir, name)
unlink(dir, recursive = TRUE)
dir.create(dir, recursive = TRUE)

for (f in list.files("data", pattern = "\\.rda$", full.names = TRUE)) load(f)

# Spatial layers in one GeoPackage (EPSG:3006)
gpkg <- file.path(dir, "swehist.gpkg")
st_write(boundaries, gpkg, layer = "boundaries", quiet = TRUE)
st_write(sweden, gpkg, layer = "sweden", quiet = TRUE, append = FALSE)
st_write(hist_town, gpkg, layer = "hist_town", quiet = TRUE, append = FALSE)

# Tables as UTF-8 CSV; boundaries also without geometry, for joins
tables <- list(boundaries = st_drop_geometry(boundaries), relations = relations,
               hierarchy = hierarchy, parish_link = parish_link,
               parish_registry = parish_registry, parish_meta = parish_meta,
               parish_codes = parish_codes,
               unit_variants = unit_variants,
               # The three tables the model added. They were missing from the export, so a user
               # outside R got the boundaries without the evidence grade of each level, the
               # deviations that are known and not corrected, or the dated events.
               quality_table = quality_table, known_issues = known_issues, events = events)
for (t in names(tables)) {
  readr::write_csv(tables[[t]], file.path(dir, paste0(t, ".csv")), na = "")
}

writeLines(c(
  paste0("swehist ", version, ": Swedish historical administrative boundaries 1600-1990"),
  "",
  "swehist.gpkg     boundaries, sweden (national outline), hist_town (county towns);",
  "                 SWEREF99 TM (EPSG:3006)",
  "boundaries.csv   boundaries without geometry (join to other tables by geom_id)",
  "relations.csv    successors and transfers between units",
  "hierarchy.csv    which unit belonged to which, by year",
  "parish_link.csv, parish_registry.csv, parish_meta.csv   parish codes and names",
  "parish_codes.csv    every dated code (DDB, SCB, NAD) of every parish version",
  "unit_variants.csv   name variants used for matching",
  "quality_table.csv   coverage and evidence grade, by type and 50-year period",
  "known_issues.csv    researched deviations that no source lets us correct",
  "events.csv          dated creations, abolitions, renamings, mergers and splits",
  "",
  "The columns are described in the R package's help pages (?boundaries, ?hierarchy, ...)",
  "and at https://github.com/junkka/swehist.",
  "",
  "License: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/).",
  "Cite: Junkka, J. (2026). swehist: Swedish Historical Administrative Boundaries.",
  paste0("R package version ", version, ". https://github.com/junkka/swehist")
), file.path(dir, "README.txt"))

zip_file <- file.path(out_dir, paste0(name, ".zip"))
unlink(zip_file)
old <- setwd(out_dir)
utils::zip(basename(zip_file), name, flags = "-rq9X")
setwd(old)
message("Wrote ", zip_file, sprintf(" (%.1f MB)", file.size(zip_file) / 1e6))
