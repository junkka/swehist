# swehist 1.1.1

First public release. swehist replaces the histmaps package
(<https://github.com/junkka/histmaps>, last version 0.3.1.9999), which it was
built from. The data were rebuilt from the Riksarkivet sources with a new,
reproducible pipeline (`data-raw/`), and most functions are new.

## Changes from histmaps

### Data

* Datasets are renamed and restructured: `geom_sp` is now `boundaries`,
  `geom_relations` is `relations` and `geom_meta` is `parish_meta`. Type ids
  are renamed (`municipal` -> `municipality`, `magistrate` ->
  `magistrates_court`, `court` -> `court_of_appeal`, `distict_court` ->
  `district_court`). geom_ids are new: re-match stored histmaps ids by name
  or code with `match_units()`.
* New datasets: `hierarchy` (which unit belonged to which, by year, in the
  civil, fiscal, judicial, ecclesiastical and military branches),
  `parish_link` and `parish_registry` (parish codes and names from
  Riksarkivet, SCB, Skatteverket and the DDB), `unit_variants` (name variants
  for matching) and `sweden` (the national outline).
* `geom_borders`, `map_desc`, `eu_geom` and `eu_border` are gone.
* Coordinates are now truly SWEREF99 TM (EPSG:3006). The source is RT90 2.5
  gon V and is transformed with the datum shift; histmaps labelled the RT90
  coordinates as EPSG:3006, about 200 m off.
* Duplicate records of one unit are merged (parishes 3,575 -> 3,080),
  overlapping units of one type are cut apart, and units that the source has
  wrong are rebuilt from their children: dioceses from their deaneries
  (Kalmar stift, 1603-1914, is added) and hundreds from their parishes.
  Courts of appeal are the six historical courts.
* `relations` records successors (an overlap of at least 10% of the smaller
  unit) and transfers of territory between units.
* The data are built on the parishes: a parish's polygon is the atom, and
  every other type is the union of the parishes that belonged to it in each
  period, from dated links in the registers, SCB's change lists, the
  församlingar volumes (SFGT) and *Sveriges statskalender*. The levels
  therefore line up with each other by construction, and a unit's boundary
  changes when its membership changes rather than being one modern polygon
  applied to its whole lifetime.
* Names, codes and memberships carry their own dates, so a unit is named and
  coded as it was in the year you ask for (Kyrkefalla, not Tibro, in 1900),
  and `boundaries` gains `unit_id`, the identity that runs through renames,
  recodings and boundary changes.
* `boundaries$kind` distinguishes the institutions that Riksarkivet's types
  hold together: rural courts (*domsagor*) from town courts
  (*rådhusrätter*) and *tingsrätter*, *härader* from *tingslag* and the towns
  outside them, and *städer*, *köpingar* and *landskommuner* from the
  municipalities of 1971.
* 80 detached parts of parishes (633 km2) that the source dropped and the
  surrounding parish absorbed are restored from Lantmäteriet's boundaries.
* New datasets: `quality_table`, which says for each type and 50-year period
  how much of the country it covers and what kind of evidence its membership
  rests on; `known_issues`, the boundary problems that research established
  but no source lets us correct; and `events`, which dates every creation,
  abolition, renaming, recoding, merger, split, transfer of part and change
  of parent.
* `parish_codes` holds every dated code of every parish version (DDB dedik,
  SCB forkod, NAD nadkod). A parish has often had several: the DDB catalogue
  gives Byske 82983 and 82990, and its codes for chapels, bönehus,
  kyrkobokföringsdistrikt and lappförsamlingar point to the parish they lie in,
  as do SCB's codes for a parish's kyrkobokföringsdistrikt (Spånga kbfd 018041).

### Functions

* New: `get_children()`, `assign_parent()`, `unit_history()`,
  `match_units()` (unit names and codes, with historical spellings and
  fuzzy matching), `get_borders()`, `crosswalk()` (areal weights between two
  divisions, for moving data from the parishes of 1880 to the municipalities
  of 1990), `locate_point()` and `neighbours()`.
* `match_units()` looks a parish code up among every code the parish has had
  (`parish_codes`); keeps the county across the steps of its name cascade,
  flagging a hit outside the county with `multiple = TRUE` instead of
  returning it silently; returns a town's parishes for a town total (forkod
  or nadkod with parish part 00, `match_type = "via_municipality"`); and, for
  a code used before its parish existed, returns the parish that held the
  territory at the date, flagged. A forkod with parish part 00 is a placeholder
  that sources fill with different parishes (a town's total in the codes of the
  1970s, its rural parish in the 1930 census), so a match on one is always
  flagged. These came from testing the package with the identifiers of POPLINK,
  SwedPop, NAPP, the death book, the 1930 census and other datasets.
* `assign_parent()` reports the evidence behind each parent in a `grade`
  column and prefers the best-founded one, so a parent taken from a dated
  source outranks one inferred from geometry.
* `get_boundaries()` takes any of the 11 types and warns when a map covers
  less than 90% of Sweden. `hist_boundaries()`, `parish_boundaries()`,
  `county_boundaries()`, `st_as_data_frame()` (use
  `sf::st_drop_geometry()`) and `create_block()` are removed.
* Period maps (`get_boundaries(c(1800, 1900), ...)`) merge units connected by
  successor relations inside the range, keep `name`, `start`, `end` and the
  merged `geom_ids`, and return a lookup from `geom_id` to the merged unit.

### Build

* The build pipeline (`data-raw/`) runs in one order and fails when a data
  invariant breaks. Two clean builds give identical data, and geom_ids stay
  the same across rebuilds (`data-raw/geom_id_lookup.csv`).
* Validation against independent sources is summarised in `VALIDATION.md`.
