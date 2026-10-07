# data-raw/sources — the build inputs that are not Riksarkivet's

The pipeline's main input is the Riksarkivet shapefile and its metadata tables
(`data-raw/data/`, not in the repo: see `data-raw/README.md`). The files here are the other
sources the build reads, kept in the repo so that a clone can run it. They are build inputs only:
`data-raw/` is in `.Rbuildignore`, so none of them is shipped in the package.

| file | what it is | source |
|---|---|---|
| `scb_codes.csv` | every SCB parish code 1952-1990 with the dates it was valid, its county and municipality digits, whether the parish was divided by a municipal boundary, and the code it had become by 1991 | Derived from SCB's registers of parish codes (`parishes-1970.csv`, `parish-2011.csv`, `parishes-2018.csv`) |
| `parish-changes-scb-1970-2018.csv` | SCB's change list: old and new code, date, type of change | SCB's register of parish changes |
| `muncipals_2011.csv` | municipality codes and names | SCB |

`scb_panel.csv` (the codes valid on 1 January of each year) is pure derivation from
`scb_codes.csv` and 5 MB, so it is not kept: `m3a_scb_municipalities.R` rebuilds it when the file
is absent.

## What the repo does not carry

**The DDB code catalogue** (`ddb_catalogue.csv`, from the Demographic Data Base's
`KOD.DEDIKKATALOG`) is not ours to redistribute. `m1b_codes.R` uses it to
date the `dedik` codes; without it the build says so and the dedik codes stay undated. Ask the
DDB (Umeå University) for the catalogue, or put a copy here.

**The SFGT source texts** (`data-raw/sfgt/batches/`) are not published either; the parsed tables
(`data-raw/sfgt/sfgt_*.rda`) are in the repo and are what the build reads. See
`data-raw/sfgt/README.md`.

**The statskalender page texts** are downloaded from runeberg.org on first use and cached under
`data-raw/model/out/cache/statskalender/`; the parsed tables are tracked
(`data-raw/model/data/statskalender_*.csv`).

## Carried over from 1.1.1

Two files hold material that the model does not derive and that came from the earlier build, frozen
here so that a rebuild never reads the previous release's `data/` directory:

| file | what is taken from it |
|---|---|
| `parish_registry_111.csv` | Skatteverket's `socken` and `alias` fields per pid, and the 779 non-territorial parishes (garrison, hospital, kbfd, ships' congregations), which have no territory and so no unit in the model |
| `unit_variants_curated.csv` | the curated name variants: abbreviations, county-code forms and manual additions (775 rows) |

Their own provenance is the old pipeline's `step7_parish_registry.R` and `step8_unit_variants.R`,
which read `swe-parish/data/for_hist.rda` (Skatteverket's *Sveriges församlingar genom tiderna*)
and the DDB parish names.
