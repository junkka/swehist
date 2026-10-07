# Building the data

> **A note on "1.1.1" in these scripts.** The released version is 1.1.1 and its data are in
> `data/`. Comments in the build scripts and checks sometimes write "1.1.1" where they mean the
> *earlier, unreleased* build that preceded the redesign on parish atoms — the one that took the
> register's polygons directly and whose steps 3 to 8 no longer exist. Where a comment describes
> something being fixed, dropped or rebuilt "in 1.1.1", it is describing that predecessor, not the
> data you have.


`build_data.R` rebuilds every dataset in `data/` from the sources. Run it from the package
root (about 70 minutes, of which 25 are the containment in m3):

```r
source("data-raw/build_data.R")          # then: Rscript data-raw/model/m7_install.R
```

Each step runs in its own R process, reads only the files the previous steps wrote
(`data-raw/intermediate/`, `data-raw/model/out/`), and checks its output; the build stops when a
check fails. `m7_install.R` is separate on purpose: a build can be checked with
`SWEHIST_ROOT=data-raw/model/out/pkg` before it replaces `data/`.

## The model

The data are built on the parishes. A parish's polygon for a period is an **atom**; every other
unit is the **union of the parishes that belonged to it** in each period where that set is
constant, from dated links in Riksarkivet's register, SCB's code lists, the församlingar volumes
(SFGT), *Sveriges statskalender* and researched corrections. So the levels line up with each
other by construction, and a unit's territory changes when its membership changes rather than
being one polygon applied to its whole lifetime. `DESIGN.md` in the review notes has the model and
the reasoning. The identity *rule* is that a unit continues through a rename, a recoding, a transfer
of part and an "uppgår i", and a new unit begins at a "bildar". How far it is implemented differs by
type, and the difference matters when reading the data: for most units identity is the register's own
`topo_id`, joined across a rename where one record ends and another of the same type and territory
begins the next year (IoU >= 0.95; 233 such joins), plus 7 `same_unit` / `separate_unit` corrections.
The wording of the source's own histories - "bildar", "uppgår i" - is read directly only for the SCB
municipalities of 1952-1990 (`m3a_scb_municipalities.R`).

Every claim about membership carries the evidence behind it, and the claims are resolved by
precedence, best evidence first: a correction, then a dated source (SCB, SFGT, the statskalender),
then a dated register link, then an undated one, then spatial containment. `quality_table` reports
what each type and period ended up resting on.

## Steps

| Step | Script | What it does |
|---|---|---|
| 1 | `step1_extract.R` | Read the Riksarkivet shapefile (RT90, EPSG:3021), repair invalid geometries |
| 2 | `step2_snap.R` | Round coordinates to 1 m |
| 3 | `model/m1_units.R` | Units and their dated names: what continues through a rename or a recoding; `drop_record`, `same_unit`, `separate_unit` and `period` corrections |
| 4 | `model/m2_atoms.R` | Atoms: one parish unit's territory for a period |
| 5 | `model/m1b_codes.R` | Dated codes (forkod, dedik, nadkod, pid, ref_code) and the names the code sources carry |
| 6 | `model/m3_evidence.R` | Claims: the register's links, spatial containment, the `membership` corrections. The containment is reused from the previous `out/m3.rds` when the atoms and records are unchanged |
| 7 | `model/m3s_sfgt.R` | SFGT claims (pastorship, county), keyed by pid |
| 8 | `model/m3a_scb_municipalities.R` | Municipalities 1952-1990 from SCB's code lists |
| 9 | `model/m3c_courts.R` | Courts from 15 editions of *Sveriges statskalender*: the courts, their composition and their court of appeal |
| 10 | `model/m3d_deaneries.R` | Kontrakt, pastorat and stift from the statskalender's kyrkor sections (1866, 1881, 1931), parsed into `model/data/statskalender_kyrkor_<year>.csv` (tracked, so a clone needs no downloads) |
| 11 | `model/m3b_resolve.R` | One parent per parish, type and year: precedence, interval painting, chains (parish -> hundred -> domsaga -> hovrätt) |
| 12 | `model/m4_derive.R` | `boundaries` (unions of atoms, the 80 exclaves restored, the Skåne correction, `kind`, `geometry_from`), `hierarchy`, `relations`, `sweden`, the coverage table, geom_ids |
| 13 | `model/m4b_matching.R` | `parish_link`, `parish_meta`, `parish_registry`, `unit_variants` |
| 14 | `model/m5_events.R` | `events`: created, abolished, renamed, recoded, merged, split, part transferred |
| 15 | `model/m6_quality.R` | `quality_table` and `known_issues` |

| | | |
|---|---|---|
| — | `model/m7_install.R` | Copy the datasets into `data/`, write `R/sysdata.rda` (run by hand) |

The corrections are data: `model/corrections.csv` and `model/corrections_<topic>.csv`, one row per
finding with its source and a note (1,310 rows); a file's header lists the full references its
rows cite in short form. `model/data/` holds the parsed statskalender tables, the exclave geometry
and the court name fixes.

`build_helpers.R` holds the functions the steps share.

## Inputs

In the repository:

- `geom_id_lookup.csv`: every geom_id ever issued. A unit keeps its id as long as it exists
  with the same period; retired ids are never reused.
- `pid_lookup.csv`: the frozen parish ids (pid), one per entry in SFGT.
- `sfgt/`: the SFGT parish histories parsed into dated tables (see `sfgt/README.md`).
- `data/county_meta.csv` (county codes, letters, names, county towns).
- `sources/`: the inputs that are not Riksarkivet's — SCB's parish code lists and change list,
  the municipality list, and the two pieces carried over from 1.1.1 (Skatteverket's socken and
  alias fields with the non-territorial parishes, and the curated name variants). See
  `sources/README.md`, which also says what is deliberately not in the repository (the DDB code
  catalogue) and what happens without it.

Not in the repository (in `data-raw/data/`):

- `histmaps_raw.tar.gz`: Riksarkivet's shapefile *Historiska GIS-kartor (information om
  territoriella indelningar i Sverige från 1500-talets slut till 1900-talets slut)*, CC0.
- `Tbl_topografi.csv`, `Tbl_topografi_rel.csv`: units and their links from Riksarkivet's
  topographic register (NAD).
- `parish_medta.csv`, `par_to_county.csv`: parish codes (SCB) and parish-county years.

`sfgt/prepare_batches.R` reads the SFGT parish texts (`for_hist.rda`, from a sibling
`swe-parish` project), which are not in this repository; the parsed tables in `sfgt/` are.

## Release export

`export_release.R` writes the datasets as a GeoPackage and CSV files and zips them, for
the GitHub release (archived by Zenodo).

## Validation

`VALIDATION.md` summarises how the data were checked.


## What each script does, in more detail

Notes that used to sit at the top of each script. They record why a step works the way it
does, which matters when reading a result that depends on it.

### `build_data.R` — build_data.R — Pipeline runner

About 70 minutes in all; the containment in m3 is 25 of them and is reused when the atoms and
the source records have not changed. The data are built on the parishes: a parish polygon is an
atom, and every other unit is the union of the parishes that belonged to it in a period
(DESIGN.md). The steps: 1. step1_extract.R — Extract the raw shapefile (labelled EPSG:3021),
make valid 2. step2_snap.R — Set coordinate precision (1 m) 3. m1_units.R — Units and their
dated names: what continues through a rename, a recoding or a boundary change 4. m2_atoms.R —
Atoms: one parish unit's territory for a period 5. m1b_codes.R — Dated codes (forkod, dedik,
nadkod, pid, ref_code) and the names the code sources give 6. m3_evidence.R — Claims about
membership: the register's links, spatial containment, and the corrections 7. m3s_sfgt.R — SFGT
claims (pastorship and county), keyed by pid 8. m3a_scb_municipalities.R — Municipalities
1952-1990 from SCB's code lists 9. m3c_courts.R — Courts from Sveriges statskalender (15
editions) 9b. m3d_deaneries.R — Kontrakt, pastorat and stift from the statskalender's kyrkor
sections (1866, 1881, 1931) 10. m3b_resolve.R — One parent per parish, type and year: precedence
by evidence, intervals painted, chains resolved 11. m4_derive.R — boundaries, hierarchy,
relations, sweden, the coverage table, the exclaves, geom_ids 12. m4b_matching.R — parish_link,
parish_meta, parish_registry, unit_variants 13. m5_events.R — events: created, abolished,
renamed, recoded, merged, split 14. m6_quality.R — quality_table and known_issues m7_install.R
then copies the datasets into data/ and writes R/sysdata.rda. It is deliberately not part of
this run: check a build with SWEHIST_ROOT=data-raw/model/out/pkg before
installing it. The earlier pipeline (step3 to step8) built the same datasets from the source
polygons of every type; it is in the history, up to commit e32834e.

### `model/data/make_exclave_pieces.R` — make_exclave_pieces.R — extract the 80 missing exclaves as geometry (one-off, 26 September 2026)

For 80 of them, 632 km2 in all, swehist gives the land to the surrounding parish instead, and
the areas show that the source dropped the detached part rather than that the boundary changed:
swehist's area plus the exclave equals the 1999 distrikt area to within 1-3 km2 in every case
checked, and where a census volume from a county that was not re-measured gives the owner's area
it already includes the exclave (Fryksände 522.13 and Torsåker (Y) 101.89 in 1910). Five of the
twenty largest are named in FR 1910's per-county Anmärkningar, all traceable to the 1882
committee report on irregular divisions; the rest are uncontested utskogar and fäbodskogar that
raised no fiscal question. The citations are in model/corrections_boundary.csv. A component is a set of 1990 parishes and distrikt joined by the changes of
1991-1999, so a component with one parish names the owner unambiguously (79 of the 80); the one
that does not (Dimbo-Ottravad) is resolved by distance and flagged.

### `model/m1_units.R` — m1: Units (identity) and their names

A unit is the thing that continues through renames, recodings and boundary adjustments
(DESIGN.md, "same unit"): - all records of one topo_id belong to one unit (the source's own
identity); - a rename: each topo_id has one name, so a renamed unit appears as a new topo_id of
the same type whose first record starts the year after another topo_id's last record ends, with
the same territory (IoU >= 0.95); - corrections: same_unit (two topo_ids recorded for one unit),
drop_record (anachronisms), separate_unit (never merged). Dates are years in the source; they
become full dates (start 1 Jan, end 31 Dec) with precision "year".

### `model/m1b_codes.R` — m1b: codes and dated names of parish units (and ref_code for every type)

Within one NAD group SE/CCMMPPnnn each source record has its own suffix, so these are already
the parish's own codes; the 6-digit prefix, however, is the code of the surviving parish
(FOLLOWUP B2), so it is used as a forkod only for the record with suffix 000. - forkod is dated,
from the SCB code lists 1952-1990, matched by name and county. - dedik is re-keyed from the DDB
catalogue by name and SCB74 (parish_medta's dedik is one value for a whole NAD group, i.e. the
survivor's). - pid: every parish unit gets its own frozen pid. A pid may serve several units
that follow each other in time (a rename), never two that coexist. Dates are full dates.
precision: "day" (the source gives a date), "year" (a year), "none" (no dated source; the code
is assumed to hold for the unit's lifetime).

### `model/m2_atoms.R` — m2: Atoms — parish territory periods

Every other type is built from atoms in m4, so the atoms are the only geometry the model
carries. Checked here: no empty or near-empty atom (the the earlier pipeline's step 3
GEOMETRYCOLLECTION bug left Kalmar landsförsamling at 0.0002 km2; m1's polygons_only() fixes
it), atoms of one period do not overlap by more than 1 km2, and a unit's atoms do not overlap in
time. Parish parts (a parish genuinely split between two parents) are made in m3, where the
memberships are known.

### `model/m3_evidence.R` — m3: Membership evidence — every claim about which parent a parish had, and when

Sources, and why: - register_direct: Tbl_topografi_rel parish links (bailiwick, municipality,
hundred, regiment). The bailiwick and municipality links are dated (0.1% open start), but 80% of
the hundred links have no start year and 89% no end year: the register asserts one hundred per
parish without saying when. That is why 1.1.1 (step4g) gave each parish a single hundred for its
whole life and pushed later transfers back to 1600 (Tannåker, Tjörnarp, Källeryd, Slädene).
Undated claims are marked dated = FALSE and must not be extended over a lifetime without
support. - register_chain: links between higher types (municipality/bailiwick -> county, hundred
-> magistrates court, magistrates court/district court -> court of appeal, municipality ->
district court, contract -> diocese). - sfgt: pastorship and county per parish, with dates
(partial = TRUE rows, which are transfers of part of a parish, are excluded here and handled as
parts in m3b). - containment: the share of a parish atom inside a source polygon of the type,
per coexisting period. This is the primary source for contract, because the register has no
contract -> parish link and taking it through the pastorship gave 89 parishes a contract they do
not overlap at all (review A3). Elsewhere it is a fallback.

### `model/m3a_scb_municipalities.R` — m3a: Municipalities 1952-1990 from SCB, and the county of every parish from the SCB codes

The SCB parish code carries the municipality in digits 1-4 and the county in digits 1-2, and a
parish is recoded whenever either changes, so the code intervals are a dated membership table.
All change dates in 1952-1990 are 1 January. What SCB can and cannot give (see report$notes): -
1967-1990: every parish has a code, so the municipality set is complete. The panel gives 464
municipalities in 1971, 278 in 1974, 279 in 1980 and 284 in 1990, which are the published
counts. - 1952-1966: the lists hold only codes still valid in 1967 or later, so 1,624-1,786 of
about 2,500 parishes and 476-510 of about 1,037 municipalities are visible. A municipality first
seen in those years was not necessarily formed then, so its start_date is marked "not_before".
Identity ("same unit", DESIGN.md): a municipality continues through a recoding or rename; a
merger into an existing municipality continues that municipality ("uppgår i"); a new formation
is a new unit ("bildar"). Implemented on the code intervals: - a municipality code that stays
active across a date is the same unit; - a code that appears when another code disappears is the
same unit when that predecessor gives it most of its parishes and gives it (nearly) all of its
own (a recoding); - otherwise the new code is a new unit and its predecessors end, merged into
it.

### `model/m3b_resolve.R` — m3b: Resolve the evidence into memberships

Direct parents the sources name for a parish: municipality, bailiwick, hundred, pastorship,
contract. 2. Parents reached through one of those: county (via municipality, else bailiwick),
magistrates court (via hundred), district court (via municipality), court of appeal (via either
court), diocese (via contract). A chain is only as good as its weakest link, so the grade of a
derived row is the lower of the two. 3. Containment fills what is still open, and is marked as
such. Precedence is set per type, not globally, because the sources differ in quality by type: -
contract never comes through the pastorship (review A3: 89 parishes got a contract they do not
overlap), only from the parish's own containment; - containment ranks last for hundred, because
many source hundred polygons are copies of another unit (which is why 1.1.1 rebuilt them from
parishes); - an undated register claim ranks below every dated source: the register asserts one
hundred per parish without saying when, and stretching that over a lifetime is what put
Tannåker, Tjörnarp, Källeryd and Slädene in the wrong härad for three centuries.

### `model/m3c_courts.R` — m3c: The court layer (domsaga / rådhusrätt / tingsrätt) from Sveriges statskalender

The source register mixes two levels in one type ("Domsaga / rådhusrätt"): 44 of its 205 units
in 1882 are single-härad courts where Rosenberg names multi-härad domsagor, and its härad ->
court links are undated (13% open start, 0% open end) so a late composition is copied back to
1600. Sveriges statskalender prints, for every year, which härader, tingslag, skeppslag and
lappmarker made up each domsaga, under which hovrätt, with the seat. PILOT.md ("Statskalender
and the held-out set", 25 September) decides to build the courts from it and to reclassify
`heldout.stk_<year>_harad_domsaga` from held-out test to consistency check. The other
statskalender metrics (fögderi, kontrakt, pastorat) stay held out, because those levels are not
built from it. Rosenberg 1882 stays independent. Dating. An edition prints the state of the year
before it was published, and nothing is known between two editions. A composition seen in
edition Y is evidence for Y and is carried forward to the day before the next edition, so the
intervals tile; the columns start_earliest / end_latest carry the uncertainty (the change can
have happened any time after the previous edition), and precision says which kind of bound it
is. The first edition's claims are never extended backwards: that is exactly the error the
redesign is fixing (a single-period state copied over a whole lifetime). Scope. National, as
PILOT.md asks for mechanical rules. Editions cover 1866-1984; the statskalender is not digitised
on runeberg.org before 1864, so 1600-1865 gets no evidence here and keeps the register's own
(undated) links, which m3b must treat as undated. Rådhusrätter are parsed only from the editions
whose court section lists them (1970); the städer sections of the older editions are not parsed
(see report$limitations). The text helpers handle the runeberg.org OCR and markup of the statskalender, extended for the 1905, 1940-1963 and 1970-1984 layouts.

### `model/m3d_deaneries.R` — m3d: Deaneries (kontrakt) and pastorat from Sveriges statskalender

The ecklesiastikstat of the statskalender names the kontrakt, the pastorat and every parish of
the pastorat, in the same volumes m3c_courts.R already reads for the courts. Three editions are
parsed here: 1866, 1881 and 1931. 1866 and 1881 name a pastorat by its mother parish, 1931 by
all its parishes, and stk_pastorat_parishes() splits either form. The claims are dated as m3c
dates the courts: an edition's statement holds from that edition until the year before the next,
the first edition opens backwards and the last forwards, and the precision says
"between_editions". m3b clips every claim to the parent's own lifetime, so a kontrakt that did
not exist in 1866 does not acquire parishes then. Held-out note: using these volumes for the
build makes the statskalender's kontrakt and pastorat metrics input measures rather than held-
out ones, exactly as its domsaga metrics already are. That is why the second gold sample
excludes the statskalender.

### `model/m3s_sfgt.R` — m3s: SFGT membership evidence, keyed by pid

SFGT names a pastorat by all its parishes ("Acklinga, Agnetorp och Baltak"), while the source
names it after its seat ("Agnetorps pastorat"). The seat is the first name in the list; it
matches a source pastorship for 92% of rows. Where it does not, the claim is kept with
parent_unit NA and the SFGT name, so m3b can still group parishes into a pastorat that the
source lacks. sfgt_lan rows with partial = TRUE are transfers of part of a parish to another
county. They are not county membership and are excluded here; m3b turns them into parish parts.
A name is not unique. "Ekeby pastorat" exists in several counties, and choosing between them on
temporal overlap alone put 369 parishes (5.7% of the pastorship claims) in a pastorat whose
polygon they never touch - the same fault the 1.1.1 audit fixed with "a spatial check on every
SFGT link", which this build had dropped. So the candidates with one name are ranked by the
share of the parish that lies inside each of them (m3's containment), and a claim whose only
candidates contain none of the parish is dropped rather than guessed. It chooses rather than
only filters: where the right Ekeby exists, the link is kept instead of lost.

### `model/m4_derive.R` — m4: Derive the compatibility datasets from atoms + membership

geom_id comes from the frozen lookup, so a unit whose type, topo_id and period are unchanged
keeps the id downstream projects use.

### `model/m4b_matching.R` — m4b: The datasets match_units() needs, rebuilt from the model

unit_variants is rebuilt from unit_names + unit_codes rather than patched, which drops the three
bugs in the earlier variant builder (302 variants ending "vörsamling", 98 "rådusrätt", 28
"vögderi") and adds the 404 earlier official names as dated variants.

### `model/m5_events.R` — m5: Events — what happened to a unit and when

An event says what happened: a unit was created or abolished, merged into another, split off,
had part of its territory transferred, was renamed, was recoded, or changed parent. Renames and
recodings were invisible in the earlier build, which is why a user cannot ask when Kyrkefalla
became Tibro. Event kinds: created, abolished, merged_into, split_from, part_transferred,
renamed, recoded, parent_changed.

### `model/m6_quality.R` — m6: The quality table — what a user can trust, per type and period

This aggregates that into one row per type and 50-year period, so the documentation can state
plainly which levels and centuries are sourced and which are inferred, instead of presenting all
of it as equally reliable. Grades, best first: corrected a documented correction with a citation
(corrections_*.csv) sourced a dated statement in a source: SCB, SFGT, the statskalender, or a
register link that carries dates chained_sourced reached through one unit whose own link is
sourced (parish -> härad -> domsaga from the statskalender) chained reached through one unit,
the link itself from the register asserted the register asserts the link but gives no dates, so
the period is inferred rather than known (most of the parish -> härad links) derived inferred
from geometry: the parish lies inside the unit's polygon `coverage` is the share of that
period's parish-years that have any parent of the type, so a low value and a good grade mean
"incomplete but trustworthy where present" (hundreds outside Norrland), while a high value and a
poor grade mean the opposite.

### `model/skane_transform.R` — Skåne offset correction

The correction is a smooth displacement field d(x, y), added to every vertex of every feature
(all types, all years), because all types were digitised on the same source maps: d(p) =
taper(p) * sum_i w_i K(p, c_i) s_i / sum_i w_i K(p, c_i), K = Gaussian(bandwidth) - c_i, s_i:
control points (1990 parish centroids in Kristianstad, Malmöhus and south Halland, with the
parish's best-fit shift onto its distrikt) and anchors (parishes in the neighbouring counties,
s_i = 0); - w_i: robustness weights of the fit (bisquare); - taper: 1 within taper_r1 of a
control point, 0 beyond taper_r2 (smoothstep between), so the field is exactly zero outside the
affected area. The field is continuous and its gradient is small (|J - I| < 0.052, det
0.95-1.04), so it cannot fold: a vertex shared by two polygons moves to one place, and shared
boundaries stay shared. In the source 96.5% of the vertices of a parish lie on a neighbour as
well, and the union of moved polygons equals the moved union, so no gaps or overlaps are
created. `densify` (metres) would segmentize long edges first, so that a vertex lying in the
middle of a neighbour's edge (a T-junction) follows the same curve as that edge. It is NOT used
(densify_m is empty in the parameters): it triples the vertex count and, because each feature is
segmentized on its own, the two sides of a shared boundary get vertices in different places and
thousands of sub-m2 slivers appear. The T-junction error it would avoid is 10 m2 in total over
all types and years. Parameters: data-
raw/model/skane_transform_params.csv, one row per control point or anchor (role, id, x, y, dx,
dy, w; EPSG:3006 metres) and one row per parameter (role = "param", id, value). source("data-
raw/model/skane_transform.R") b2 <- skane_transform(boundaries) # sf or sfc in EPSG:3006

### `sfgt/combine.R` — SFGT re-parse, step 3: combine the parsed batches into data-raw/sfgt/sfgt_{field}.rda

In step 2 a language model (Claude Opus 5.5) parsed each batch, 2026-09-23, with the prompts in
data-raw/sfgt/prompts/ ({field}.md + common.md): the March 2026 prompts corrected for the errors
found in an audit sample of the March parse: - indelning: split_from = "utbruten
ur X" (this parish broke away from X); split_off = "utbrutet/utbrutna X" without "ur" (X broke
away from this one) - pastorat: decision dates in parentheses inside a range, "ca", "senast",
"före", and a missing space as in "1962Sjösås" - lan: transfers of part of a parish are rows
with partial = TRUE, not periods of membership Step 2 wrote data-
raw/sfgt/results/{field}_result_NN.json. This script combines the results, runs deterministic
checks against the texts, compares with the March parse when it is present and saves
data-raw/sfgt/sfgt_pastorat.rda, sfgt_indelning.rda, sfgt_lan.rda which the build reads. Checks are
written to data-raw/sfgt/checks/ (not in the repository).
