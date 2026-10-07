# How swehist is validated

The data are built from sources that disagree with each other, so the question is never "is the
build correct" but "how far can each level be trusted, and on what evidence". This file says what is
measured, with what, and what the measurements are for. The validation scripts and their results
are not part of this repository; this file is their summary.

## 1. The build checks its own data

21 checks run over **every unit, with no sampling**, and
list every violation rather than stopping at the first. They are invariants, not statistics: a
parish with two counties at one date, a code on two coexisting units, a unit whose polygon is not
inside the parent the hierarchy gives it, a source record that reached no unit, territory that no
unit of a type covers. Each check is graded by how severe a violation is — a silent wrong answer, a
missing answer, or a cosmetic fault — and the violations that are allowed are listed, each with a
cited reason.

Two of them exist because of the model the build uses. Every unit above the parish is the union of
its member parishes, so a unit whose members were never established would simply disappear;
**a11** counts the units that keep their source polygon instead, because each one is a piece of
missing membership. **a12** is the other side: a source record that reached no unit at all and that
no correction accounts for.

## 2. What the data say about themselves

`quality_table` reports, for each type and 50-year period, how much of the country the type covers
and what kind of evidence its memberships rest on, best first: a documented correction, a dated
source, a chain through one intermediate unit, an undated register assertion, or geometry alone. It
is part of the shipped data because it is the honest answer to "can I trust the hundred column in
1750": the county and the municipality rest on dated sources, the härad on the register's assertion
without dates, and where a level rests on geometry the table says so.

`known_issues` is the same principle for single cases: findings that research established but no
source lets us correct, such as the Malingsbo/Söderbärke division of 1708-1969, where the combined
territory is right, the line between them is about 235 km² out, and no map of the historical
division was found.

## 3. Independent data: development instruments

A second set of instruments compares a build with data that swehist does not use, or does not
use for the level being measured: the DDB parish code catalogue, the Demographic Data Base's death
book 1860-1990, Lantmäteriet's distrikt of 1999, NAPP 1900, the 1930 census, Rosenberg's
*Geografiskt-statistiskt handlexikon* (1882), BiSOS 1880, SCB's code lists, church points from
OpenStreetMap and Lantmäteriet, and 190 documented cases from the September 2026 review.

These are **development instruments**. They are used to find and fix faults, so their numbers
measure progress, not an error rate: a fix aimed at one of them cannot then be tested by it. Where a
source became a build input — SCB's municipalities, the statskalender's courts and deaneries — its
metrics measure consistency with a build input, not independent agreement.

Further instruments work on the build's intermediate tables instead of
the finished package, so a rule can be tried in minutes rather than in a full build. The precedence
of one source over another was decided with one of them.

## 4. Held out: measured once, and never used while fixing

A number is only an error rate if nobody tuned the data to it. Two instruments are therefore sealed:

- **ISOF's Ortnamnsregister** (3.7M place records with their socken and härad), used geometrically.
- **A gold sample of parish-years**, each one's parish name, county, hundred, bailiwick, courts,
  pastorat, kontrakt, diocese and municipality researched blind from printed sources, with the
  source and locator recorded for every fact.

The first gold sample (200 parish-years, 2,040 facts) was measured on
both builds and then read case by case, to find out why the model lost facts.
That reading found a scoring artefact and a real fault, and it is documented — but a set whose
disagreements have been examined one by one can no longer measure the work that followed it. It is a
development instrument now.

Its replacement is 100 new parish-years with no parish in common with
the first, researched blind under a stricter source rule: it excludes *Sveriges statskalender* as
well, because the build takes its courts and deaneries from those volumes. Every fact records its
source, so the held-out property can be **checked from the data** rather than promised — the scorer
can drop any fact whose source later becomes a build input.

The two sealed samples were scored on different builds - the first on the build of 27 September,
the second on that of 29 September before the Blekinge correction - so the pooled figure mixes them
and is accurate to about a percentage point rather than exactly.

Held-out measurements are **paired**: the same facts on both versions, compared with McNemar's test
on the discordant pairs. The unpaired comparison of two proportions both understated a real
difference and hid that another was an artefact of the instrument.

### How accurate the data are

Two sealed samples have now been measured, 200 parish-years and 2,200 researched facts in all, and
scored on the build described here under one rule for both. This is the headline result of the whole
validation, and it is an **absolute** figure: how often the shipped data name the same unit as a
source that was researched without seeing them.

**85.1% of the 1,489 facts that name a unit** (95% cluster bootstrap 82.7–87.3). The two samples,
researched months of source-work apart, agree closely: 85.3% of 680 and 84.9% of 809.

A further 329 scored facts say that no unit of that type existed in that year — a town outside any
hundred, a district court before 1971 — and the database agrees with 96.7% of them by being silent.
Those are reported separately, as the samples' own protocol requires, because agreeing with them only
requires the data to say nothing, which is a far easier test than naming the right unit. Counting
them together gives 87.2% of 1,818, and that figure should always carry the qualification.

Accuracy varies by a third across levels and rises steeply through time:

| level | agreement | named facts | | period | agreement | named facts |
|---|---|---|---|---|---|---|
| diocese | 0.974 | 195 | | 1650–1749 | 0.692 | 224 |
| municipality | 0.941 | 85 | | 1750–1849 | 0.770 | 283 |
| court of appeal | 0.931 | 159 | | 1850–1899 | 0.872 | 382 |
| pastorat | 0.925 | 160 | | 1900–1949 | 0.910 | 376 |
| hundred / tingslag | 0.922 | 154 | | 1950–1990 | 0.978 | 224 |
| county | 0.886 | 176 | | | | |
| magistrates court | 0.775 | 89 | | | | |
| parish name | 0.733 | 172 | | | | |
| bailiwick | 0.703 | 128 | | | | |
| kontrakt | 0.667 | 159 | | | | |

District courts are left out of that table: of 188 facts about them only **12 name a unit**, because
the type exists solely from 1971 and most of the sample predates it. Their apparent perfect score is
almost entirely the database correctly saying nothing.

**Which facts count.** One rule is applied to both samples: a fact is dropped if its source is one the
build uses, if it rests on the standard reference work on judicial districts, or on the 2015 statute
whose division Lantmäteriet administers. That rule was designed for the second sample and is stricter
than the first sample's own, so applying it to both drops 231 facts from the first and 18 from the
second, and it raises the figure — the gain sits in magistrates courts and courts of appeal, the two
levels whose remaining evidence had leaned on a work the build itself uses. The facts about
Blekinge's court of appeal are excluded outright, because the correction that fixed them was found by
reading the second sample; scoring them would measure a fix against the evidence that produced it.

**The differences between levels are systematic.** The two samples put the levels in the same order:
their per-level rates on named facts correlate at r = 0.75 (Spearman 0.75, district courts excluded).
Kontrakt, fögderi and the judicial districts are weak because their membership is largely derived
from spatial containment rather than from a dated source.

**The grade column does not predict accuracy, and this is the most uncomfortable result here.**
`hierarchy$grade` says what a membership rests on, and it was natural to expect better evidence to
mean more agreement. On named facts it does not. Pooled: `chained_sourced` 0.99 (87 facts),
`chained` 0.95 (189), `asserted` 0.92 (227), `sourced` 0.88 (465), `derived` 0.81 (297), `corrected`
0.71 (21). Within the hundreds the order is inverted outright — `asserted` 0.98 on 125 facts against
`sourced` 0.80 on 15 and `corrected` 0.58 on 12.

The likely reason is selection, not noise: a dated source was sought, and a correction researched,
exactly where the easy answer looked doubtful. The grades therefore mark **where the work went**, not
where the data are right, and the hardest cases carry the best-sounding grades. Several of these
cells are small — 21 corrected facts, 12 for hundreds — so the sizes of the differences are uncertain,
but the absence of the expected ordering is not. Read `grade` as provenance, which is what it
records, and read the level and the period for expected accuracy.

Three caveats belong with the numbers. Each level rests on 139–198 facts, so a single level's figure
carries a couple of points of noise and the *ranking* replicates far better than the point estimates.
The parish-name figure is closer to a floor than an error rate: much of it is orthography and naming
convention (*Sveneby / Sväneby / Svenneby* for one parish) rather than the wrong parish. And the
early periods are measured against thinner sources, so some of the 1650–1749 disagreement is the
source's uncertainty rather than the data's.

### Whether this is better than what came before

Before the rebuild, the same datasets were produced by repairing the polygons Riksarkivet drew for
each unit. That earlier build was never released; the version number 1.1.1 belongs to the rebuilt
data described here, so the comparison below is internal engineering evidence, not a claim about any
published dataset.

Both sealed samples were scored on both builds, paired, with McNemar's exact test on the discordant
pairs. The first favoured the rebuild (0.828 → 0.840, 14 facts gained against 3, p = 0.013); the
second did not (0.840 → 0.837, 3 against 6, p = 0.51). **Pooled, there is no overall difference:**
0.834 against 0.838, 17 against 9, p = 0.17. Nine facts moving in a thousand is within noise, and
samples of this size can detect a large change but not a small one.

One difference does replicate: **hundreds, 10 facts gained against 1, p = 0.012** (0.816 → 0.862),
which is the level the rebuild was principally made for — the 908 researched corrections and the
tingslag the earlier build dropped, taking coverage of the country from 33% to 96.9%. The rebuild's
case rests on that, on the coverage, and on the fact that every level now reports what it rests on;
it does not rest on a broad accuracy gain, and none is claimed.

### What reading the disagreements found

Both samples are spent now: their disagreements have been read, which is what turns a measurement
into a development tool, and nothing below can be re-measured on them. Reading them found faults no
internal check had caught.

- **Corrected.** *Östra och Medelstads domsaga* flapped between Göta hovrätt and Hovrätten över
  Skåne och Blekinge — Göta in 1866–1904 and 1915–1930 — putting about 23 of Blekinge's 40 parishes
  under the wrong court of appeal for 55 of the years between 1866 and 1930. Blekinge's three other courts are right
  throughout, so the fault is demonstrable without the sample; that is why correcting it is
  legitimate rather than fitting the data to the instrument (`corrections_courts.csv`, C001).
- **Documented, not corrected.** Varnum 1720 sits in Herrljunga kontrakt where a prostlängd names
  the kontraktsprost of Ås kontrakt; Bjurbäck 1806 in Vartofta kontrakt where the 1859 lexikon puts
  its pastorat in Redvägs kontrakt. Both are containment following the härad — the documented
  weakness above. Neither has corroboration inside the dataset, so correcting them would be fitting
  two rows out of 1,100 to the instrument. They ship in `known_issues` instead.
- **A scoring limit, left uncorrected.** One kontrakt is scored wrong because the data give its full
  name (*Bankekinds och Skärkinds*, enlarged 1919) where the source gives the short one. The scorer
  folds that for pastorat but not for kontrakt. Changing it after seeing which way it cut would not
  be a measurement, so it was not changed.

That is what a sealed sample is for: it did not hand the rebuild a margin, and it found a county's
court of appeal wrong for 55 years.

## 4b. Who did the research

I designed the data model, set the rules for which sources count as evidence and in what order,
and decided the cases where the sources disagree. Within that design AI agents (Claude) gathered
much of the evidence the validation above rests on, which bears on how to read it. The agents
researched the 1,310 correction rows, each citing the source it rests on, and wrote the scripts
that parsed the statskalender tables from scanned text. They also researched both held-out samples
from printed sources they cited, without sight of the database. That is what makes the samples held
out, but it also means the instrument and the thing measured were built by the same kind of worker.

Against that, I checked about 100 of the held-out facts by hand, chosen by browsing the samples
rather than by a fixed rule and including many of the facts where the database and the source
disagree. For each I confirmed that the cited source, at the recorded
place, gives the recorded unit and that the unit is historically right, and for each disagreement
also that **the database, not the fact, is wrong**. I found a few minor faults and no reason to
doubt the facts as a whole.

That matters for how the agreement figures should be read. The disagreements are what the figures
count, and a check of many of them found the fault on the database's side rather than the
researcher's, so the rates above are an error rate of the data rather than partly an artefact of bad
research. However, the sample was chosen by browsing and not at random, so it supports the facts
without measuring them: it gives no error rate for the research itself.

I have not checked the correction rows one by one. The warrant for any single value is its
citation, which is in the data and can be followed. Where a result matters, check the citation.

## 5. What the data do not cover, and why not

Some gaps were looked at and deliberately left. In each the only way to fill them would be to carry
a later state backwards, which is the error this rebuild exists to remove, so the data answer
nothing instead of answering confidently.

- **Counties and bailiwicks before about 1700.** The register's county units begin in 1686 (three of
  them) and 1720 (the rest), and its bailiwick memberships are almost absent before 1700
  (`coverage` 0.005 for 1600-1649). Counties were created in 1634, so the units are younger than the
  thing they represent — but the län were reorganised in 1634, 1654, 1683, 1714 and 1719-20, and
  Halland, Bohuslän, Skåne, Blekinge, Gotland and Jämtland were not Swedish for part of that time.
  The 1720 structure is therefore not the 1650 structure, and painting it back would put parishes in
  counties that did not hold them. The fögderi division is documented as unstable through most of
  the 17th century and is not in any printed source we could reach.
- **Kontrakt composition rests on the register's polygons**, not on a dated source. Taking it from
  *Sveriges statskalender* was tried and measured worse against BiSOS 1880 (see §3), so the
  `sourced` grade is not claimed for that level.
- **`forkod` is one vintage.** A parish's code in `unit_codes` is the code it had last, in or after
  1974. The 1930 census uses the scheme of its own day, in which a stad's rural parish has parish
  part 00, and those codes are not in the data; the parishes are found by name instead. A dated code
  history, from SCB's lists year by year, is the fix.
- **Tingslag polygons in Norrland and most of Dalarna.** Those regions had tingslag, not härader, and
  the source has polygons for neither. The judicial chain works there (parishes are linked to their
  domsaga), but there is no polygon to draw.
- **A municipality renamed before 1952 may carry its later name for its whole life.** Version
  periods do break at a recorded rename — 691 units across all types have more than one dated
  official name and are named by the one in force — but municipality names come from SCB's lists,
  which begin in 1952, so only 38 of 2,924 municipality units have a second name recorded, and 732
  of them start before 1952 and survive it. Fässbergs landskommun is called Mölndals stad for
  1863-1921, the name it took in 1922. Towns are also missing before the SCB lists: Bodens stad is
  absent altogether and Katrineholms stad begins in 1961, though both were towns from 1917-19.
- **Sixteen hundreds have too few parishes in their earliest periods** — Västerrekarne has one before
  1931 — so their polygons for those years are smaller than the unit was.

`quality_table` carries two columns for reading these honestly: `existed`, the share of a period in
which any unit of the type existed at all, and `coverage_existed`, the coverage over those years
only. Municipality coverage in 1850-1899 is 0.74 because municipalities begin in 1863;
`coverage_existed` is 1.00. Where both are low, as for county membership before 1700, the data
really are incomplete.

## 6. Reproducibility and identity

Two clean builds of the rebuilt data are **byte-identical for every dataset and for the frozen-id
ledger** (checked 29 September): all 13 datasets and `data-raw/geom_id_lookup.csv`
match between the two runs and match what is committed, and all 13,036 features keep the same id for
the same unit and period.

That claim was made once before, on 27-28 September, on worthless evidence. The script snapshotted
`data/*.rda`, which `build_data.R` never writes — only `m7_install.R` does, and the script does not
run it — so it compared the same untouched committed files three times and could not have failed.
Its geom_id identity test reported `NA` rather than a count, because `topo_id` is NA for the units
that exist only in SCB's lists and `NA == NA` is NA. Both are fixed: the script snapshots
`data-raw/model/out/pkg/data/`, which the build does write, and the identity test is NA-safe and
now prints `13036 (differing 0)`.

Getting there found a defect in the id mechanism itself. The frozen-id key was
`type_id + topo_id + start + end`, and the municipalities that exist only in SCB's lists have no
Riksarkivet `topo_id`, so three of them shared the key "municipality NA 1863 1951" and three shared
"municipality NA 1967 1970". Colliding keys cannot be matched, so those six units were issued a fresh
id on every build — Lärbro landskommun had collected seven — and the ledger grew by six rows each
time. The key now falls back to the `ref_code` and then the name where there is no `topo_id`; the six
units are back on their original ids and the ledger no longer moves. No published id was ever wrong:
within any one build the ids were consistent, and the drift was in the ledger.

`geom_id`s are frozen in `data-raw/geom_id_lookup.csv`: a unit keeps its id as long as it exists with
the same period, the build writes every id it issues back to the lookup, and retired ids are never
reused. `unit_id` is the identity behind the ids — it runs through renames, recodings and boundary
changes — and `data-raw/pid_lookup.csv` does the same for parish pids.

## 7. What the numbers were, and where they are

The result tables, the sealed samples with every fact's source, and the review notes with the
readings, the decisions and what each measurement cost are kept in the accompanying report
repository, not in this package.
