#' @name boundaries
#' @title Historical administrative boundaries
#' @description Swedish historical administrative boundaries for 11
#'   administrative types 1600-1990, as an sf object in SWEREF99 (EPSG:3006).
#'   Types: Parish, County, Municipality, Pastorship, Contract, Diocese,
#'   Hundred, Magistrates Court, District Court, Court of Appeal, Bailiwick.
#' @docType data
#' @usage data(boundaries)
#' @format An \code{sf} object with the following columns:
#' \describe{
#'   \item{geom_id}{Unique geometry ID (integer)}
#'   \item{unit_id}{The unit this feature is a version of, e.g. "P:00412".
#'     A unit keeps its id through renames, recodings and boundary changes, so
#'     all versions of one parish, county or court share it, and it is the key
#'     to \code{\link{relations}} and \code{\link{hierarchy}} when what you
#'     want is the unit rather than one of its periods}
#'   \item{topo_id}{UUID from Riksarkivet}
#'   \item{ref_code}{Reference code from Riksarkivet}
#'   \item{name}{Unit name, as it was in force during the feature's period}
#'   \item{geometry_from}{"members" where the polygon is the union of the
#'     parishes that belonged to the unit in the period, "source" where no
#'     evidence established a membership and the unit keeps the polygon
#'     Riksarkivet recorded for it (cut against the units of its own type that
#'     were built from members). Parishes are always "source": they are the
#'     atoms everything else is built from}
#'   \item{kind}{The institution, where the type holds more than one:
#'     "domsaga", "rådhusrätt" or "tingsrätt" for courts, "härad", "tingslag"
#'     or "stad" for hundreds, and "stad", "köping", "landskommun",
#'     "municipalsamhälle" or "kommun" (from 1971) for municipalities. NA for
#'     the types that hold one institution}
#'   \item{type}{English type label: "Parish", "County", "Municipality",
#'     "Pastorship", "Contract", "Diocese", "Hundred", "Magistrates Court",
#'     "District Court", "Court of Appeal", "Bailiwick"}
#'   \item{type_id}{Type identifier: "parish", "county", "municipality",
#'     "pastorship", "contract", "diocese", "hundred", "magistrates_court",
#'     "district_court", "court_of_appeal", "bailiwick"}
#'   \item{start}{Start year (min 1600)}
#'   \item{end}{End year (max 1990)}
#'   \item{geometry}{MULTIPOLYGON geometry in SWEREF99 TM (EPSG:3006)}
#' }
#' @section Coverage:
#' Not every type covers all of Sweden in every year. Coverage is the share of
#' the parish territory (\code{\link{sweden}}, the union of all parishes) that
#' a type's units cover: whether each parish lies in a unit of the type. Each
#' unit is built from its member parishes, so a gap means that no source places
#' those parishes in a unit of that type, not that the polygons fail to meet.
#' \itemize{
#'   \item parish: 100\% throughout (99.9\% in 1600)
#'   \item pastorship: 63\% in 1600, then at least 96\% from 1607
#'   \item contract: 85\% in 1600, then at least 99.8\% from 1647
#'   \item diocese: 71\% in 1600, then at least 98.9\% from 1647 (the source
#'     enters the medieval dioceses at their register date; the periods of
#'     Uppsala, Linköping, Skara, Strängnäs and Karlstad are corrected)
#'   \item hundred: at least 96.9\% from 1600 to 1970. Härader, tingslag and
#'     the towns outside them are all in the \code{hundred} type; \code{kind}
#'     tells them apart. Härader lost their judicial role in 1971, and the 34\%
#'     coverage after that is only the units the source continues to 1990
#'   \item magistrates_court: 14\% in 1600, 100\% from 1687 to 1970
#'     (abolished 1971)
#'   \item district_court: 100\% from 1971 (created 1971)
#'   \item court_of_appeal: 100\% from 1687
#'   \item county and bailiwick: 100\% from 1720. The source has no county or
#'     bailiwick before then, although counties were created in 1634
#'   \item municipality: at least 99.4\% from 1863 (municipalities did not
#'     exist before); 1952-1990 is built from SCB's code lists
#' }
#' \code{get_boundaries()} warns when the requested map covers less than
#' 90\% of Sweden. \code{\link{quality_table}} says what the memberships
#' behind each type and period rest on.
#'
#' @section Things to know:
#' \itemize{
#'   \item Parishes: before the local government reform of 1862 a parish
#'     (\emph{socken}) was both a civil and a church unit; after it the parish
#'     (\emph{församling}) is a church unit and the municipality
#'     (\emph{kommun}) the civil one.
#'   \item Magistrates courts include both rural courts (\emph{domsagor}) and
#'     town courts (\emph{rådhusrätter}); the \code{kind} column tells them
#'     apart, from the statskalender where it identifies the court and from the
#'     name otherwise.
#'   \item How well founded a parent is varies by type and century:
#'     \code{\link{quality_table}} gives the coverage and the kind of
#'     evidence for each, and \code{\link{known_issues}} lists the boundary
#'     problems that are established but that no source lets us correct.
#'   \item Municipalities in 1952 and 1971 miss some towns and market towns
#'     (\emph{städer}, \emph{köpingar}) that the source does not have.
#' }
#'
#' @section Sources and processing:
#' The Riksarkivet shapefile is in RT90 2.5 gon V (EPSG:3021); it is
#' transformed to SWEREF99 TM with the proper datum shift. Diocese
#' (from contracts) and hundred (from parishes) boundaries are rebuilt from
#' their children because many source polygons of those types are wrong.
#' Units recorded twice in the source under different names (e.g.
#' Kopparbergs / Dalarnas län) are kept once; the other names are
#' matched by \code{\link{match_units}}. \code{geom_id}s are stable across
#' rebuilds: a unit keeps its id as long as it exists with the same period.
#'
#' Every unit above the parish is the union of its member parishes for the
#' period, and it is shipped that way, holes and all. It has a hole wherever
#' the parishes do not reach: over a lake, which no parish covers, around a
#' town that had its own jurisdiction and is its own unit of the same type
#' (\code{kind = "stad"} at the hundred level), and along the slivers where
#' the source's parish polygons do not quite meet. Closing them in the data was
#' tried and reverted: filling is per type, so a filled layer no longer nests
#' inside another type's, and a court of appeal that absorbs its lakes covers
#' more than the parish territory the coverage figures are measured against.
#' For a map, ask for it instead:
#' \code{get_boundaries(date, type, fill_holes = TRUE)}.
#'
#' The parish layer itself is generalised along the coast. An archipelago
#' parish is one to a few smoothed shapes where the ground is dozens of
#' islands, and 1.4\% of Lantmateriet's named settlements fall in no parish at
#' all, concentrated in the island municipalities (Ockero 59\%, Varmdo 26\%).
#' @source Based on administrative boundaries from Riksarkivet.
NULL

#' @name parish_meta
#' @title Parish metadata
#' @description Extended metadata for historical parish boundaries,
#'   including numeric codes and county linkage. For a richer linking table
#'   with additional codes and names, see \code{\link{parish_link}}.
#' @docType data
#' @usage data(parish_meta)
#' @format A \code{tibble} with the following columns:
#' \describe{
#'   \item{geom_id}{Unique geometry ID, links to \code{boundaries} (integer)}
#'   \item{topo_id}{UUID from Riksarkivet ARKIS/NAD database}
#'   \item{ref_code}{Riksarkivet reference code (format \code{SE/CCKKKKVVV})}
#'   \item{name}{Parish name (e.g. "Jukkasjärvi församling")}
#'   \item{start}{Start year (min 1600)}
#'   \item{end}{End year (max 1990)}
#'   \item{nadkod}{NAD-kod from Riksarkivet (9-digit: county + unit + version)}
#'   \item{forkod}{Församlingskod / LKF-kod from SCB (6-digit: county +
#'     municipality + parish). Used in population registration until 2015.}
#'   \item{dedikscb}{SCB dedication code (identical to forkod)}
#'   \item{county}{County code (1--27)}
#' }
#' @source Compiled from Riksarkivet \code{Tbl_topografi.csv},
#'   \code{parish_medta.csv}, and \code{par_to_county.csv}.
NULL

#' @name relations
#' @title Administrative boundary relationships
#' @description Predecessor/successor relationships between administrative
#'   boundaries, tracking how units changed over time across all 11 types.
#' @docType data
#' @usage data(relations)
#' @format A \code{tibble} with the following columns:
#' \describe{
#'   \item{parent_id}{geom_id of parent/earlier unit}
#'   \item{child_id}{geom_id of child/later unit}
#'   \item{type_id}{Type identifier (e.g. "parish", "county", "municipality")}
#'   \item{transition_year}{Last year of the parent unit; the child starts
#'     the following year}
#'   \item{relation}{\code{"successor"} or \code{"transfer"}}
#' }
#' @details A child succeeds a parent when it starts the year after the
#'   parent ends and the two share territory: the overlap must be at least
#'   10\% of the smaller unit (and at least 1 ha). This keeps boundary slivers
#'   from linking neighbouring units. A smaller overlap of at least 5 km2 is
#'   recorded as a \code{"transfer"}: a piece of territory moved from one
#'   unit to the other (a village moving between counties). Period maps and
#'   the matching functions follow successors only; \code{unit_history()}
#'   shows transfers with \code{transfers = TRUE}.
#' @source Computed from spatial and temporal overlap analysis.
NULL

#' @name hierarchy
#' @title Administrative hierarchy relationships
#' @description Hierarchical parent-child relationships between Swedish
#'   administrative units, recording which units were contained within which
#'   at a given time. Covers civil, fiscal, judicial, ecclesiastical, and
#'   military branches. Derived from Riksarkivet's \code{Tbl_topografi_rel.csv}.
#' @docType data
#' @usage data(hierarchy)
#' @format A \code{tibble} with the following columns:
#' \describe{
#'   \item{parent_geom_id}{geom_id of the containing (parent) unit in
#'     \code{boundaries}. \code{NA} for Regiment parents, which have no
#'     spatial geometry.}
#'   \item{child_geom_id}{geom_id of the contained (child) unit in
#'     \code{boundaries}}
#'   \item{parent_type}{English type of parent: "County", "Municipality",
#'     "Bailiwick", "Hundred", "Magistrates Court", "Diocese", "Regiment", etc.}
#'   \item{child_type}{English type of child unit}
#'   \item{parent_ref_code}{Riksarkivet reference code of the parent unit}
#'   \item{parent_name}{Name of the parent unit}
#'   \item{start}{Start year of the containment relationship}
#'   \item{end}{End year of the containment relationship}
#'   \item{source}{Where the link comes from. Direct evidence:
#'     \code{"register_dated"} and \code{"register_undated"} (Riksarkivet's
#'     \code{Tbl_topografi_rel.csv}, with and without dates), \code{"scb"}
#'     (Statistics Sweden's code lists, municipalities 1952-1990),
#'     \code{"sfgt"} (the parish histories, checked for containment),
#'     \code{"statskalender"} (\emph{Sveriges statskalender}),
#'     \code{"containment"} (spatial containment alone) or
#'     \code{"correction"} (a documented correction with a citation). A
#'     \code{"via_<type>_..."} value means the parent was reached through an
#'     intermediate unit of that type, and a \code{"+gap"} suffix that the
#'     period was extended across a gap the sources leave.}
#'   \item{grade}{What the link rests on, from \code{"corrected"} through
#'     \code{"sourced"}, \code{"chained_sourced"}, \code{"chained"} and
#'     \code{"asserted"} to \code{"derived"} (containment alone). A chain
#'     takes the weaker of its two steps.
#'
#'     \strong{Read this as provenance, not as expected accuracy.} Measured
#'     against two sealed samples of parish-years researched from printed
#'     sources, the grades do not rank agreement: pooled over the facts that
#'     name a unit, \code{"asserted"} agrees 92\% and \code{"sourced"} 88\%,
#'     and within the hundreds the order is inverted outright
#'     (\code{"asserted"} 98\% on 125 facts, \code{"sourced"} 80\% on 15,
#'     \code{"corrected"} 58\% on 12). The likely reason is selection: a dated
#'     source was sought, and a correction researched, exactly where the easy
#'     answer looked doubtful, so the hardest cases carry the best-sounding
#'     grades. For expected accuracy read the level and the period in
#'     \code{VALIDATION.md}; some of these cells are small, so the sizes are
#'     uncertain, but the absence of the expected ordering is not.}
#' }
#' @details
#' The hierarchy encodes several parallel administrative branches:
#' \itemize{
#'   \item Civil: County -> Municipality -> Parish (and County -> Parish from
#'     SFGT for parishes that changed county)
#'   \item Fiscal: County -> Bailiwick -> Parish
#'   \item Judicial: Court of Appeal -> Magistrates Court -> Hundred -> Parish
#'     (Magistrates Court -> Parish where there were no hundreds)
#'   \item Judicial (modern): Court of Appeal -> District Court -> Municipality
#'   \item Ecclesiastical: Diocese -> Contract -> Pastorship -> Parish
#'   \item Military: Regiment -> Parish
#' }
#'
#' Sources of the links: Riksarkivet's \code{Tbl_topografi_rel.csv} for most
#' pairs; Contract -> Pastorship and Pastorship -> Parish from spatial
#' containment; Magistrates Court -> Parish by containment where the court has
#' no hundreds (Norrland and Dalarna had tingslag, which the source lacks); SFGT (Skatteverket) for additional Pastorship -> Parish and
#' County -> Parish links, each kept only if the parish lies inside the
#' parent. Every link's period lies within both units' lifetimes, and a child
#' has at most one parent of each type in any year (where the sources gave
#' two, the parent that contains the child is kept). In checks of the built
#' data, parents contain at least half of their children's area for every
#' link type.
#'
#' Regiment parents have \code{parent_geom_id = NA} because regiments have no
#' spatial geometry in \code{boundaries}. Use \code{parent_ref_code} and
#' \code{parent_name} to identify them. A parish can belong to more than one
#' regiment at a time (farms within it served different regiments).
#' @source Derived from Riksarkivet \code{Tbl_topografi_rel.csv} with temporal
#'   alignment to \code{boundaries} features.
NULL

#' @name parish_codes
#' @title Every code a parish version has had
#' @description \code{parish_link} carries one code of each kind per parish
#'   version. A parish has often had several: the Demographic Data Base gives
#'   Byske both 82983 and 82990, Stockholm's city parishes a code of their own
#'   besides the city's, and chapels, bönehus and works congregations codes of
#'   their own that lie within the parish; SCB recoded parishes in 1952--1990.
#'   Data from a register carry the code valid at their date, so
#'   \code{match_units()} looks here when \code{parish_link} has no match.
#' @docType data
#' @usage data(parish_codes)
#' @format A \code{tibble}, one row per parish version, code system and code:
#' \describe{
#'   \item{geom_id}{The parish version (\code{boundaries}, \code{parish_link}).}
#'   \item{pid}{The parish's SFGT pid.}
#'   \item{system}{\code{"dedik"} (DDB/POPLINK), \code{"forkod"} (SCB) or
#'     \code{"nadkod"} (Riksarkivet NAD, = NAPP PARSE).}
#'   \item{code}{The code (numeric).}
#'   \item{start, end}{Years the code applies to this version: the code's own
#'     validity where a source dates it (SCB lists 1952--1990), else the
#'     version's lifetime.}
#'   \item{precision}{\code{"day"} or \code{"year"} when the source dates the
#'     code, \code{"none"} when it does not.}
#'   \item{source}{Where the code comes from. A DDB code matched "within its
#'     SCB74 group" or "county" belongs to a part of the parish (a chapel, a
#'     bönehus, a kyrkobokföringsdistrikt) or to a congregation without
#'     territory of its own (lappförsamling), and points to the parish it lies in.}
#' }
#' @source Built by \code{data-raw/model/m1b_codes.R} from the DDB parish
#'   catalogue, SCB's parish code lists 1952--1990, Riksarkivet's NAD codes and
#'   Skatteverket's parish registry (SFGT).
NULL

#' @name parish_link
#' @title Parish linking table
#' @description Enriched parish metadata for linking external datasets to
#'   spatial data. Combines codes from three sources:
#'   \itemize{
#'     \item \strong{Riksarkivet} (Swedish National Archives): NAD-kod, topo_id,
#'       ref_code from the ARKIS/NAD topographic register
#'     \item \strong{Skatteverket} (Swedish Tax Agency): parish registry data from
#'       "Sveriges församlingar genom tiderna" (SFGT, 1989) including pid, charid,
#'       and alias
#'     \item \strong{SCB} (Statistics Sweden): LKF parish codes (forkod/dedikscb),
#'       dedication codes (dedik), area
#'   }
#'
#'   Use \code{geom_id} to join to \code{boundaries} for spatial data. Use
#'   \code{nadkod}, \code{forkod}, \code{socken}, or \code{alias} to match
#'   records from external datasets. \code{pid} identifies the parish's entry
#'   in SFGT.
#' @docType data
#' @usage data(parish_link)
#' @format A \code{tibble} with the following columns:
#' \describe{
#'   \item{geom_id}{Unique geometry ID, links to \code{boundaries} (integer).
#'     Primary key.}
#'   \item{name}{Full parish name (e.g. "Jukkasjärvi församling")}
#'   \item{socken}{Short name without "församling" suffix, for matching
#'     (e.g. "Jukkasjärvi"). Retains stads-/lands-/domkyrkoförsamling
#'     distinctions where relevant.}
#'   \item{alias}{Alternative/colloquial names from SFGT redirect entries
#'     (e.g. "Kiruna, Simojärvi" for Jukkasjärvi). Comma-separated.}
#'   \item{start}{Start year (min 1600)}
#'   \item{end}{End year (max 1990)}
#'   \item{nadkod}{NAD-kod (Nationell Arkivdatabas-kod) from Riksarkivet.
#'     9-digit: CC (county) + KKKK (unit) + VVV (temporal version).
#'     First 6 digits = forkod. (numeric, 88\% coverage)}
#'   \item{forkod}{Församlingskod / LKF-kod (Län-Kommun-Församling) from SCB.
#'     6-digit: LL (county) + KK (municipality) + FF (parish). Used in
#'     population registration (folkbokföring) until 2015. Not unique per
#'     row: temporal splits share the same forkod. (numeric, 88\% coverage)}
#'   \item{dedikscb}{SCB dedication code. Identical to forkod in all rows.
#'     (numeric, 88\% coverage)}
#'   \item{dedik}{Sequential geographic catalog number, possibly from the
#'     Demographic Database at Umeå University. Range 50000--85600, spaced
#'     by 10. Not alphabetical or county-ordered. (numeric, 88\% coverage)}
#'   \item{grkod}{Grade code indicating parish type. Constant value 900
#'     (= kyrksocken/standard parish) for all rows in this dataset.
#'     (numeric)}
#'   \item{county}{County code (1--27, historical Swedish counties)}
#'   \item{letter}{County letter abbreviation (e.g. "AB", "C", "BD")}
#'   \item{county_name}{County name (e.g. "Stockholms", "Norrbottens")}
#'   \item{center}{County administrative center / residensstad
#'     (e.g. "Stockholm", "Luleå")}
#'   \item{area}{Parish area (approximate, from SCB metadata)}
#'   \item{pid}{Parish ID: one per SFGT entry, stable across releases
#'     (\code{data-raw/pid_lookup.csv}). (integer, 100\% coverage)}
#'   \item{charid}{HTML anchor ID from Skatteverket's SFGT web pages.
#'     URL fragment identifier for direct links. (character, 98\% coverage)}
#'   \item{topo_id}{UUID from Riksarkivet ARKIS/NAD database}
#'   \item{ref_code}{Riksarkivet reference code (format \code{SE/CCKKKKVVV})}
#'   \item{parse_code}{9-character NAPP PARSE code (character, zero-padded).
#'     Equivalent to \code{sprintf("\%09d", nadkod)}. For mapping IPUMS NAPP
#'     Swedish census data (1880--1910) to swehist. \code{NA} when nadkod is
#'     missing. (88\% coverage)}
#' }
#' @details
#' Coverage: 3,080 parishes. Code fields (\code{nadkod}, \code{forkod}, etc.)
#' are available for 88\% of parishes. The remaining 12\% are early-period
#' parishes (pre-1700) that predate the coding systems. The \code{pid} field
#' has 100\% coverage via a 6-strategy matching chain (forkod, nadkod,
#' exact name, genitive normalization, alias, and manual matching).
#'
#' Note that \code{forkod} is not unique per row: temporal splits (e.g.
#' Ljusterö -> Norra/Södra Ljusterö) share the same forkod. Use
#' \code{geom_id} as the primary key.
#'
#' For linking, \code{socken} + \code{county} + time period usually provides
#' a unique match. The 234 ambiguous socken+county combinations are all
#' temporal versions of the same parish.
#' @source Compiled from Riksarkivet \code{Tbl_topografi.csv} and
#'   \code{parish_medta.csv}, SCB \code{county_meta.csv}, and Skatteverket's
#'   \emph{Sveriges församlingar genom tiderna} (SFGT, 1989).
NULL

#' @name parish_registry
#' @title Comprehensive parish registry
#' @description Complete registry of Swedish parishes from the Skatteverket
#'   "Sveriges församlingar genom tiderna" (SFGT, 1989), covering both
#'   territorial parishes (with geometry in \code{boundaries}) and non-territorial
#'   parishes (bruk, garrison, hospital, kbfd, mosaiska, etc.) that have no
#'   spatial representation. Provides all available name variants and identifier
#'   codes in a single wide table for cross-dataset linking.
#'
#'   While \code{\link{parish_link}} covers only the 3,080 temporal versions
#'   of parishes with geometry, \code{parish_registry} covers the full
#'   Skatteverket universe of ~3,407 parishes.
#' @docType data
#' @usage data(parish_registry)
#' @format A \code{tibble} with the following columns:
#' \describe{
#'   \item{pid}{Parish ID from Skatteverket SFGT (integer, primary key)}
#'   \item{geom_id}{Link to \code{boundaries} (integer). \code{NA} for ~840
#'     non-territorial parishes. When multiple temporal versions exist, this
#'     is the latest version (max end year).}
#'   \item{forkod}{6-digit LKF code from SCB (numeric, 100\% coverage)}
#'   \item{nadkod}{9-digit NAD code from Riksarkivet (numeric, spatial only)}
#'   \item{dedik}{5-digit DDB code (numeric, spatial only)}
#'   \item{dedikscb}{SCB dedication code (numeric, spatial only)}
#'   \item{charid}{ASCII slug from SFGT web pages (character, 97.5\%)}
#'   \item{topo_id}{UUID from Riksarkivet (character, spatial only)}
#'   \item{ref_code}{Riksarkivet reference code (character, spatial only)}
#'   \item{category}{Parish classification: "territorial", "bruk", "garrison",
#'     "hospital", "kbfd", "mosaiska", "slott", "kapell", "straffanstalt",
#'     "regemente", or "other"}
#'   \item{parent_pid}{For non-territorial parishes, the \code{pid} of the
#'     parent territorial parish (61\% coverage, ~510 of 841). Extracted
#'     from SFGT \code{indelning} text ("bildat inom X församling",
#'     "utbruten ur X", "uppgått i X") and name-stripping heuristics.
#'     \code{NA} for territorial parishes and ~330 non-territorial parishes
#'     where no parent could be identified (mostly archaic parishes
#'     merged before 1600). Used by \code{\link{match_units}} as a
#'     fallback to resolve \code{geom_id} for non-territorial parishes.}
#'   \item{county}{County code 1--25 (integer)}
#'   \item{letter}{County letter abbreviation (e.g. "AB", "BD")}
#'   \item{county_name}{County name (e.g. "Norrbottens")}
#'   \item{name}{Canonical short name from SFGT (e.g. "Lövånger")}
#'   \item{name_full}{Official name with suffix from Riksarkivet
#'     (e.g. "Lövångers församling"). \code{NA} for non-spatial.}
#'   \item{socken}{Cleaned SCB-style name from parish_link
#'     (e.g. "Lövångers"). \code{NA} for non-spatial.}
#'   \item{alias}{Alternative names from SFGT redirect entries,
#'     comma-separated (e.g. "Kiruna, Simojärvi"). 19\% coverage.}
#'   \item{name_previous}{Old/previous names parsed from SFGT name-change
#'     notes, comma-separated (e.g. "Elgå"). ~16\% coverage.}
#'   \item{name_scb}{SCB uppercase name from parish_medta.csv
#'     (e.g. "LÖVÅNGER"). Spatial only.}
#'   \item{name_ddb}{Parish name in the Demographic Data Base (DDB), Umeå
#'     University (e.g. "KARESUANDO (ENONTEKI)"). Partial coverage (~62\%).}
#'   \item{start}{Start year from parish_link for spatial parishes (integer).
#'     \code{NA} for non-spatial.}
#'   \item{end}{End year from parish_link for spatial parishes (integer).
#'     \code{NA} for non-spatial.}
#' }
#' @details
#' The registry solves three common problems in historical data linkage:
#' \enumerate{
#'   \item \strong{Code identification}: External datasets use different parish
#'     code systems (dedik, forkod, nadkod, pid). This table has all codes,
#'     so you can join via whichever code your data uses.
#'   \item \strong{Non-territorial parishes}: ~840 parishes (bruk, garrison,
#'     hospital, kbfd, etc.) have no geometry in \code{boundaries}. Joins via
#'     \code{parish_link} silently drop these. The \code{category} and
#'     \code{parent_pid} columns identify them and link them to their parent
#'     territorial parish.
#'   \item \strong{Name variants}: The same parish may appear as different
#'     names in different sources. The wide name columns (\code{name},
#'     \code{name_full}, \code{socken}, \code{alias}, \code{name_previous},
#'     \code{name_scb}, \code{name_ddb}) provide all known variants.
#' }
#' @source Compiled from Skatteverket SFGT, Riksarkivet metadata
#'   (\code{parish_link}), SCB \code{parish_medta.csv}, and DDB parish names.
NULL

#' @name unit_variants
#' @title Name variants knowledge base for unit matching
#' @description A knowledge base of name variants for matching administrative
#'   unit names via \code{\link{match_units}}. Contains canonical names from
#'   \code{\link{boundaries}}, parish registry name columns (alias, previous names,
#'   SCB names, DDB names), auto-generated stripped/stem/historical variants,
#'   and manually curated entries for historical name changes and common short
#'   forms.
#' @docType data
#' @usage data(unit_variants)
#' @format A \code{tibble} with the following columns:
#' \describe{
#'   \item{variant}{Lowercased variant text (lookup key)}
#'   \item{variant_normalized}{Pre-computed normalized form (suffix-stripped,
#'     diacritics folded)}
#'   \item{geom_id}{Links to \code{boundaries} (integer)}
#'   \item{canonical_name}{Official name in \code{boundaries}}
#'   \item{type_id}{Administrative type identifier}
#'   \item{start}{Start year of the geom_id in \code{boundaries}}
#'   \item{end}{End year of the geom_id in \code{boundaries}}
#'   \item{source}{How the variant was generated: \code{"canonical"},
#'     \code{"registry"}, \code{"manual"}, \code{"stripped"}, \code{"stem"},
#'     \code{"historical_spelling"}, or \code{"abbreviation"}}
#'   \item{priority}{Lower values indicate higher confidence (integer)}
#' }
#' @source Built from \code{boundaries}, \code{parish_registry}, and manually
#'   curated historical name mappings. See
#'   \code{data-raw/model/m4b_matching.R}.
NULL

#' @name hist_town
#' @title County towns
#' @description Swedish county towns (residensstäder), as an sf object.
#' @docType data
#' @usage data(hist_town)
#' @format An \code{sf} object with the following columns:
#' \describe{
#'   \item{code}{County code}
#'   \item{town}{County town name}
#'   \item{from}{Start year}
#'   \item{tom}{End year}
#'   \item{geometry}{POINT geometry in SWEREF99 EPSG:3006}
#' }
NULL

#' @name sweden
#' @title Sweden national outline (parish territory)
#' @description The union of all parish polygons in \code{\link{boundaries}},
#'   1600-1990, in EPSG:3006, with holes under 10 km2 filled (small gaps
#'   between parish polygons, which the other types cover). Large lakes that no parish
#'   includes (Vänern, Vättern, Hjälmaren, Storsjön, Siljan, lakes near
#'   Arjeplog) lie outside it. Coverage (the warning in
#'   \code{get_boundaries()}, and \code{?boundaries}) is the share of this
#'   territory that the units of a type cover in a year: whether each parish
#'   can be placed in a unit of that type. Courts of appeal are clipped to it.
#' @docType data
#' @usage data(sweden)
#' @format An \code{sf} object with one row:
#' \describe{
#'   \item{name}{"Sweden (parish territory 1600-1990)"}
#'   \item{geometry}{MULTIPOLYGON geometry}
#' }
NULL

#' @name quality_table
#' @title What the hierarchy rests on, by type and period
#' @description One row per administrative type and 50-year period, saying how much of
#'   the country's parish territory that type covers in the period and what kind of
#'   evidence the membership of parishes in it rests on. The levels
#'   are not equally well founded: county and municipality membership comes from dated
#'   register links, while most parish-to-hundred links are asserted by the register
#'   without dates, and some periods are reached only through a neighbour or by
#'   geometry. Read it before treating a parent as a fact of record.
#' @docType data
#' @usage data(quality_table)
#' @format A tibble:
#' \describe{
#'   \item{parent_type}{Administrative type}
#'   \item{period}{50-year period, e.g. "1800-1849"}
#'   \item{coverage}{Share of the period's parish-years that have a parent of this type}
#'   \item{main_grade}{The grade carrying most of that coverage: "corrected" (a
#'     documented correction with a citation), "sourced" (a dated statement in SCB, SFGT,
#'     the statskalender or a dated register link), "chained_sourced" and "chained"
#'     (reached through one intermediate unit), "asserted" (the register asserts the link
#'     but gives no dates, so the period is inferred) or "derived" (from geometry alone)}
#'   \item{main_share}{Share of the coverage carried by \code{main_grade}}
#'   \item{sourced_or_better}{Share that is corrected, sourced or chained_sourced.
#'     The name encodes an ordering that the held-out measurement does not support:
#'     see the note on \code{grade} in \code{?hierarchy}. Read it as the share
#'     resting on a dated statement, not as a quality score.}
#'   \item{existed}{Share of the period's parish-years in which any unit of the type
#'     existed at all. A type that begins inside a period cannot cover the whole of it:
#'     municipalities begin in 1863, so \code{coverage} for 1850-1899 cannot pass 0.74,
#'     and district courts (\emph{tingsratter}) begin in 1971}
#'   \item{coverage_existed}{\code{coverage} over those years only -- the number to read
#'     when asking whether a column is complete. Municipality 1850-1899 is
#'     \code{coverage} 0.74 and \code{coverage_existed} 1.00: complete from 1863 on.
#'     Where the two are both low the data really are incomplete: county membership before
#'     1700 is 0.24 even in the years county units exist}
#' }
NULL

#' @name known_issues
#' @title Documented deviations that are not fixed
#' @description Findings from the boundary research that are established but that no
#'   source lets us correct, because the sources give an area or a date and not the line
#'   to draw. The clearest case is the Malingsbo/Söderbärke division of 1708-1969: the
#'   two parishes' combined territory is right, the line between them is about 235 km2
#'   out (both churches fall in the polygon labelled Malingsbo), and no map of the
#'   historical division was found, so the polygons are left as the source has them and
#'   the finding is recorded here instead. Where a correction was possible it was made
#'   and does not appear in this table.
#' @docType data
#' @usage data(known_issues)
#' @format A tibble:
#' \describe{
#'   \item{id}{Row id in the research file}
#'   \item{type}{Administrative type}
#'   \item{name}{Unit name as the research names it}
#'   \item{start, end}{Years the finding concerns, where it is dated}
#'   \item{issue}{What is wrong or what happened, in one line}
#'   \item{source}{The source that establishes it}
#'   \item{note}{The evidence, quoted or summarised}
#' }
NULL

#' @name events
#' @title What happened to a unit, and when
#' @description One row per event in a unit's life. \code{\link{relations}} says
#'   that territory passed from one polygon to another; an event says what
#'   happened: the unit was created or abolished, it merged into another, it was
#'   split off, part of its territory was transferred, it was renamed or
#'   recoded, or it changed parent. Renames and recodings are what a user most
#'   often needs and cannot get from the boundaries alone — when Kyrkefalla
#'   became Tibro, when a parish's SCB code changed.
#' @docType data
#' @usage data(events)
#' @format A tibble:
#' \describe{
#'   \item{date}{When it happened (1 January of the year for a change that took
#'     effect with the year, 31 December for an ending)}
#'   \item{unit_id}{The unit, as in \code{\link{boundaries}}}
#'   \item{type_id}{Its type}
#'   \item{event}{"created", "abolished", "merged_into", "split_from",
#'     "part_transferred", "renamed", "recoded" or "parent_changed"}
#'   \item{other_unit}{The other unit, where the event names one}
#'   \item{detail}{The change in words: "Kyrkefalla -> Tibro",
#'     "forkod: 168302 -> 168301", "county: Kopparbergs län -> Örebro län"}
#'   \item{source}{Where it comes from: the unit's period, its names, its codes,
#'     SFGT, SCB, or the memberships}
#' }
NULL
