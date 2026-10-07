You are parsing Swedish historical parish records from "Sveriges församlingar genom tiderna" (SFGT). Read the file {BATCH}. Each line has format: pid=INT|name=STRING|text=STRING. The name field is the parish's naming history (context only). The text field describes which county (län) the parish belonged to over time.

Parse EVERY entry into JSON rows. Each row = one time period in a county.
Output format per row: {"pid": int, "parish_name": "str", "start_year": int|null, "end_year": int|null, "county_name": "str", "partial": bool, "notes": "str"|null}
parish_name: the parish's current name as best you can tell from the name field (or null if NA).

Rules:
- "-1762 Västernorrlands län" = start_year=null, end_year=1762. "1762-06-29- Gävleborgs län" = start_year=1762, end_year=null (ignore month/day).
- partial=false for periods when the whole parish (or its main part) belonged to the county. partial=true for rows that describe only PART of the parish: "del (Y) i Z län", transfers of part of the parish ("1950-01-01 överfört Broby från Torpa" when only a village or area moves), "delar", "en del". A partial transfer is NOT a change of the whole parish's county: keep the whole-parish period running across it and record the transfer as its own row with partial=true, the year, the county, and the details in notes.
- "huvuddelen i X län, del (Y) i Z län" = two rows: X län with partial=false (notes "huvuddelen"), Z län with partial=true (notes naming the part).
- "helt i X län" = one row for X län with notes "helt", partial=false.
- County names exactly as written (e.g. "Närkes och Värmlands län", "Kristianstads län").
- "möjligen" or "har möjligen" = uncertain, put in notes.
- When no explicit start/end is given, infer from context (previous period end + 1, etc.). Keep every period, including the most recent one at the end of the text.
