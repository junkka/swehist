You are parsing Swedish historical parish records from "Sveriges församlingar genom tiderna" (SFGT). Read the file {BATCH}. Each line has format: pid=INT|name=STRING|text=STRING. The name field is the parish's naming history (context only). The text field describes which PASTORAT (pastorship) the parish belonged to over time.

Parse EVERY entry into JSON rows. Each row = one time period.
Output format per row: {"pid": int, "parish_name": "str", "start_year": int|null, "end_year": int|null, "pastorat_name": "str"|null, "is_own": bool, "notes": "str"|null}
parish_name: the parish's current name as best you can tell from the name field (or null if the name field is NA).

Rules:
- "eget", "eget pastorat" or "moderförsamling" = is_own=true and pastorat_name=null (the parish forms its own pastorat; the name is filled in later).
- Dash prefix "-1961" = start_year=null, end_year=1961. Dash suffix "1962-" = start_year=1962, end_year=null.
- "1924-05-01-1948" = start=1924, end=1948 (ignore month/day).
- "1400-talet" = 1400; "medeltiden", "tidigare", "förr" = null.
- Multi-parish pastorat names like "Åby och Bäckebo" = keep as-is.
- Parenthetical notes like "(beslut 1919-06-19)" go in notes. A decision date in parentheses INSIDE a range does not change the range: "1962 (beslut 1961-06-02)-1998 X" = start 1962, end 1998, notes "beslut 1961-06-02".
- "ca 1700", "omkring 1700" = 1700 with notes "ca". "senast 1650" = that year with notes "senast". "före 1650" = end_year 1649 with notes "före 1650".
- A missing space between year and name, e.g. "1962Sjösås", is year 1962, name "Sjösås".
- "därefter" means the next period follows the previous one. When a period has no explicit start, infer it from the previous period's end + 1; when it has no explicit end, infer it from the next period's start - 1 if available.
- Only record periods stated in the text; do not invent any. Keep every period, including the most recent one at the end of the text.
