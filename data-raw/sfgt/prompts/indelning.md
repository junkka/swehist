You are parsing Swedish historical parish records from "Sveriges församlingar genom tiderna" (SFGT). Read the file {BATCH}. Each line has format: pid=INT|name=STRING|text=STRING. The name field is the parish's naming history (context only). The text field describes administrative changes of THIS parish: splits, merges, formations, dissolutions.

Parse EVERY entry into JSON rows. Each row = one event.
Output format per row: {"pid": int, "parish_name": "str", "year": int|null, "event_type": "str", "other_parish": "str"|null, "notes": "str"|null}
parish_name: the parish's current name as best you can tell from the name field (or null if NA).

Event types (direction matters — the subject is always THIS parish):
- "split_from" = THIS parish broke away from another parish X: "utbruten ur X", "utbrutet ur X", "utbruten från X", "bildad genom utbrytning ur X". other_parish = X.
- "split_off" = another parish X broke away from THIS parish: "utbrutet X", "utbrutna X", "utbrutits X", "X utbrutet härifrån", "avskilt X" — i.e. "utbrutet/utbrutna" WITHOUT "ur"/"från" directly before X. other_parish = X. Example: "1733 utbrutet Bäckebo" = split_off, other_parish "Bäckebo".
- "merged_into" = uppgått i (THIS parish merged into X)
- "incorporated" = införlivat (X was incorporated into THIS parish)
- "formed" = bildad / bildat
- "dissolved" = upplöst
- "transferred" = överfört (territory transferred)
- "became_kapell" = kapellförsamling
- "became_annex" = annexförsamling
- "renamed" = namnbyte / ändrat namn
- "other" = anything that doesn't fit above

Rules:
- "1.655" is a typo for 1655. "omkring 1580" = year 1580, notes "omkring". "ca 1580" likewise with notes "ca".
- Decision dates in parentheses "(beslut 1640-02-17)" go in notes; they do not change the event year.
- Multiple events in one entry = multiple rows; keep every event, including the last ones in the text.
- "utbruten ur X och Y" = one event with other_parish "X och Y".
- "införlivat A, B och C" = separate events for each parish, or one with other_parish "A, B och C" if done simultaneously.
- "kbfd" = kyrkobokföringsdistrikt (church registration district).
