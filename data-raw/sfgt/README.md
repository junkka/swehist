# Parsed SFGT texts

m3s_sfgt.R reads `sfgt_pastorat.rda` and `sfgt_lan.rda` from this folder. They are the
pastorat and län texts of *Sveriges församlingar genom tiderna* (SFGT, Skatteverket 1989),
turned into dated rows by a large language model. `m5_events.R` reads `sfgt_indelning.rda`
(splits and mergers) for the parish events.

The parsed rows feed two kinds of links in `hierarchy` (`source` = `"sfgt"` or
`"sfgt"`): Pastorship -> Parish and County -> Parish. m3s_sfgt.R keeps an SFGT
link only when the parish lies inside the parent unit.

## How the files were made (September 2026)

1. `prepare_batches.R` wrote `batches/{field}_batch_NN.txt` (4,862 entries, 11 batches)
   from the full SFGT texts, with pids mapped to the frozen pids of
   `data-raw/pid_lookup.csv`. The batch files hold the SFGT texts verbatim and are not
   published; neither is the `text` column of the check files. Look an entry up in SFGT
   by its pid (`pid_lookup.csv` gives the name and the SFGT page anchor, `charid`).
2. Each batch was parsed by the language model Claude Opus 5.5 (Anthropic), one run per
   batch, on 2026-09-23, with the prompts in `prompts/{field}.md` + `prompts/common.md`.
   Output: `results/{field}_result_NN.json`. A first run was interrupted; the results come
   from a second, complete run.
3. `combine.R` combined the results, ran the checks below and wrote `sfgt_*.rda`. The check
   tables it writes to `checks/` hold source text and are not in the repository.

## An earlier parse

An earlier parse (March 2026, Claude Opus 4.6) read texts cut at 200
characters, so entries lost their latest history. A sample of 110 entries of that parse,
drawn at random within each field, was coded against the source text (the coding was also
done with Claude Opus 5.5) and 18 were wrong. The prompts were corrected for those error types:

- indelning: `split_from` is "utbruten ur X" (this parish broke away from X); `split_off`
  is "utbrutet/utbrutna X" without "ur" (X broke away from this parish). The March prompt
  mapped both to `split_from`.
- pastorat: decision dates in parentheses inside a range, "ca", "senast", "före", and
  a missing space as in "1962Sjösås".
- lan: a transfer of part of a parish is a row with `partial = TRUE`, not a period in
  which the parish belonged to that county. m3s_sfgt.R drops partial rows.

## Checks

- Every input pid is in the output; no period has start > end.
- Years in the output that do not appear in the text (±1): 33 pastorat, 23 indelning,
  1 lan. On inspection all are correct readings of garbled years in the source
  ("19051940" = 1905-1940, "1.655" = 1655).
- Entries whose output stops before the latest year in the text: 11, 25, 4. On inspection
  these are years in notes, archive remarks or name changes, not lost periods.
- The new parse of the 110 audit entries: all 18 former
  errors are now right, and none of the 92 correct entries got worse. These are the
  entries the prompts were corrected against, so this is not an error rate.
- Of the split and merger events in
  1601-1990 that `relations` confirms, the direction agrees for 98% (79% in the March
  parse).
