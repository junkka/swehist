#' SFGT re-parse, step 3: combine the parsed batches into data-raw/sfgt/sfgt_{field}.rda
#'
#' Step 1 (prepare_batches.R) wrote data-raw/sfgt/batches/{field}_batch_NN.txt from the full
#' SFGT texts. Run from the package root: Rscript data-raw/sfgt/combine.R

suppressMessages({library(dplyr); library(jsonlite)})
dir <- "data-raw/sfgt"
dir.create(file.path(dir, "checks"), showWarnings = FALSE)
say <- function(...) message(sprintf(...))
# The check files are published without the SFGT text (look entries up by pid)
write_check <- function(d, file) readr::write_csv(select(d, -any_of("text")), file)

schema <- list(
  pastorat  = c(pid = "integer", parish_name = "character", start_year = "integer",
                end_year = "integer", pastorat_name = "character", is_own = "logical",
                notes = "character"),
  indelning = c(pid = "integer", parish_name = "character", year = "integer",
                event_type = "character", other_parish = "character", notes = "character"),
  lan       = c(pid = "integer", parish_name = "character", start_year = "integer",
                end_year = "integer", county_name = "character", partial = "logical",
                notes = "character")
)
event_types <- c("split_from", "split_off", "merged_into", "incorporated", "formed", "dissolved",
                 "transferred", "became_kapell", "became_annex", "renamed", "other")

read_batches <- function(field){
  f <- sort(list.files(file.path(dir, "batches"), sprintf("^%s_batch_\\d+\\.txt$", field), full.names = TRUE))
  l <- unlist(lapply(f, readLines, encoding = "UTF-8"))
  m <- regmatches(l, regexec("^pid=(\\d+)\\|name=(.*?)\\|text=(.*)$", l))
  tibble(pid = as.integer(sapply(m, `[`, 2)), namn = sapply(m, `[`, 3), text = sapply(m, `[`, 4))
}

read_results <- function(field){
  f <- sort(list.files(file.path(dir, "results"), sprintf("^%s_result_\\d+\\.json$", field), full.names = TRUE))
  cols <- schema[[field]]
  bind_rows(lapply(f, function(x){
    d <- as_tibble(fromJSON(x, simplifyDataFrame = TRUE))
    for (k in names(cols)) {
      if (!k %in% names(d)) d[[k]] <- NA
      d[[k]] <- methods::as(d[[k]], cols[[k]])
    }
    d[names(cols)]
  }))
}

years_in <- function(x) as.integer(unlist(regmatches(x, gregexpr("(?<!\\d)1[0-9]{3}(?!\\d)", x, perl = TRUE))))

pid_lookup <- readr::read_csv("data-raw/pid_lookup.csv", col_types = "icci", na = "")

out <- list()
for (field in names(schema)) {
  inp <- read_batches(field)
  d <- read_results(field)
  say("── %s: %d rows for %d pids (input %d pids)", field, nrow(d), n_distinct(d$pid), nrow(inp))

  # 1. Every input pid parsed, no unknown pids
  missing <- setdiff(inp$pid, d$pid); extra <- setdiff(d$pid, inp$pid)
  if (length(missing) || length(extra))
    stop(sprintf("%s: %d pids missing, %d unknown pids", field, length(missing), length(extra)))

  ychk <- d %>% left_join(inp, by = "pid") %>% mutate(row = row_number())
  if (field == "indelning") {
    bad_type <- d %>% filter(!event_type %in% event_types)
    if (nrow(bad_type)) stop(sprintf("indelning: unknown event types %s",
                                     paste(unique(bad_type$event_type), collapse = ", ")))
    yv <- ychk %>% transmute(row, pid, text, year = year)
  } else {
    # Periods must be ordered
    rev <- d %>% filter(!is.na(start_year), !is.na(end_year), start_year > end_year)
    say("  periods with start > end: %d", nrow(rev))
    write_check(rev, file.path(dir, "checks", paste0(field, "_reversed.csv")))
    yv <- bind_rows(ychk %>% transmute(row, pid, text, year = start_year, what = "start"),
                    ychk %>% transmute(row, pid, text, year = end_year, what = "end"))
  }

  # 2. Every year in the output appears in the text (or is one off a stated
  #    year: inferred starts and ends, "före 1650" -> 1649)
  yv <- yv %>% filter(!is.na(year)) %>% rowwise() %>%
    mutate(in_text = any(abs(years_in(text) - year) <= 1)) %>% ungroup()
  inv <- yv %>% filter(!in_text)
  say("  output years not in the text (±1): %d of %d", nrow(inv), nrow(yv))
  write_check(inv, file.path(dir, "checks", paste0(field, "_years_not_in_text.csv")))

  # 3. The latest year in the text is reached by the output (a lost last
  #    period is what the 200-character truncation did)
  last <- inp %>% rowwise() %>% mutate(text_max = suppressWarnings(max(years_in(text)))) %>% ungroup() %>%
    filter(is.finite(text_max)) %>% select(pid, text_max, text)
  got <- if (field == "indelning") d %>% group_by(pid) %>% summarise(out_max = suppressWarnings(max(year, na.rm = TRUE))) else
    d %>% group_by(pid) %>% summarise(out_max = suppressWarnings(max(c(start_year, end_year), na.rm = TRUE)))
  short <- last %>% left_join(got, by = "pid") %>% filter(!is.finite(out_max) | out_max < text_max - 1)
  say("  pids whose output stops before the latest year in the text: %d", nrow(short))
  write_check(short, file.path(dir, "checks", paste0(field, "_short.csv")))

  # is_own rows: name the pastorate after the parish, as in March, so that
  # step4d can map pastorate names from other parishes' texts to it
  if (field == "pastorat") {
    reg_name <- pid_lookup$name[match(d$pid, pid_lookup$pid)]
    ok_name <- !is.na(d$parish_name) & !grepl("[0-9]", d$parish_name) & nchar(d$parish_name) <= 40
    d <- d %>% mutate(pastorat_name = ifelse(is_own & is.na(pastorat_name),
                                             ifelse(ok_name, parish_name, reg_name), pastorat_name))
  }
  if (field == "lan") say("  partial rows: %d", sum(d$partial, na.rm = TRUE))

  # 4. Compare with the March parse
  march_f <- file.path(dir, "march2026", sprintf("sfgt_%s.rda", field))   # not in the repository
  if (file.exists(march_f)) {
    e <- new.env(); load(march_f, envir = e)
    old <- get(paste0("sfgt_", field), e)
    key_cols <- intersect(setdiff(names(schema[[field]]), c("parish_name", "notes", "partial")), names(old))
    sig <- function(x) x %>% select(all_of(key_cols)) %>% arrange(across(everything())) %>%
      group_by(pid) %>% summarise(sig = paste(do.call(paste, c(across(-any_of("pid")), sep = "/")), collapse = "; "))
    cmp <- full_join(sig(old), sig(d), by = "pid", suffix = c("_march", "_new")) %>%
      left_join(inp %>% select(pid, text), by = "pid")
    changed <- cmp %>% filter(is.na(sig_march) | is.na(sig_new) | sig_march != sig_new)
    say("  vs March: %d rows -> %d rows; %d of %d pids changed", nrow(old), nrow(d), nrow(changed), nrow(cmp))
    write_check(changed, file.path(dir, "checks", paste0(field, "_changed_vs_march.csv")))
  }

  d <- as.data.frame(d)
  assign(paste0("sfgt_", field), d)
  save(list = paste0("sfgt_", field), file = file.path(dir, sprintf("sfgt_%s.rda", field)))
  out[[field]] <- d
}

# 5. The audit sample (110 entries of the March parse coded against the source
#    text, 18 wrong):
#    show the new parse of every entry for review
audit_file <- file.path(dir, "checks", "audit_sample_march.csv")
if (file.exists(audit_file)) {
  audit <- readr::read_csv(audit_file, show_col_types = FALSE)
  rows <- bind_rows(lapply(seq_len(nrow(audit)), function(i){
    a <- audit[i, ]; d <- out[[a$field]]; r <- d[d$pid == a$pid, ]
    txt <- read_batches(a$field)$text[read_batches(a$field)$pid == a$pid]
    fmt <- switch(a$field,
      pastorat  = sprintf("%s-%s %s%s", r$start_year, r$end_year, r$pastorat_name, ifelse(r$is_own, " (own)", "")),
      indelning = sprintf("%s %s %s", r$year, r$event_type, r$other_parish),
      lan       = sprintf("%s-%s %s%s", r$start_year, r$end_year, r$county_name, ifelse(r$partial, " (partial)", "")))
    tibble(field = a$field, pid = a$pid, march_correct = a$correct, error_type = a$error_type,
           comment = a$comment, text = if (length(txt)) txt else NA_character_,
           new_parse = paste(fmt, collapse = " | "))
  }))
  write_check(rows, file.path(dir, "checks", "audit_sample_new_parse.csv"))
  say("Audit sample: %d entries written to checks/audit_sample_new_parse.csv", nrow(rows))
}
