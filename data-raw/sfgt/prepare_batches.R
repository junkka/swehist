## SFGT re-parse, step 1: batch files from the full texts (2026-09)
##
## The March 2026 parse read texts cut at 200 characters (swe-parish's old
## parse_parish.R truncated every field), so 228 entries lost their latest
## history. This writes new batch files from the full texts in the current
## swe-parish for_hist, with pids mapped to the frozen pids used everywhere in
## swehist (data-raw/pid_lookup.csv, same mapping as step7).
## Line format: pid=INT|name=STRING|text=STRING (as in March).

suppressMessages(library(dplyr))
load(file.path("..", "swe-parish", "data", "for_hist.rda"))

# Same alias step and frozen pid mapping as the earlier pipeline's parish registry
if (!"alias" %in% names(for_hist)) {
  aliases <- for_hist %>%
    filter(is.na(link2), !is.na(link1), is.na(fodelsebok), is.na(lan), is.na(namn),
           is.na(husforhorslangd), is.na(indelning), is.na(pastorat), is.na(pastoratskod),
           (forkod %% 100) == 0, !is.na(ovrigt)) %>% select(name, link1, pid)
  for_hist <- tibble::as_tibble(for_hist) %>% filter(!pid %in% aliases$pid)
}
pid_lookup <- readr::read_csv("data-raw/pid_lookup.csv", col_types = "icci", na = "")
key <- function(n, c, f) paste(n, c, as.integer(f), sep = "|")
for_hist$pid <- pid_lookup$pid[match(key(for_hist$name, for_hist$charid, for_hist$forkod),
                                     key(pid_lookup$name, pid_lookup$charid, pid_lookup$forkod))]
stopifnot(!anyNA(for_hist$pid), !anyDuplicated(for_hist$pid))

clean <- function(x) gsub("[|\r\n]+", " ", trimws(x))
sizes <- c(pastorat = 480, indelning = 382, lan = 460)
for (field in names(sizes)) {
  d <- for_hist %>% filter(!is.na(.data[[field]]), nchar(trimws(.data[[field]])) > 0) %>% arrange(pid)
  lines <- sprintf("pid=%d|name=%s|text=%s", d$pid, ifelse(is.na(d$namn), "NA", clean(d$namn)), clean(d[[field]]))
  chunks <- split(lines, ceiling(seq_along(lines) / sizes[[field]]))
  for (k in seq_along(chunks))
    writeLines(chunks[[k]], sprintf("data-raw/sfgt/batches/%s_batch_%02d.txt", field, k - 1), useBytes = TRUE)
  message(sprintf("%s: %d entries, %d batches, longest text %d chars, %d over 200",
                  field, nrow(d), length(chunks), max(nchar(d[[field]])), sum(nchar(d[[field]]) > 200)))
}
