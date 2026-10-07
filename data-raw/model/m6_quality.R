#' m6: The quality table — what a user can trust, per type and period
#'
#' Every membership row carries the grade of the evidence behind it (m3b).
#'
#' Input:  data-raw/model/out/{m2,m3b,m4}.rds
#' Output: data-raw/model/out/m6.rds and out/pkg/data/quality.rda

source("data-raw/build_helpers.R")
source("data-raw/model/model_helpers.R")
m1 <- readRDS("data-raw/model/out/m1.rds")
m2 <- readRDS("data-raw/model/out/m2.rds")
m3b <- readRDS("data-raw/model/out/m3b.rds")
t0 <- timer_start("m6: quality table")
atoms <- st_drop_geometry(m2$atoms)
mb <- m3b$membership

GRADE_ORDER <- c("corrected", "sourced", "chained_sourced", "chained", "asserted", "derived")
periods <- list(c(1600, 1649), c(1650, 1699), c(1700, 1749), c(1750, 1799), c(1800, 1849),
                c(1850, 1899), c(1900, 1949), c(1950, 1990))

# Parish-years available in each period (the denominator)
atom_years <- function(p){
  a <- atoms %>% mutate(s = pmax(start, p[1]), e = pmin(end, p[2])) %>% filter(s <= e)
  sum(a$e - a$s + 1)
}
# Membership-years of a type in a period, by grade
mb_years <- mb %>% select(atom_id, parent_type, grade, start, end)

quality <- bind_rows(lapply(periods, function(p){
  denom <- atom_years(p)
  mb_years %>% mutate(s = pmax(start, p[1]), e = pmin(end, p[2])) %>% filter(s <= e) %>%
    mutate(yrs = e - s + 1) %>%
    group_by(parent_type, grade) %>% summarise(yrs = sum(yrs), .groups = "drop") %>%
    group_by(parent_type) %>%
    mutate(period = sprintf("%d-%d", p[1], p[2]),
           coverage = sum(yrs) / denom,
           share = yrs / sum(yrs)) %>% ungroup()
})) %>%
  mutate(grade = factor(grade, levels = GRADE_ORDER)) %>%
  arrange(parent_type, period, grade)

# A type that did not exist for part of a period drags its own coverage down, and the number then
# reads as missing data when it is history: municipalities begin in 1863, so 1850-1899 can never be
# more than 0.74, and tingsratter begin in 1971, so district_court 1950-1990 can never pass 0.49.
# `existed` is the share of the period's parish-years in which any unit of the type existed at all,
# and `coverage_existed` is the coverage over those years only -- the number a reader wants when
# asking "is the municipality column complete in 1880".
exist_tbl <- bind_rows(lapply(periods, function(p){
  yrs <- p[1]:p[2]
  ut <- m1$units %>% transmute(type_id, s = as.integer(format(start_date, "%Y")),
                               e = as.integer(format(end_date, "%Y")))
  live <- ut %>% group_by(type_id) %>%
    summarise(years = sum(vapply(yrs, function(y) any(s <= y & e >= y), logical(1))), .groups = "drop")
  live %>% transmute(parent_type = type_id, period = sprintf("%d-%d", p[1], p[2]),
                     existed = years / length(yrs))
}))

# One row per type and period: coverage, and the grade that carries most of it
summary_tbl <- quality %>% group_by(parent_type, period) %>%
  summarise(coverage = first(coverage),
            main_grade = as.character(grade[which.max(share)]),
            main_share = max(share),
            sourced_or_better = sum(share[grade %in% c("corrected", "sourced", "chained_sourced")]),
            .groups = "drop") %>%
  left_join(exist_tbl, by = c("parent_type", "period")) %>%
  mutate(existed = ifelse(is.na(existed), 1, existed),
         coverage_existed = pmin(1, ifelse(existed > 0, coverage / existed, NA_real_))) %>%
  arrange(parent_type, period)

print(as.data.frame(summary_tbl %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

# The same for the documentation: one line per type
by_type <- quality %>% group_by(parent_type, grade) %>%
  summarise(yrs = sum(yrs), .groups = "drop") %>% group_by(parent_type) %>%
  mutate(share = yrs / sum(yrs)) %>% ungroup() %>%
  select(-yrs) %>% tidyr::pivot_wider(names_from = grade, values_from = share, values_fill = 0)
print(as.data.frame(by_type %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)
timer_end(t0)

message("  --- m6 checks ---")
check(nrow(quality) > 0, "quality table built")
check(all(quality$coverage <= 1.0001), "coverage is a share")
# ---- Known issues: the researched deviations that are documented but not fixed ---------------
# A boundary_note correction records what a source says about a unit's territory. Where the note
# says the data are wrong and no source gives the line to draw instead (the Malingsbo/Söderbärke
# division of 1708-1969, the pre-1929 Värmland territories), the finding is shipped so a user can
# see it rather than having to rediscover it. Rows that record agreement are left out.
corr <- read_corrections()
known_issues <- corr %>% filter(kind == "boundary_note", !is.na(value),
                                !grepl("^no error", value, ignore.case = TRUE)) %>%
  transmute(id, type = coalesce(type_id, "parish"), name, start, end, issue = value,
            source, note) %>% arrange(type, name)
message(sprintf("  known_issues: %d documented deviations (of %d boundary notes)",
                nrow(known_issues), sum(corr$kind == "boundary_note")))

pkg_data <- "data-raw/model/out/pkg/data"
if (dir.exists(pkg_data)) {
  quality_table <- summary_tbl
  save(quality_table, file = file.path(pkg_data, "quality_table.rda"), compress = "gzip")
  save(known_issues, file = file.path(pkg_data, "known_issues.rda"), compress = "gzip")
  message("  Saved quality_table.rda and known_issues.rda into the package tree")
}
saveRDS(list(quality = quality, summary = summary_tbl, by_type = by_type,
             known_issues = known_issues),
        "data-raw/model/out/m6.rds")
message("  Saved data-raw/model/out/m6.rds")
