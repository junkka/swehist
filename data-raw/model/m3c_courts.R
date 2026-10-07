#' m3c: The court layer (domsaga / rådhusrätt / tingsrätt) from Sveriges statskalender
#'
#' Why the statskalender. The courts are the worst level in the earlier build (64% on the first
#' gold sample).
#'
#' Input:  runeberg.org page texts (cached in data-raw/model/out/cache/statskalender/,
#' reused from a local cache where already downloaded)
#' data-raw/model/data/statskalender_courts_<year>.csv (written here, tracked;
#' re-parsed when missing, or when STK_REPARSE=1)
#' data-raw/model/out/m1.rds, out/m1b.rds, data-raw/data/Tbl_topografi_rel.csv,
#' data-raw/county_meta.csv
#' Output: data-raw/model/out/m3c.rds: list(court_units, court_evidence, court_names,
#' sources, report) and the report CSVs in data-raw/model/out/

source("data-raw/build_helpers.R")
source("data-raw/model/model_helpers.R")
t0 <- timer_start("m3c: courts from Sveriges statskalender")

STK_CACHE <- "data-raw/model/out/cache/statskalender"
STK_ALT   <- "data-raw/validation/external/cache/statskalender"   # read-only, already downloaded
STK_DATA  <- "data-raw/model/data"
dir.create(STK_DATA, showWarnings = FALSE, recursive = TRUE)

## ---- 0. Editions ---------------------------------------------------------------------
## Volumes digitised on runeberg.org, at 5-20 year steps, with the years around the reforms
## that matter for the courts: 1947/1950 (the 1948 domsaga and tingslag reform) and
## 1970/1972 (tingsrätterna, 1971-01-01).
editions <- tribble(
  ~year, ~work,           ~from, ~to,  ~parser,  ~cut,                            ~cut_which, ~head_extra,
  1866,  "sonkal/1866",     147,  152, "person", "Kommendant\\s+o\\.\\s+Direktör", "first",   NA_character_,
  1881,  "statskal/1881",   108,  111, "person", "Fångv\\S*\\s*-?\\s*[Ss]taten",   "last",    NA_character_,
  1905,  "statskal/1905",   116,  120, "person", "Fångv\\S*\\s*-?\\s*[Ss]taten",   "last",    "Fångv",
  1915,  "statskal/1915",   154,  161, "inv",     NA_character_,                   NA,        NA_character_,
  1925,  "statskal/1925",   169,  177, "inv",     NA_character_,                   NA,        NA_character_,
  1931,  "statskal/1931",   166,  174, "inv",     NA_character_,                   NA,        NA_character_,
  1940,  "statskal/1940",   200,  208, "inv",     NA_character_,                   NA,        NA_character_,
  1947,  "statskal/1947",   250,  258, "inv",     NA_character_,                   NA,        NA_character_,
  1950,  "statskal/1950",   261,  268, "seat",    NA_character_,                   NA,        NA_character_,
  1955,  "statskal/1955",   287,  294, "seat",    NA_character_,                   NA,        NA_character_,
  1963,  "statskal/1963",   281,  288, "seat",    NA_character_,                   NA,        NA_character_,
  1970,  "statskal/1970",   303,  320, "modern",  NA_character_,                   NA,        NA_character_,
  1972,  "statskal/1972",   306,  322, "modern",  NA_character_,                   NA,        NA_character_,
  1978,  "statskal/1978",   376,  393, "modern",  NA_character_,                   NA,        NA_character_,
  1984,  "statskal/1984",   285,  304, "modern",  NA_character_,                   NA,        NA_character_)
ED <- editions$year
next_ed <- setNames(c(ED[-1], NA), ED)
prev_ed <- setNames(c(NA, ED[-length(ED)]), ED)
# year_start()/year_end() of model_helpers.R, but NA-safe (an open bound has no year)
d_start <- function(y) as.Date(ifelse(is.na(y), NA_character_, sprintf("%04d-01-01", as.integer(y))))
d_end   <- function(y) as.Date(ifelse(is.na(y), NA_character_, sprintf("%04d-12-31", as.integer(y))))

## ---- 1. Page text --------------------------------------------------------------------
stk_dir <- function(work){
  n <- gsub("/", "_", work)
  alt <- file.path(STK_ALT, n)
  if (dir.exists(file.path(alt, "Pages"))) return(alt)
  d <- file.path(STK_CACHE, n)
  if (dir.exists(file.path(d, "Pages"))) return(d)
  dir.create(STK_CACHE, showWarnings = FALSE, recursive = TRUE)
  z <- paste0(d, ".zip")
  message(sprintf("    download runeberg %s", work))
  utils::download.file(sprintf("https://runeberg.org/download.pl?mode=txtzip&work=%s", work),
                       z, mode = "wb", quiet = TRUE)
  utils::unzip(z, exdir = d)
  d
}
stk_url <- function(work, page) sprintf("https://runeberg.org/%s/%04d.html", work, page)

# One row per printed line, runeberg markup and HTML removed
stk_lines <- function(work, from, to){
  d <- stk_dir(work)
  bind_rows(lapply(from:to, function(p){
    f <- file.path(d, "Pages", sprintf("%04d.txt", p))
    if (!file.exists(f)) return(NULL)
    t <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    t <- gsub("\\[-.*?-\\]", "", t, perl = TRUE)          # runeberg deletions
    t <- gsub("\\{\\+(.*?)\\+\\}", "\\1", t, perl = TRUE) # runeberg insertions
    t <- gsub("<[^>]+>", "", t)
    t <- gsub("&amp;", "&", t, fixed = TRUE)
    t <- gsub("\r", "", t, fixed = TRUE)
    tibble(page = p, text = strsplit(t, "\n")[[1]])
  }))
}

# Running heads, page numbers and section numbers ("[190-191]", "559[649-650]", "83")
stk_is_head <- function(x, extra = NULL){
  y <- trimws(x)
  ex <- if (is.null(extra)) rep(FALSE, length(y)) else grepl(extra, y, perl = TRUE)
  keep <- grepl("(?i)l[äa]n|domsag|häradshövding|hovrätt|hofrätt|tingsrätt|rådhusrätt|under",
                y, perl = TRUE)
  y == "" | grepl("^[\\[\\(]?[0-9IlJ ,.\\-—–^¾°SQabc]*[\\]\\)J]?\\s*[0-9]*$", y, perl = TRUE) |
    grepl("^\\S{0,4}\\[[^\\]]{1,15}[\\]J]\\s*\\S{0,4}$", y, perl = TRUE) |
    (!keep & grepl("^\\S{0,5}\\[[^\\]]{1,15}[\\]J]\\s+[A-ZÅÄÖ][^\\[\\]]{2,60}$", y, perl = TRUE)) |
    (!keep & grepl("^[A-ZÅÄÖ][^\\[\\]]{2,60}\\s+\\S{0,5}\\[[^\\]]{1,15}[\\]J]\\S{0,5}$", y, perl = TRUE)) |
    ex | grepl(paste0("^(Ecklesiastikstaten|Länsstyrelserna|Domsagor och häradshövdingar\\.?|",
                      "Häradshöfding ?ar\\.?|RäradshÖfdingar\\.?|Underrätter|",
                      "Underrätter o\\. rådhusrätter|Tingsrätter|Tingsrätterna|Justitiestaten)\\.?$"), y)
}

# County header lines: "i Upsala Län,", "3. Upsala län.", "STOCKHOLMS LÄN", "344g Västmanlands län"
stk_county_line <- function(x){
  y <- trimws(gsub("^\\[[^\\]]*\\]\\s*", "", x, perl = TRUE))
  y <- sub("^\\W{0,3}[A-Za-z]{2,3}\\.\\s*[\\d,.]+[.:]?\\s*", "", y, perl = TRUE)   # "Inv. 96,094."
  y <- sub("^\\d{1,4}\\s?[a-z]?\\s+", "", y, perl = TRUE)                          # "344g "
  y <- sub("^[^A-ZÅÄÖ]{0,3}\\d{1,4}\\s?[a-z]?[\\]J]\\s*", "", y, perl = TRUE)          # "£217d] "
  y <- gsub("(?<=[A-Za-zåäö])0|0(?=[A-Za-zåäö])", "o", y, perl = TRUE)
  m <- str_match(y, paste0("^(?:[^A-Za-zÅÄÖ]{0,4}|[iJ][^A-Za-zÅÄÖ]{0,3}|(?:\\d{1,2}|[IU]\\d?)\\s*\\S?\\s+)",
                           "([A-ZÅÄÖWC][^,;:()]{2,34}?)\\s+(?:L|I|l|\\|)(?:[äaAüuå][nND]|aa)[,.;\"'-]?\\s*(?:\\(.*|f\\(.*|[^A-Za-z]*)$"))[, 2]
  m <- gsub("[^A-Za-zåäöÅÄÖ. -]", "", m)
  caps <- str_match(trimws(x), "^(?:\\[[^\\]]{0,12}[\\]J]\\s*)?([A-ZÅÄÖ][A-ZÅÄÖ \\-]{2,32})\\s+(LÄN|STAD)\\s*$")
  m <- ifelse(is.na(m) & !is.na(caps[, 2]),
              paste0(substr(caps[, 2], 1, 1), tolower(substring(caps[, 2], 2))), m)
  ifelse(is.na(m), NA_character_, trimws(gsub("\\s+", " ", m)))
}

# A section as one string, with page (¶) and county (§C:) markers, dehyphenated
stk_stream <- function(L, fixes = character(), head_extra = NULL){
  L <- L %>% filter(!stk_is_head(text, head_extra))
  for (k in names(fixes)) L$text <- gsub(k, fixes[[k]], L$text, fixed = TRUE)
  cty <- stk_county_line(L$text)
  L$text <- ifelse(is.na(cty), L$text, paste0(" \u00a7C:", cty, "\u00a7 "))
  L$text <- paste0(ifelse(L$page != lag(L$page, default = -1), sprintf(" \u00b6%04d\u00b6 ", L$page), ""),
                   L$text)
  s <- paste(L$text, collapse = "\n")
  s <- gsub("([a-zåäöé])-\\s*\n\\s*(?!(och|o\\.)\\s)([a-zåäö])", "\\1\\3", s, perl = TRUE)
  s <- gsub("([a-zåäöé])-(?!(och|o\\.)\\s)([a-zåäö])", "\\1\\3", s, perl = TRUE)
  s <- gsub("[ \t]*\n[ \t]*", " ", s)
  s <- gsub("\\s+", " ", s)
  s <- gsub("(?<=[a-zåäö]) [0O]\\.? (?=[A-ZÅÄÖ])", " o. ", s, perl = TRUE)      # OCR 0. for o.
  s <- gsub(" oeh ", " och ", s, fixed = TRUE)
  s <- gsub(" (\\(ho|\\(l:o|cLo|d\\.o|d:0|d\\s:o)(?=[\\s.,;(])", " d:o", s, perl = TRUE)
  s <- gsub("([a-zåäö])(h:d|t:lag|sk:lag)\\b", "\\1 \\2", s, perl = TRUE)        # "Fjäreh:d"
  for (k in names(fixes)) s <- gsub(k, fixes[[k]], s, fixed = TRUE)
  s
}
stk_unmark  <- function(x) trimws(gsub("\\s+", " ", gsub("\u00b6\\d{4}\u00b6", " ", x)))
stk_page_at <- function(s, pos, default){
  a <- str_match_all(substr(s, 1, pos), "\u00b6(\\d{4})\u00b6")[[1]]
  if (nrow(a)) as.integer(a[nrow(a), 2]) else default
}
stk_mark_at <- function(s, pos, mark){
  a <- str_match_all(substr(s, 1, pos), paste0("\u00a7", mark, ":([^\u00a7]+)\u00a7"))[[1]]
  if (nrow(a)) trimws(a[nrow(a), 2]) else NA_character_
}

## Units of a list such as "Sjuhundra, Lyhundra härader, Frötuna o. Länna, Bro o. Vätö
## skeppslag": split on , ; samt jemte; kind = the trailing word. " o. " / "och" inside a
## piece is kept (a tingslag of two härader is printed the same way as one of two parishes);
## the matching step below splits it when the whole piece is not a unit.
stk_kinds <- paste0("härader|härad|hårad|härads|h:ds|h:dt|h:d|tingslaget|tingslag|tingslags|",
                    "t:lagt|t:lag|t:laget|tdag|tdaget|skeppslag|sk:lagt|sk:lag|lappmarker|",
                    "lappmarks|lappmark|bergslag|d:o|d\\.o|d:0|mot|möt|mots")
stk_units <- function(x){
  x <- chartr("{}", "()", x)
  x <- gsub("\\([^()]*\\)?", " ", x)
  x <- gsub("(?<!\\S)(Norra|Södra|Östra|Västra|Vestra|Westra|Öfre|Övre|Nedre)\\s+(och|oeh|o\\.)\\s+(Norra|Södra|Östra|Västra|Vestra|Westra|Öfre|Övre|Nedre)\\s+(\\S+)",
            "\\1 \\4, \\3 \\4", x, perl = TRUE)
  x <- gsub("(?<!\\S)(Öster|Wester|Vester|Väster)-?\\s+(och|o\\.)\\s+(Öster|Wester|Vester|Väster)-\\s*(\\S+)",
            "\\1-\\4, \\3-\\4", x, perl = TRUE)
  x <- gsub("\\s+(och\\s+)?återstående\\s+delen?\\s+a[fv]\\s+", ", en del af ", x, perl = TRUE)
  x <- gsub(",?\\s*(innefattande|hvilka|hvilken|vilka|med [A-ZÅÄÖ]\\w+ (skärgård|stad)).*?(?=[,;]|$)",
            "", x, perl = TRUE, ignore.case = TRUE)
  x <- gsub("\\b(med|jemte)\\s+(?=[A-ZÅÄÖ])", ", ", x, perl = TRUE)
  p <- trimws(unlist(strsplit(x, "[,;]|\\bsamt\\b|\\bjemte\\b")))
  p <- gsub("^(och|o\\.)\\s+", "", p)
  p <- gsub("^[\u00b6\\d\\s]+", "", p, perl = TRUE)
  p <- gsub("[.:)\\}\\]]+$", "", p, perl = TRUE)
  p <- trimws(p[nchar(p) > 1])
  kind <- tolower(str_match(p, paste0("\\s(", stk_kinds, ")\\.?$"))[, 2])
  name <- p
  for (k in 1:2) name <- trimws(sub(paste0("\\s(", stk_kinds, ")\\.?$"), "", name, ignore.case = TRUE))
  name <- gsub("^(en del af|en del av|del af|del av)\\s+", "", name)
  partial <- grepl("^(en )?del a[fv] ", p)
  keep <- nchar(name) > 1 & grepl("^[A-ZÅÄÖW]", name)
  tibble(unit = name, unit_kind = kind, partial = partial)[keep, ]
}
# The unit list ends where the häradshövding's name begins ("... Häverödal. Sjögren, Fritjof,")
stk_cut_person <- function(x){
  if (is.na(x)) return(x)
  x <- sub("\\.\\s+[A-ZÅÄÖ][A-Za-zåäöé:.-]+,\\s*[A-ZÅÄÖ].*$", "", x, perl = TRUE)
  x <- sub("\\s(Tingsdomare|Tingssekreterare|Häradshövding|Vakant|Fiskal|Särskild|Biträdande|Tings\\s?sekret).*$",
           "", x, perl = TRUE)
  trimws(x)
}
# Units read from the court's own name, when no list is printed ("utgör 1 t:lag"):
# "Norra och Södra Tjusts härads domsaga" is two härader, "Aska, Dals och Bobergs" three.
stk_name_units <- function(x){
  x <- sub("\\s*(domsagan|domsaga|tingsrätten|tingsrätt|rådhusrätten|rådhusrätt)$", "", x)
  x <- sub("\\s+härade?r?s?$", "", x)
  x <- gsub(paste0("(?<!\\S)(Norra|Södra|Östra|Västra|Övre|Nedre|Nedan|Ovan)\\s+(och|o\\.)\\s+",
                   "(Norra|Södra|Östra|Västra|Övre|Nedre|Nedan|Ovan)\\s+(\\S+)"),
            "\\1 \\4, \\3 \\4", x, perl = TRUE)
  x <- gsub("\\s+och\\s+", ", ", x)
  stk_units(x)
}
# The canonical name of the one hovrätt a header names, or NA when it names none or two
hov_one <- function(x){
  k <- chartr("\u00e5\u00e4\u00f6", "aao", tolower(x))
  hits <- c("Svea", "Göta", "Skåne och Blekinge", "Västra Sverige", "Nedre Norrland",
            "Övre Norrland")[c(grepl("svea", k), grepl("gota", k), grepl("skane", k),
                               grepl("vastra sverige", k), grepl("nedre norrland", k),
                               grepl("ovre norrland", k))]
  if (length(hits) == 1) hits else NA_character_
}
stk_fill_ditto <- function(x){
  for (i in seq_along(x)) if (!is.na(x[i]) && x[i] %in% c("d:o", "d.o", "d:0") && i > 1) x[i] <- x[i - 1]
  x
}

## OCR fixes found by reading the parsed lists against the page images (exact strings)
stk_fix <- list(
  `1866` = c("häradf " = "härad, ", "1-auras" = "Faurås", "OnsjöA" = "Onsjö,", "Ilimhle" = "Himble",
             "JÄforrø Ångermanland,)" = "(Norra Ångermanland,)", "Sunnerbo eho" = "Sunnerbo d:o",
             "Jerfsö do" = "Jerfsö d:o", "Hedesundib- o." = "Hedesunda o.", "Sefcedes" = "Sevedes",
             "Bedicägs" = "Redvägs", "Barn c," = "Barne,", "Tireta" = "Tveta", "Säfcedals" = "Säfvedals",
             "Wartojta" = "Wartofta", "Sötlra" = "Södra", "Weslra" = "Westra", "Bönnebcrgs" = "Rönnebergs",
             "Sne friuge" = "Snefringe", "Skin skatteberg s" = "Skinskattebergs", "Fjiire" = "Fjäre",
             "Frlinghundra" = "Erlinghundra", "Banderyds" = "Danderyds", "Baga, Akers" = "Daga, Åkers",
             "Bals, Bobergs" = "Dals, Bobergs", "Wülåttinge" = "Willåttinge", "Bönö" = "Rönö",
             "Wester-Bekarne" = "Wester-Rekarne", "Basbo" = "Rasbo", "Bannemora" = "Dannemora",
             "Bamsbergs" = "Ramsbergs", "Garpe?ibergs" = "Garpenbergs", "Sundhorns" = "Sundborns",
             "Bagunda" = "Ragunda", "Bef sunds" = "Refsunds", "Bödöns" = "Rödöns", "Båneå" = "Råneå",
             "Ijöfångers" = "Löfångers", "Stängenäs" = "Stångenäs", "Solenäs" = "Sotenäs",
             "Inlandsßödre" = "Inlands Södre", "Sandals" = "Sundals", "Welte" = "Wette",
             "Forssa d:o V v. Hudiksvall)" = "Forssa d:o (adr. Hudiksvall)", "(d:u)" = "(d:o)",
             "f. 14;0Lysings" = "f. 14; Lysings", "Skånings. Wilske7 Walle" = "Skånings, Wilske, Walle",
             "Carl^hamn, Ilobyj." = "Carlshamn, Hoby).", "6'udhents" = "Gudhems",
             "Ytler-Tjurbo" = "Ytter-Tjurbo", "Arbrå, Jerfsö d-o" = "Arbrå, Jerfsö d:o"),
  `1881` = c("Ök?iebo" = "Öknebo", "Bönö" = "Rönö", "Öster-Bekarne" = "Öster-Rekarne", "Basbo" = "Rasbo",
             "Vester-Bekarne" = "Vester-Rekarne", "Barnsbergs" = "Ramsbergs", "Hainsele" = "Ramsele",
             "Bödöns" = "Rödöns", "Bagunda" = "Ragunda", "Belsunds" = "Refsunds", "Båneå" = "Råneå",
             "Bedvägs" = "Redvägs", "Hahnstads" = "Halmstads", "Hoks" = "Höks", "Säjvedals" = "Säfvedals",
             "Lanehär ad;" = "Lane härad;", "Boslags" = "Roslags", "Akers" = "Åkers", "Asunda" = "Åsunda",
             "?ned Haparanda" = "med Haparanda", "Tima," = "Tuna,", " As, Gäsene" = " Ås, Gäsene",
             "Häradshöj'dinge-Embe tet" = "Häradshöfdinge-Embetet", "{Falu domsaga^" = "(Falu domsaga,)",
             "o U†neå" = "Umeå"),
  `1905` = c("Åska" = "Aska", "Hemmings" = "Memmings", "Bisinge" = "Risinge", "Tjellmo" = "Tjällmo",
             "Bedvägs" = "Redvägs", "Vätle" = "Vättle", "Anneiund" = "Annelund",
             "Fin-spånga" = "Finspånga", "Karl Gustaf$" = "Karl Gustafs", "Juckasjärvi" = "Jukkasjärvi",
             "Gellivare" = "Gällivare", "Qville" = "Kville", "As härad" = "Ås härad",
             "Skins,tattebergs" = "Skinnskattebergs", "Nedan-Süjans" = "Nedansiljans",
             "Ofvan-Siljans" = "Ofvansiljans", "Elf dals" = "Elfdals",
             "Dorn haf v':s" = "Domhafvandens"))
stk_fix_inv <- c("Frös åkers" = "Frösåkers", "t: lag" = "t:lag", "t dag" = "t:lag", "t.lag" = "t:lag",
                 "tdag" = "t:lag", "t:las," = "t:lag,", "k:d," = "h:d,", "h:dtingsst." = "h:d, tingsst.",
                 "Opptmda" = "Oppunda", "Arvidsjatirs" = "Arvidsjaurs", "Jokk7)iokks" = "Jokkmokks",
                 "Korpilo??ibolo" = "Korpilombolo", "Karesua7ido lapp771 arks" = "Karesuando lappmarks",
                 "Frösdkers" = "Frösåkers", "Pinspånga" = "Finspånga", "Bräbygdens" = "Bråbygdens",
                 "Hah?istads" = "Halmstads", "Hi?nle" = "Himle")
stk_fix_seat <- c("t dag" = "t:lag", "t.lag" = "t:lag", "tdag" = "t:lag", "tilag" = "t:lag",
                  "t:laget" = "t:lag", "tdaget" = "t:lag", "Tingst:n" = "Tingsst:n")

## ---- 2. Four layouts -----------------------------------------------------------------

## 2a. Person-led (1866, 1881, 1905):
##     "Person, titles, 51; f. 13; (X domsaga,) Frösåkers härad; Närdinghundra d:o (adr. ...)"
parse_courts_person <- function(year, work, from, to, cut = NULL, cut_which = "last",
                                head_extra = NULL){
  L <- stk_lines(work, from, to)
  s <- stk_stream(L, stk_fix[[as.character(year)]], head_extra)
  s <- sub("^.*?Justitie-?\\s*[Ss]taten [åIi] landet\\.?", "", s)
  if (!is.null(cut) && !is.na(cut)) {
    m <- gregexpr(cut, s)[[1]]
    if (m[1] > 0) s <- substr(s, 1, (if (cut_which == "last") m[length(m)] else m[1]) - 1)
  }
  s <- gsub("\\b([abc])\\)\\s*under\\s+(K\\.|Kongl\\.|Kungl\\.)\\s*([^;]{3,40}?)\\s*;",
            " \u00a7H:\\3\u00a7 ", s, perl = TRUE)
  # an entry ends at the address, or at "härader." / "d:o." before the next person
  s <- gsub("\\b((?:härader|härad|d:o|tingslag|skeppslag|lappmarker|Mot)\\s*\\.)\\s+(?=[A-ZÅÄÖ\u00b6])",
            "\\1 \u00a7E\u00a7 ", s, perl = TRUE)
  s <- gsub("\\((?:[^()]*?adr[^()]*?|[^()]*adress[^()]*|d:o|d\\.o)\\)\\s*[.,]?", " \u00a7E\u00a7 ", s, perl = TRUE)
  tok <- strsplit(s, "\u00a7")[[1]]
  county <- NA_character_; hov <- NA_character_; rows <- list(); buf <- ""
  for (t in tok) {
    if (startsWith(t, "C:")) { county <- sub("^C:", "", t); buf <- ""; next }
    if (startsWith(t, "H:")) { hov <- sub("^H:", "", t); buf <- ""; next }
    if (t == "E") {
      rows[[length(rows) + 1]] <- tibble(hovratt = hov, county_raw = county, entry = buf)
      buf <- ""; next
    }
    buf <- paste(buf, t)
  }
  e <- bind_rows(rows)
  pg <- from; pages <- integer(nrow(e))
  for (i in seq_len(nrow(e))) {
    m <- str_match(e$entry[i], "^\\s*\u00b6(\\d{4})\u00b6")[, 2]
    pages[i] <- if (!is.na(m)) as.integer(m) else pg
    all <- str_match_all(e$entry[i], "\u00b6(\\d{4})\u00b6")[[1]][, 2]
    if (length(all)) pg <- as.integer(all[length(all)])
  }
  e$page <- pages
  e$entry <- stk_unmark(e$entry)
  e$entry <- gsub("[;,]?\\s*hvilken\\s+s\\S+\\s.*?(Emb\\S*|Embetet)\\s*[,;]?", " ", e$entry, perl = TRUE)
  # the units start after the years: 1866 "51; f. 13;", 1881/1905 "f. 40; 79;"; else after "Vakant"
  # "51; f. 13;" (1866) or "f. 40; 79;" / "f. 43; (78) 91;" (1881, 1905)
  anchor <- if (year < 1870) "(?<![A-Za-zåäö])o?f\\.\\s*\"?[^\\s;:]{1,5}\\s*[;:,]" else
    "(?<![A-Za-zåäö])[fi£]\\.?\\s*[^\\s;]{1,4}\\s*[;:]\\s*(\\([^)]{1,6}\\)\\s*)?[^\\s;,]{1,4}\\s*[;:,]"
  m  <- regexpr(anchor, e$entry, perl = TRUE)
  mv <- regexpr("Vakant\\s*[;,]", e$entry)
  e$units <- ifelse(m > 0, substring(e$entry, m + attr(m, "match.length")),
                    ifelse(mv > 0, substring(e$entry, mv + attr(mv, "match.length")), NA_character_))
  e <- e %>% filter(!is.na(units), nchar(trimws(units)) > 1)
  e$units <- sub("^[\\s;:,.'\"]*((QQ|\\(\\]Q|Q|o)\\s*[;:,.]\\s*)?", "", e$units, perl = TRUE)
  e$units <- sub("^[^A-ZÅÄÖ(\\{\\[]*", "", e$units, perl = TRUE)
  dm <- str_match(e$units, "^[\\(\\{\\[]?\\s*([^(){}]{3,60}?)[\\s,.;\\^]*[\\)\\}]")
  has_name <- !is.na(dm[, 1]) & (grepl("[DB]om", dm[, 2], ignore.case = TRUE) |
                                   grepl("^[\\(\\{]", e$units, perl = TRUE))
  e$court_name <- ifelse(has_name, trimws(gsub("[,.;]+$", "", dm[, 2])), NA_character_)
  e$units <- ifelse(has_name, substring(e$units, nchar(dm[, 1]) + 1), e$units)
  e$units <- sub("^[\\s,.;)}\\]]+", "", e$units, perl = TRUE)
  # "Oppunda o. Villåttinge härads domsaga" names the domsaga and its härader at once
  hd <- str_match(e$units, "^\\s*([^,;()]+?)\\s+härads\\s+domsaga\\b")
  e$court_name <- ifelse(is.na(e$court_name) & !is.na(hd[, 1]), trimws(hd[, 1]), e$court_name)
  e$units <- ifelse(!is.na(hd[, 1]), paste(hd[, 2], "härad"), e$units)
  e$court_name <- gsub("\\bBomsaga\\b", "Domsaga", e$court_name)
  e$court_name <- sub("\\s*,?\\s*som utgör.*$", "", e$court_name)
  e$court_no <- seq_len(nrow(e))
  out <- bind_rows(lapply(seq_len(nrow(e)), function(i){
    u <- stk_units(e$units[i])
    if (!nrow(u)) u <- tibble(unit = NA_character_, unit_kind = NA_character_, partial = FALSE)
    bind_cols(e[rep(i, nrow(u)), c("hovratt", "county_raw", "court_no", "court_name", "page")], u)
  }))
  out$unit_kind <- stk_fill_ditto(out$unit_kind)
  out %>% mutate(year = year, url = stk_url(work, page), court_kind = "domsaga",
                 seat = NA_character_, unit_source = "list",
                 source_text = e$entry[match(court_no, e$court_no)], .before = 1)
}

## 2b. Domsaga-led with population (1915, 1925, 1931, 1940, 1947):
##     "X domsaga (Seat), 32,950 inv., omfattar 2 t:lag: A t:lag, tingsst. P; ... — 1,600 kr."
##     "utgör 1 t:lag" means there is no list: the units are then read from the name.
parse_courts_inv <- function(year, work, from, to, ...){
  L <- stk_lines(work, from, to)
  s <- stk_stream(L, stk_fix_inv)
  s <- gsub(paste0("under\\s+(Svea|Göta|Skåne och Blekinge|Västra Sverige|Nedre Norrland|",
                   "Övre Norrland)\\S*\\s+hovrätt(en)?\\S*"), " \u00a7H:\\1\u00a7 ", s, perl = TRUE)
  s <- gsub("under\\s+[Hh]ovrätten\\s+(över|för)\\s+([^.;:\u00a7\u00b6]{3,30}?)\\s*(?=[.;:\u00a7\u00b6])",
            " \u00a7H:\\2\u00a7 ", s, perl = TRUE)
  # not preceded by a letter: a dropped running head or a stray "39." can leave ";" or "]"
  # in front of the name, which an explicit list of sentence-end characters would miss
  m <- gregexpr(paste0("(?<![A-Za-zÅÄÖåäö-])([A-ZÅÄÖ][^.;:\u00a7\u00b6»—]{1,80}?\\sdomsaga)",
                       "\\s*(\\([^)]{1,40}\\))?\\s*[,.»]?\\s*\\d[\\d.,;]*\\s*inv"), s, perl = TRUE)[[1]]
  if (m[1] < 0) return(NULL)
  st <- as.integer(m); en <- c(st[-1] - 1, nchar(s))
  seg    <- substring(s, st, en)
  county <- vapply(st, function(p) stk_mark_at(s, p, "C"), "")
  hov    <- vapply(st, function(p) stk_mark_at(s, p, "H"), "")
  pages  <- vapply(st, function(p) stk_page_at(s, p, from), 1L)
  seg    <- stk_unmark(gsub("\u00a7[^\u00a7]*\u00a7", " ", seg))
  nm     <- trimws(str_match(seg, "^(.*?domsaga)")[, 2])
  seat   <- str_match(seg, "^.*?domsaga\\s*\\(([^)]{1,40})\\)")[, 2]
  bind_rows(lapply(seq_along(seg), function(i){
    om <- str_match(seg[i], paste0("omfatta[rn]\\s*\\d*\\s*(?:t:lag|tingslag)\\s*:\\s*(.*?)",
                                   "(\\s+[—-]\\s*\\d|\\s\\d[\\d,.]*\\s*kr\\.|$)"))[, 2]
    om <- stk_cut_person(om)
    if (!is.na(om) && nzchar(om)) {
      pcs <- sub(",?\\s*tingsst\\S*\\s.*$", "", trimws(strsplit(om, ";")[[1]]))
      u <- bind_rows(lapply(pcs, stk_units)); src <- "list"
    } else {
      u <- stk_name_units(nm[i]); src <- "name"
    }
    if (!nrow(u)) u <- tibble(unit = NA_character_, unit_kind = NA_character_, partial = FALSE)
    tibble(year = year, url = stk_url(work, pages[i]), page = pages[i], hovratt = hov[i],
           county_raw = county[i], court_no = i, court_name = nm[i], court_kind = "domsaga",
           seat = seat[i], u, unit_source = src, source_text = substr(seg[i], 1, 300))
  }))
}

## 2c. Domsaga-led with seat, no population (1950, 1955, 1963):
##     "X domsaga (Seat). 2 t:lag: A t:lag, tingsst. P; B t:lag, tingsst. Q." / "Tingsst. P"
parse_courts_seat <- function(year, work, from, to, ...){
  L <- stk_lines(work, from, to)
  s <- stk_stream(L, stk_fix_seat)
  s <- gsub("(?i)DOMSAGOR OCH\\s+HÄRADSHÖVDINGAR UNDER\\s+([^.;:\u00a7\u00b6]{3,40}?)\\s+HOVRÄTT(EN)?",
            " \u00a7H:\\1\u00a7 ", s, perl = TRUE)
  s <- gsub("(?i)under\\s+[Hh]ovrätten\\s+(över|för)\\s+([^.;:\u00a7\u00b6]{3,30}?)\\s*(?=[.;:\u00a7\u00b6])",
            " \u00a7H:\\2\u00a7 ", s, perl = TRUE)
  m <- gregexpr("(?<![A-Za-zÅÄÖåäö-])([A-ZÅÄÖ][^.;:\u00a7\u00b6»—]{1,80}?\\sdomsaga)\\s*\\(([^)]{1,40})\\)",
                s, perl = TRUE)[[1]]
  if (m[1] < 0) return(NULL)
  st <- as.integer(m); en <- c(st[-1] - 1, nchar(s))
  seg    <- substring(s, st, en)
  county <- vapply(st, function(p) stk_mark_at(s, p, "C"), "")
  hov    <- vapply(st, function(p) stk_mark_at(s, p, "H"), "")
  pages  <- vapply(st, function(p) stk_page_at(s, p, from), 1L)
  seg    <- stk_unmark(gsub("\u00a7[^\u00a7]*\u00a7", " ", seg))
  nm     <- trimws(str_match(seg, "^(.*?domsaga)")[, 2])
  seat   <- str_match(seg, "^.*?domsaga\\s*\\(([^)]{1,40})\\)")[, 2]
  bind_rows(lapply(seq_along(seg), function(i){
    body <- sub("^.*?domsaga\\s*\\([^)]{1,40}\\)\\s*[.,]?\\s*", "", seg[i])
    body <- stk_cut_person(body)
    body <- sub("\\s[A-ZÅÄÖ][a-zåäöé]+(-[A-ZÅÄÖ][a-zåäöé]+)?,\\s+[A-ZÅÄÖ].*$", "", body)
    om <- str_match(body, "\\d*\\s*t:lag\\s*:\\s*(.*)$")[, 2]
    if (!is.na(om) && nzchar(om)) {
      pcs <- sub(",?\\s*tingsst\\S*\\s.*$", "", trimws(strsplit(om, ";")[[1]]))
      u <- bind_rows(lapply(pcs, stk_units)); src <- "list"
    } else {
      u <- stk_name_units(nm[i]); src <- "name"
    }
    if (!nrow(u)) u <- tibble(unit = NA_character_, unit_kind = NA_character_, partial = FALSE)
    tibble(year = year, url = stk_url(work, pages[i]), page = pages[i], hovratt = hov[i],
           county_raw = county[i], court_no = i, court_name = nm[i], court_kind = "domsaga",
           seat = seat[i], u, unit_source = src, source_text = substr(seg[i], 1, 300))
  }))
}

## 2d. Line-led modern lists (1970 underrätter = domsagor + rådhusrätter; 1972, 1978, 1984
##     tingsrätter). After 1971 the calendar no longer prints what a court is made of: its
##     domkrets is a list of kommuner in SFS, so these editions give names, seats and the
##     hovrätt only.
parse_courts_modern <- function(year, work, from, to, ...){
  L <- stk_lines(work, from, to)
  L$text <- trimws(gsub("\\s+", " ", L$text))
  L <- L %>% filter(text != "")
  # the section header is broken over two or three lines ("UNDERRÄTTER UNDER HOVRÄTTEN" /
  # "FÖR NEDRE NORRLAND"): an all-capitals line that names a hovrätt but not which one is
  # joined with the line below until it does
  for (pass in 1:3) {
    caps <- L$text == toupper(L$text) & grepl("HOVRÄTT", L$text, fixed = TRUE) &
      grepl("UNDER", L$text, fixed = TRUE) & nchar(L$text) < 60
    j <- which(caps & is.na(vapply(L$text, hov_one, "", USE.NAMES = FALSE)))
    j <- j[j < nrow(L)]
    if (!length(j)) break
    L$text[j] <- paste(L$text[j], L$text[j + 1]); L <- L[-(j + 1), ]
  }
  county <- NA_character_; hov <- NA_character_
  rows <- list(); cur <- NULL; n <- 0L
  flush <- function() { if (!is.null(cur)) { rows[[length(rows) + 1]] <<- cur; cur <<- NULL } }
  for (i in seq_len(nrow(L))) {
    t <- sub("^\\[[^\\]]{1,12}[\\]J]\\s*", "", L$text[i], perl = TRUE)
    t <- trimws(sub("^\\d{2,4}[a-z]?([—–-]\\d{2,4}[a-z]?)*\\s*", "", t))
    if (grepl("^(Ju|Underrätter|Tingsrätter|Tingsrätterna|Hovrätterna|Justitiestaten)\\b", t) &&
        nchar(t) < 40) next
    hm <- str_match(t, "(?i)(UNDERRÄTTER|TINGSRÄTTER)\\s+UNDER\\s+(.+?)\\s*$")
    if (!is.na(hm[1, 1])) { if (!is.na(hov_one(hm[1, 3]))) hov <- trimws(hm[1, 3]); next }
    hm2 <- str_match(t, "(?i)^Under\\s+((Svea|Göta)\\s+hovrätt|[Hh]ovrätten (för|över)[^\\[]{3,40}?)\\s*(\\[.*)?$")
    # a running head naming two hovrätter ("Under Hovrätten för Nedre Norrland o. Hovrätten
    # för Övre Norrland") says nothing about which one this page's courts belong to
    if (!is.na(hm2[1, 1])) { if (!is.na(hov_one(hm2[1, 2]))) hov <- trimws(hm2[1, 2]); next }
    t <- sub("^[^A-ZÅÄÖ]{0,3}\\d{1,4}\\s?[a-z]?[\\]J]\\s*", "", t, perl = TRUE)      # "£217d] "
    cm <- str_match(t, "^([A-ZÅÄÖ][A-ZÅÄÖ \\-]{3,40})\\s+(LÄN|STAD)\\s*$")
    if (!is.na(cm[1, 1])) {
      county <- paste0(substr(cm[1, 2], 1, 1), tolower(substring(cm[1, 2], 2))); next
    }
    cl <- stk_county_line(t)
    if (!is.na(cl) && nchar(t) < 40) { county <- cl; next }
    nmm <- str_match(t, "^([A-ZÅÄÖ][^,;()\\[\\]]{2,60}?\\s(domsaga|tingsrätt|rådhusrätt))\\s*$")
    rm2 <- str_match(t, "^Rådhusrätten\\s+i\\s+([A-ZÅÄÖ][A-Za-zÅÄÖåäöé \\-]{2,40}?)\\s*$")
    if (!is.na(nmm[1, 1])) {
      flush(); n <- n + 1L
      k <- c(domsaga = "domsaga", "tingsrätt" = "tingsratt", "rådhusrätt" = "radhusratt")[nmm[1, 3]]
      cur <- tibble(year = year, url = stk_url(work, L$page[i]), page = L$page[i], hovratt = hov,
                    county_raw = county, court_no = n, court_name = nmm[1, 2],
                    court_kind = unname(k), seat = NA_character_, units = "", source_text = t)
      next
    }
    if (!is.na(rm2[1, 1])) {
      flush(); n <- n + 1L
      cur <- tibble(year = year, url = stk_url(work, L$page[i]), page = L$page[i], hovratt = hov,
                    county_raw = county, court_no = n, court_name = paste(rm2[1, 2], "rådhusrätt"),
                    court_kind = "radhusratt", seat = rm2[1, 2], units = "", source_text = t)
      next
    }
    if (is.null(cur)) next
    tm <- str_match(t, "^Ting i\\s+(.+?)\\.?$")
    if (!is.na(tm[1, 1]) && is.na(cur$seat)) cur$seat <- tm[1, 2]
    if (grepl("tingslag", t)) cur$units <- paste(cur$units, t)
    if (grepl("^(Lagman|Häradshövding|Borgmästare|Rådmän|Rådman|Tingsdomare|Tingsfiskal|Vakant)", t)) flush()
  }
  flush()
  e <- bind_rows(rows)
  if (!nrow(e)) return(NULL)
  bind_rows(lapply(seq_len(nrow(e)), function(i){
    ul <- trimws(e$units[i])
    if (nzchar(ul)) {
      pcs <- sub(":\\s*ting i.*$", "", trimws(strsplit(ul, ";")[[1]]))
      u <- bind_rows(lapply(pcs, stk_units)); src <- "list"
    } else {
      x <- sub("\\s*(domsaga|tingsrätt|rådhusrätt)$", "", e$court_name[i])
      u <- if (e$court_kind[i] == "domsaga") stk_name_units(e$court_name[i]) else
        if (e$court_kind[i] == "radhusratt")
          tibble(unit = x, unit_kind = "stad", partial = FALSE) else
          tibble(unit = NA_character_, unit_kind = NA_character_, partial = FALSE)
      src <- if (e$court_kind[i] == "tingsratt") "none" else "name"
    }
    if (!nrow(u)) u <- tibble(unit = NA_character_, unit_kind = NA_character_, partial = FALSE)
    bind_cols(e[rep(i, nrow(u)), setdiff(names(e), "units")], u, tibble(unit_source = src))
  }))
}

PARSERS <- list(person = parse_courts_person, inv = parse_courts_inv,
                seat = parse_courts_seat, modern = parse_courts_modern)
STK_COLS <- c("year", "url", "page", "hovratt", "county_raw", "court_no", "court_name",
              "court_kind", "seat", "unit", "unit_kind", "partial", "unit_source", "source_text")

## ---- 3. Parse every edition (cached as tracked CSVs) ---------------------------------
reparse <- nzchar(Sys.getenv("STK_REPARSE"))
stk <- bind_rows(lapply(seq_len(nrow(editions)), function(i){
  e <- editions[i, ]
  f <- file.path(STK_DATA, sprintf("statskalender_courts_%d.csv", e$year))
  if (file.exists(f) && !reparse) {
    x <- suppressWarnings(read_csv(f, show_col_types = FALSE,
                                   col_types = cols(partial = "l", .default = "c"))) %>%
      mutate(year = as.integer(year), page = as.integer(page), court_no = as.integer(court_no))
    message(sprintf("  %d  %5d rows (cached)", e$year, nrow(x)))
    return(x)
  }
  x <- PARSERS[[e$parser]](e$year, e$work, e$from, e$to, cut = e$cut, cut_which = e$cut_which,
                           head_extra = if (is.na(e$head_extra)) NULL else e$head_extra)
  x <- x[, STK_COLS]
  write_csv(x, f, na = "")
  message(sprintf("  %d  %5d rows (parsed %s p%d-%d)", e$year, nrow(x), e$work, e$from, e$to))
  x
}))

stk <- stk %>% mutate(year = as.integer(year), page = as.integer(page),
                      court_no = as.integer(court_no))

## ---- 4. Canonical counties and courts of appeal --------------------------------------
## The counties as the statskalender prints them, with their SCB code. data-raw/data/
## county_meta.csv carries the post-1997 names (Skåne for code 12, Västra Götalands for 14),
## which never appear in these volumes, so the historical names are listed here.
county_meta <- tribble(
  ~code, ~name,
  1L, "Stockholms",      3L, "Uppsala",          4L, "Södermanlands",  5L, "Östergötlands",
  6L, "Jönköpings",      7L, "Kronobergs",       8L, "Kalmar",         9L, "Gotlands",
 10L, "Blekinge",       11L, "Kristianstads",   12L, "Malmöhus",      13L, "Hallands",
 14L, "Göteborgs och Bohus", 15L, "Älvsborgs",  16L, "Skaraborgs",    17L, "Värmlands",
 18L, "Örebro",         19L, "Västmanlands",    20L, "Kopparbergs",   21L, "Gävleborgs",
 22L, "Västernorrlands", 23L, "Jämtlands",      24L, "Västerbottens", 25L, "Norrbottens")
fold <- function(x) chartr("\u00e5\u00e4\u00f6\u00e9\u00fc", "aaoeu", tolower(trimws(x)))
# the spelling rules of R/match_parishes.R normalize_historical_spelling(), the part that
# matters for administrative names (Vermlands -> Varmlands, Elfsborgs -> Alvsborgs, ...)
hist_spell <- function(x){
  x <- tolower(x)
  x <- gsub("fv", "v", x, fixed = TRUE); x <- gsub("qv", "kv", x, fixed = TRUE)
  x <- gsub("\\bhv", "v", x, perl = TRUE); x <- gsub("\\bth", "t", x, perl = TRUE)
  x <- gsub("sch", "sk", x, fixed = TRUE); x <- gsub("dh", "d", x, fixed = TRUE)
  x <- gsub("dt\\b", "t", x, perl = TRUE); x <- gsub("gh\\b", "g", x, perl = TRUE)
  x <- gsub("w", "v", x, fixed = TRUE);    x <- gsub("q", "k", x, fixed = TRUE)
  x <- gsub("\\bvest(er|ra)", "v\u00e4st\\1", x, perl = TRUE)
  x <- gsub("ij", "i", x, fixed = TRUE)
  x <- gsub("([a-z\u00e5\u00e4\u00f6])f([\u00e5\u00e4\u00f6aeiouy])", "\\1v\\2", x, perl = TRUE)
  x <- gsub("\\bcarl", "karl", x, perl = TRUE); x <- gsub("\\bchrist", "krist", x, perl = TRUE)
  x <- gsub("ki\u00f6", "k\u00f6", x, fixed = TRUE)
  x <- gsub("\\bjern", "j\u00e4rn", x, perl = TRUE); x <- gsub("\\belf", "\u00e4lv", x, perl = TRUE)
  x <- gsub("\\belm", "\u00e4lm", x, perl = TRUE)
  x <- gsub("\\bupsala", "uppsala", x, perl = TRUE)
  x <- gsub("\\bjemt", "j\u00e4mt", x, perl = TRUE); x <- gsub("\\bnerike", "n\u00e4rke", x, perl = TRUE)
  x
}
norm_key <- function(x){
  x <- fold(hist_spell(x))
  x <- gsub("[^a-z0-9 ]", " ", x)
  trimws(gsub("\\s+", " ", x))
}
# county_raw is OCR ("Kron ober rs", "Chri.tianstads", "Göteborgs o. Bohus"): nearest canonical
canon_county <- local({
  cn  <- county_meta$name
  key <- norm_key(gsub(" och ", " o ", cn))
  # the OCR breaks county names into pieces ("Kron ober rs", "J Cal mar"), so the spaces are
  # taken out on both sides before the distance is measured
  tight <- gsub(" ", "", key)
  function(x){
    k <- norm_key(gsub("\\s+o(ch)?\\.?\\s+", " o ", x))
    out <- rep(NA_character_, length(k))
    ok <- !is.na(k) & nzchar(k)
    if (any(ok)) {
      kt <- gsub("(^| )[a-z]( |$)", " ", k[ok])            # a stray one-letter piece
      kt <- gsub(" ", "", kt)
      d <- pmin(adist(k[ok], key, ignore.case = TRUE), adist(kt, tight, ignore.case = TRUE))
      j <- max.col(-d, ties.method = "first")
      best <- d[cbind(seq_len(nrow(d)), j)]
      out[ok] <- ifelse(best <= pmax(2, floor(0.35 * nchar(kt))), cn[j], NA_character_)
    }
    out
  }
})
canon_hovratt <- function(x){
  k <- norm_key(x)
  out <- rep(NA_character_, length(k))
  out[grepl("svea", k)]               <- "Svea"
  out[grepl("gota", k)]               <- "Göta"
  out[grepl("skane", k)]              <- "Skåne och Blekinge"
  out[grepl("vastra sverige", k)]     <- "Västra Sverige"
  out[grepl("nedre norrland", k)]     <- "Nedre Norrland"
  out[grepl("ovre norrland", k)]      <- "Övre Norrland"
  out
}
stk <- stk %>%
  mutate(county = canon_county(county_raw), hovratt = canon_hovratt(hovratt)) %>%
  group_by(year) %>% arrange(court_no, .by_group = TRUE) %>%
  tidyr::fill(hovratt, .direction = "down") %>% ungroup()      # "HOVRÄTTEN" split over two lines
message(sprintf("  %d parsed rows, %d editions; county resolved %.1f%%, hovrätt %.1f%%",
                nrow(stk), n_distinct(stk$year), 100 * mean(!is.na(stk$county)),
                100 * mean(!is.na(stk$hovratt))))

## ---- 5. Court records per edition ----------------------------------------------------
## A court printed across a page break can appear twice; records of one edition with the
## same name are one record, and their unit lists are merged.
## drop_s = TRUE also drops the genitive s, so that the statskalender's "Rådhusrätten i
## Norrköping" meets the register's "Norrköpings rådhusrätt". It is used only when comparing
## with m1, never for grouping the records of one edition (there it would merge namesakes).
court_key <- function(x, drop_s = FALSE){
  k <- norm_key(x)
  k <- gsub(paste0("\\b(domsagan|domsaga|domsagor|tingsratten|tingsratt|radhusratten|radhusratt|",
                   "harads|harad|harader|lans|tingslag|tingslaget|skeppslag|mot|mots)\\b"),
            " ", k, perl = TRUE)
  k <- gsub("\\boch\\b|\\bo\\b", ",", k)
  parts <- strsplit(k, ",")
  vapply(parts, function(p){
    p <- trimws(p); if (drop_s) p <- sub("s$", "", p)
    p <- p[nzchar(p)]
    paste(sort(unique(p)), collapse = "+")
  }, "")
}
## OCR repairs to the printed court names (data/court_name_fixes.csv). A name the scan broke
## ("V cii erb er g slags domsaga") neither matches the register's unit nor joins its own other
## editions into one court, so it would become a separate unnamed court in every edition. Applied
## before ckey, which is what groups editions and matches units.
fx <- read.csv("data-raw/model/data/court_name_fixes.csv", comment.char = "#",
               fileEncoding = "UTF-8", stringsAsFactors = FALSE)
if (nrow(fx)) {
  i <- match(trimws(stk$court_name), trimws(fx$printed))
  n_drop <- sum(!is.na(i) & fx$action[i] == "drop")
  n_ren <- sum(!is.na(i) & fx$action[i] == "name")
  stk <- stk[is.na(i) | fx$action[i] != "drop", ]
  i <- match(trimws(stk$court_name), trimws(fx$printed))
  stk$court_name[!is.na(i) & fx$action[i] == "name"] <-
    fx$fix[i[!is.na(i) & fx$action[i] == "name"]]
  message(sprintf("  court name fixes: %d renamed, %d rows dropped", n_ren, n_drop))
}

rec <- stk %>%
  filter(!is.na(court_name) | !is.na(unit)) %>%
  mutate(ckey = ifelse(is.na(court_name), paste0("#", year, "#", court_no), court_key(court_name)))
rec_meta <- rec %>%
  group_by(year, ckey) %>%
  summarise(court_name = first(na.omit(court_name)),
            court_kind = first(court_kind), hovratt = first(na.omit(hovratt)),
            county = first(na.omit(county)), seat = first(na.omit(seat)),
            page = min(page), url = first(url), court_no = min(court_no),
            unit_source = if (any(unit_source == "list")) "list" else first(unit_source),
            .groups = "drop")
## The kind word is printed once for a run ("Frösåkers, Närdinghundra härader"), so it is
## filled backwards and then forwards inside each court record.
rec_units <- rec %>% filter(!is.na(unit)) %>%
  mutate(ord = row_number()) %>%
  distinct(year, ckey, unit, unit_kind, partial, unit_source, .keep_all = TRUE) %>%
  arrange(year, ckey, ord) %>% group_by(year, ckey) %>%
  tidyr::fill(unit_kind, .direction = "up") %>%
  tidyr::fill(unit_kind, .direction = "down") %>% ungroup() %>%
  select(year, ckey, ord, unit, unit_kind, partial, unit_source)
message(sprintf("  court records: %s",
                paste(sprintf("%d:%d", editions$year,
                              vapply(editions$year, function(y) sum(rec_meta$year == y), 1L)),
                      collapse = " ")))

## ---- 6. m1 units to link to ----------------------------------------------------------
m1  <- readRDS("data-raw/model/out/m1.rds")
m1b <- readRDS("data-raw/model/out/m1b.rds")
u1  <- m1$units
rec1 <- st_drop_geometry(m1$records) %>% distinct(topo_id, unit_id, type_id)
# county of an m1 hundred, from its parishes' forkod (first two digits = SCB county code);
# only used to separate namesakes (Åkerbo, Östra), so a few wrong ones do no harm
rel1 <- read_csv("data-raw/data/Tbl_topografi_rel.csv", show_col_types = FALSE) %>%
  filter(Ordning == "Överordnad") %>% transmute(parent_topo = kalla_id, child_topo = dest_id) %>%
  inner_join(rec1 %>% rename(parent = unit_id, pt = type_id), by = c("parent_topo" = "topo_id"),
             relationship = "many-to-many") %>%
  inner_join(rec1 %>% rename(child = unit_id, ct = type_id), by = c("child_topo" = "topo_id"),
             relationship = "many-to-many")
# the earliest forkod of each parish (1952-66), not the last: the 1971 reform moved whole
# härader between counties (Bro to Stockholm, Vättle to Göteborg), and the court lists are
# older than that
fk <- m1b$unit_codes %>% filter(system == "forkod") %>%
  group_by(unit_id) %>% slice_min(start_date, n = 1, with_ties = FALSE) %>% ungroup() %>%
  transmute(child = unit_id, cc = substr(code, 1, 2))
hundred_county <- rel1 %>% filter(pt == "hundred", ct == "parish") %>% distinct(parent, child) %>%
  inner_join(fk, by = "child", relationship = "many-to-many") %>%
  count(parent, cc) %>% group_by(parent) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>%
  transmute(unit_id = parent,
            county = county_meta$name[match(as.integer(cc), county_meta$code)])

hundreds <- u1 %>% filter(type_id == "hundred") %>%
  transmute(unit_id, name, start_date, end_date,
            key = norm_key(sub("\\s+(härad|skeppslag|bergslag|stad|tingslag|lappmark|lappmarker|mot|mots)$",
                               "", name)),
            ckey = court_key(name, drop_s = TRUE),
            kind_word = tolower(str_match(name, "\\s(härad|skeppslag|bergslag|stad|tingslag|lappmark|lappmarker|mots?)$")[, 2])) %>%
  left_join(hundred_county, by = "unit_id")
m1_courts <- u1 %>% filter(type_id %in% c("magistrates_court", "district_court")) %>%
  transmute(unit_id, type_id, name, start_date, end_date, key = court_key(name, drop_s = TRUE),
            n_parts = lengths(strsplit(key, "+", fixed = TRUE)))

## Match a printed unit name to an m1 hundred: exact key, then key with the kind word, then
## the county for namesakes, then a near miss (OCR, historical spelling) in the same county.
## A match is refused when both counties are known and differ (the Näs tingslag of Jämtland
## is not Näs härad in Värmland) and for a bare direction word, which is what is left of a
## tingslag name the calendar prints as "Norra t:lag".
BARE <- c("norra", "sodra", "ostra", "vastra", "vestra", "ovre", "nedre", "mellersta", "nya",
          "norr", "soder", "oster", "vaster")
HARAD_KINDS <- c("härad", "härader", "härads", "hårad", "h:d", "h:ds", "h:dt", "skeppslag",
                 "sk:lag", "mot", "mots", "möt", "bergslag")
## `allow_stad` is TRUE only for a town court: a domsaga named after a town is named after
## its seat, not made of the town, so "Linköpings domsaga" must not become Linköpings stad.
match_hundred <- function(nm, kind, county, allow_stad = rep(TRUE, length(nm))){
  k <- norm_key(nm)
  out <- rep(NA_character_, length(k)); how <- rep(NA_character_, length(k))
  for (i in seq_along(k)) {
    if (is.na(k[i]) || !nzchar(k[i])) next
    # "Norra t:lag" of Falu domsaga is a truncated tingslag name, but Östra and Västra härad
    # in Njudung really are called that
    if (k[i] %in% BARE && !(!is.na(kind[i]) & kind[i] %in% HARAD_KINDS)) {
      how[i] <- "bare direction"; next
    }
    cand <- if (allow_stad[i]) hundreds else hundreds[is.na(hundreds$kind_word) |
                                                        hundreds$kind_word != "stad", ]
    h <- cand[cand$key == k[i], ]
    if (nrow(h) == 1 && !is.na(county[i]) && !is.na(h$county) && h$county != county[i]) {
      how[i] <- "county mismatch"; next
    }
    if (nrow(h) > 1) {
      kw <- if (!is.na(kind[i])) sub("^(h:d|h:ds|härads|härader|hårad)$", "härad",
                                     sub("^(t:lag|t:laget|tingslaget|tingslags)$", "tingslag",
                                         sub("^(sk:lag)$", "skeppslag", kind[i]))) else NA
      if (!is.na(kw) && any(h$kind_word == kw, na.rm = TRUE)) h <- h[h$kind_word %in% kw, ]
      if (nrow(h) > 1 && !is.na(county[i])) {
        hh <- h[!is.na(h$county) & h$county == county[i], ]
        if (nrow(hh) >= 1) h <- hh
      }
    }
    if (nrow(h) == 1) { out[i] <- h$unit_id; how[i] <- "name"; next }
    if (nrow(h) > 1)  { how[i] <- "ambiguous"; next }
    pool <- if (!is.na(county[i])) cand[is.na(cand$county) | cand$county == county[i], ] else cand
    lim <- if (nchar(k[i]) >= 8) 2L else 1L
    if (nrow(pool) && nchar(k[i]) >= 5) {
      d <- as.vector(adist(k[i], pool$key))
      j <- which.min(d)
      if (length(j) && d[j] <= lim && sum(d == d[j]) == 1) { out[i] <- pool$unit_id[j]; how[i] <- "fuzzy" }
    }
  }
  list(unit_id = out, how = how)
}
uu <- rec_units %>% left_join(rec_meta %>% select(year, ckey, county), by = c("year", "ckey"))
allow_stad <- uu$unit_source == "list" | (!is.na(uu$unit_kind) & uu$unit_kind == "stad")
mm <- match_hundred(uu$unit, uu$unit_kind, uu$county, allow_stad)
uu$hundred_id <- mm$unit_id; uu$match_how <- mm$how
# "Bråbo och Memmings härad" is two härader: split what did not match as a whole
split_idx <- which(is.na(uu$hundred_id) & grepl("\\s(och|o\\.)\\s", uu$unit))
if (length(split_idx)) {
  extra <- bind_rows(lapply(split_idx, function(i){
    parts <- trimws(unlist(strsplit(uu$unit[i], "\\s+(och|o\\.)\\s+")))
    tibble(uu[rep(i, length(parts)), setdiff(names(uu), c("unit", "hundred_id", "match_how"))],
           unit = parts)
  }))
  me <- match_hundred(extra$unit, extra$unit_kind, extra$county,
                      extra$unit_source == "list" |
                        (!is.na(extra$unit_kind) & extra$unit_kind == "stad"))
  extra$hundred_id <- me$unit_id
  extra$match_how <- ifelse(is.na(me$unit_id), NA_character_, "split")
  extra <- extra %>% filter(!is.na(hundred_id))
  uu <- bind_rows(uu[-split_idx[split_idx %in% which(is.na(uu$hundred_id))], ], extra)
}
## A printed unit is a härad (or skeppslag/mot) when the calendar says so; a tingslag in
## Norrland or Dalarna has no counterpart in m1 (the source register has no tingslag,
## FOLLOWUP "Tingslag polygons for Norrland/Dalarna"), and a unit read from the domsaga's own
## name (unit_source = "name") is often not a härad at all ("Mellersta Roslags", "Stockholms
## läns västra"). The match rate is therefore reported for each group.
uu <- uu %>% mutate(is_harad = !is.na(unit_kind) & unit_kind %in% HARAD_KINDS,
                    child = coalesce(hundred_id, paste0("stk:", norm_key(unit))))
message(sprintf("  printed constituent units: %d, matched to an m1 hundred: %d (%.1f%%)",
                nrow(uu), sum(!is.na(uu$hundred_id)), 100 * mean(!is.na(uu$hundred_id))))
message(sprintf("    printed as a härad/skeppslag: %d of %d (%.1f%%)",
                sum(uu$is_harad & !is.na(uu$hundred_id)), sum(uu$is_harad),
                100 * mean(!is.na(uu$hundred_id[uu$is_harad]))))
message(sprintf("    read from the domsaga's own name: %d of %d (%.1f%%)",
                sum(uu$unit_source == "name" & !is.na(uu$hundred_id)), sum(uu$unit_source == "name"),
                100 * mean(!is.na(uu$hundred_id[uu$unit_source == "name"]))))

## ---- 7. Court units: chain the records of consecutive editions -----------------------
## Same court when the name keys agree, or (for the unnamed Göta hovrätt entries of
## 1866-1905) when the sets of constituent units overlap by at least half, best match first
## and each record claimed once. The units are compared as the m1 hundred they matched, so
## spelling differences between editions (Wester-Dalarne / Vester-Dalarne) do not break a
## chain. Domsagor, rådhusrätter and tingsrätter are never chained into one another: the
## 1971 reform replaced the first two by the third, and m1 keeps them as two types; the name
## link between them is kept as `successor_of`.
uset <- uu %>% group_by(year, ckey) %>% summarise(u = list(sort(unique(child))), .groups = "drop")
rec_meta <- rec_meta %>% left_join(uset, by = c("year", "ckey"))
rec_meta$u[lengths(rec_meta$u) == 0] <- list(character(0))
rec_meta$rid <- seq_len(nrow(rec_meta))
# a court with no printed name is named after its units, the way the register names its own
# multi-härad courts ("Lösings, Bråbo och Memmings")
unit_names_of <- uu %>% arrange(year, ckey, ord) %>% group_by(year, ckey) %>%
  summarise(un = paste(unique(unit), collapse = ", "), .groups = "drop")
rec_meta <- rec_meta %>% left_join(unit_names_of, by = c("year", "ckey")) %>%
  mutate(name_source = ifelse(is.na(court_name), "units", "printed"),
         cname = ifelse(is.na(court_name), ifelse(is.na(un), paste0("[", county, " ", court_no, "]"), un),
                        court_name))

link <- bind_rows(lapply(seq_len(nrow(editions) - 1), function(i){
  a <- rec_meta %>% filter(year == ED[i]);  b <- rec_meta %>% filter(year == ED[i + 1])
  if (!nrow(a) || !nrow(b)) return(NULL)
  g <- expand_grid(ai = seq_len(nrow(a)), bi = seq_len(nrow(b))) %>%
    filter(a$court_kind[ai] == b$court_kind[bi])
  if (!nrow(g)) return(NULL)
  same_name <- !startsWith(a$ckey[g$ai], "#") & a$ckey[g$ai] == b$ckey[g$bi]
  jac <- vapply(seq_len(nrow(g)), function(k){
    x <- a$u[[g$ai[k]]]; y <- b$u[[g$bi[k]]]
    if (!length(x) || !length(y)) return(0)
    length(intersect(x, y)) / length(union(x, y))
  }, 0)
  g$score <- ifelse(same_name, 1 + jac, ifelse(jac >= 0.5, jac, 0))
  g <- g %>% filter(score > 0) %>% arrange(desc(score))
  g <- g[!duplicated(g$ai) & !duplicated(g$bi), ]
  tibble(from = a$rid[g$ai], to = b$rid[g$bi], score = g$score)
}))
## Second pass: a court renamed between two editions ("Tjusts domsaga" ->
## "Norra och Södra Tjusts härads domsaga") has neither the same key nor overlapping units.
## Records still unlinked in two adjacent editions are joined when they are in the same
## county and share the distinctive words of their names.
name_words <- lapply(rec_meta$cname, function(x){
  w <- strsplit(norm_key(x), " ")[[1]]
  w <- setdiff(w, c("domsaga", "domsagan", "tingsratt", "tingsratten", "radhusratt", "harad",
                    "harads", "harader", "lans", "och", "o", "med", "samt"))
  w[nchar(w) >= 4]
})
link2 <- bind_rows(lapply(seq_len(nrow(editions) - 1), function(i){
  ai <- which(rec_meta$year == ED[i] & !rec_meta$rid %in% link$from)
  bi <- which(rec_meta$year == ED[i + 1] & !rec_meta$rid %in% link$to)
  if (!length(ai) || !length(bi)) return(NULL)
  g <- expand_grid(a = ai, b = bi) %>%
    filter(rec_meta$court_kind[a] == rec_meta$court_kind[b],
           !is.na(rec_meta$county[a]), !is.na(rec_meta$county[b]),
           rec_meta$county[a] == rec_meta$county[b])
  if (!nrow(g)) return(NULL)
  g$score <- vapply(seq_len(nrow(g)), function(k){
    x <- name_words[[g$a[k]]]; y <- name_words[[g$b[k]]]
    if (!length(x) || !length(y)) return(0)
    length(intersect(x, y)) / min(length(x), length(y))
  }, 0)
  g <- g %>% filter(score >= 0.6) %>% arrange(desc(score))
  g <- g[!duplicated(g$a) & !duplicated(g$b), ]
  tibble(from = rec_meta$rid[g$a], to = rec_meta$rid[g$b], score = g$score)
}))
link <- bind_rows(link, link2)
grp <- create_block(c(rec_meta$rid, link$from), c(rec_meta$rid, link$to))[seq_len(nrow(rec_meta))]
rec_meta$grp <- grp
## Third pass: a court missed in one edition (an OCR failure) leaves two chains with the same
## name and no overlap in time. They are one court.
gkey <- rec_meta %>% group_by(grp) %>%
  summarise(kind = first(court_kind), k = court_key(cname[which.max(year)]),
            y1 = min(year), y2 = max(year), .groups = "drop") %>%
  filter(nzchar(k))
pairs <- gkey %>% inner_join(gkey, by = c("kind", "k"), relationship = "many-to-many") %>%
  filter(grp.x < grp.y, y2.x < y1.y | y2.y < y1.x)
if (nrow(pairs)) {
  grp <- create_block(c(rec_meta$grp, pairs$grp.x), c(rec_meta$grp, pairs$grp.y))[seq_len(nrow(rec_meta))]
  rec_meta$grp <- grp
  message(sprintf("  chains merged across a missing edition: %d", nrow(pairs)))
}

KIND_PREFIX <- c(domsaga = "DS", radhusratt = "RR", tingsratt = "TR")
court_units <- rec_meta %>%
  group_by(grp) %>%
  summarise(court_kind = first(court_kind),
            name = cname[which.max(year)], first_name = cname[which.min(year)],
            name_source = name_source[which.max(year)],
            first_seen = min(year), last_seen = max(year), n_editions = n_distinct(year),
            editions = paste(sort(unique(year)), collapse = ";"),
            hovratt = paste(unique(na.omit(hovratt)), collapse = ";"),
            county = paste(unique(na.omit(county)), collapse = ";"),
            seat = paste(unique(na.omit(seat)), collapse = ";"),
            .groups = "drop") %>%
  arrange(court_kind, first_seen, name) %>%
  group_by(court_kind) %>%
  mutate(court_id = sprintf("%s:%04d", KIND_PREFIX[court_kind], row_number())) %>% ungroup()
# dates: certain at the editions where the court is printed, open between them
court_units <- court_units %>%
  mutate(start_date     = d_start(first_seen),
         start_earliest  = d_start(prev_ed[as.character(first_seen)] + 1L),
         start_precision = ifelse(first_seen == min(ED), "not_after", "between_editions"),
         end_date        = d_end(last_seen),
         end_latest      = d_end(next_ed[as.character(last_seen)] - 1L),
         end_precision   = ifelse(last_seen == max(ED), "not_before", "between_editions"),
         gaps = vapply(strsplit(editions, ";"), function(e){
           e <- as.integer(e); sum(!ED[ED >= min(e) & ED <= max(e)] %in% e) }, 1L)) %>%
  select(court_id, court_kind, name, first_name, name_source, hovratt, county, seat,
         start_date, start_earliest, start_precision, end_date, end_latest, end_precision,
         first_seen, last_seen, n_editions, editions, gaps, grp)
rec_meta$court_id <- court_units$court_id[match(rec_meta$grp, court_units$grp)]
uu <- uu %>% left_join(rec_meta %>% select(year, ckey, court_id), by = c("year", "ckey"))

# the tingsrätt that took over a domsaga's or rådhusrätt's name in 1971
old70 <- court_units %>% filter(court_kind != "tingsratt", last_seen >= 1970) %>%
  transmute(old_id = court_id, k = court_key(name))
succ <- court_units %>% filter(court_kind == "tingsratt") %>%
  transmute(court_id, k = court_key(name)) %>%
  inner_join(old70, by = "k") %>% distinct(court_id, .keep_all = TRUE)
court_units$successor_of <- succ$old_id[match(court_units$court_id, succ$court_id)]

## court_names: the name as printed in each edition, with the period it is attested for
court_names <- rec_meta %>% filter(!is.na(court_name)) %>%
  arrange(court_id, year) %>% group_by(court_id) %>%
  mutate(run = cumsum(court_key(court_name) != lag(court_key(court_name), default = ""))) %>%
  group_by(court_id, run) %>%
  summarise(name = court_name[which.max(year)], first_seen = min(year), last_seen = max(year),
            .groups = "drop") %>%
  transmute(court_id, name, start_date = d_start(first_seen), end_date = d_end(last_seen),
            precision = "between_editions", kind = "official",
            source = paste0("statskalender_", first_seen))

## ---- 8. Match court units to m1 courts ----------------------------------------------
## By name key first, then by the härader the court is made of against the m1 unit whose own
## name lists them (the register names its multi-härad courts after their härader).
m1_parts <- m1_courts %>%
  mutate(part = strsplit(key, "+", fixed = TRUE)) %>% tidyr::unnest(part) %>%
  filter(nzchar(part))
cu_parts <- uu %>% filter(!is.na(hundred_id)) %>%
  mutate(part = hundreds$ckey[match(hundred_id, hundreds$unit_id)]) %>%
  distinct(court_id, year, part)
match_report <- bind_rows(lapply(seq_len(nrow(court_units)), function(i){
  cu <- court_units[i, ]
  ty <- if (cu$court_kind == "tingsratt") "district_court" else "magistrates_court"
  pool <- m1_courts %>% filter(type_id == ty,
                               as.integer(substr(start_date, 1, 4)) <= cu$last_seen,
                               as.integer(substr(end_date, 1, 4)) >= cu$first_seen)
  if (!nrow(pool)) return(tibble(court_id = cu$court_id, m1_unit = NA_character_,
                                 m1_name = NA_character_, how = "no m1 unit in period", score = 0))
  k <- court_key(cu$name, drop_s = TRUE)
  hit <- pool %>% filter(key == k)
  if (nrow(hit) == 1) return(tibble(court_id = cu$court_id, m1_unit = hit$unit_id,
                                    m1_name = hit$name, how = "name", score = 1))
  if (!nrow(hit) && nchar(k) >= 6) {                   # Hälsingborg / Helsingborgs
    d <- as.vector(adist(k, pool$key)); j <- which.min(d)
    if (length(j) && d[j] <= 2 && sum(d == d[j]) == 1)
      return(tibble(court_id = cu$court_id, m1_unit = pool$unit_id[j], m1_name = pool$name[j],
                    how = "fuzzy name", score = 0.9))
  }
  p <- cu_parts %>% filter(court_id == cu$court_id) %>% pull(part) %>% unique()
  if (!length(p)) p <- setdiff(trimws(strsplit(k, "+", fixed = TRUE)[[1]]), "")
  if (!length(p)) return(tibble(court_id = cu$court_id, m1_unit = NA_character_,
                                m1_name = NA_character_, how = "no units", score = 0))
  sc <- m1_parts %>% filter(unit_id %in% pool$unit_id) %>%
    group_by(unit_id) %>%
    summarise(j = length(intersect(part, p)) / length(union(part, p)), .groups = "drop") %>%
    arrange(desc(j))
  if (!nrow(sc) || sc$j[1] == 0) return(tibble(court_id = cu$court_id, m1_unit = NA_character_,
                                               m1_name = NA_character_, how = "new", score = 0))
  tibble(court_id = cu$court_id, m1_unit = sc$unit_id[1],
         m1_name = pool$name[match(sc$unit_id[1], pool$unit_id)],
         how = if (sc$j[1] >= 0.5) "composition" else "weak", score = sc$j[1])
}))
match_report <- match_report %>%
  left_join(court_units %>% select(court_id, court_kind, name, first_seen, last_seen), by = "court_id") %>%
  mutate(m1_unit = ifelse(how == "weak", NA_character_, m1_unit),
         status = ifelse(is.na(m1_unit), "new", how))
court_units$m1_unit   <- match_report$m1_unit[match(court_units$court_id, match_report$court_id)]
court_units$m1_match  <- match_report$status[match(court_units$court_id, match_report$court_id)]
court_units$m1_score  <- match_report$score[match(court_units$court_id, match_report$court_id)]
message(sprintf("  court units: %d (%s); matched to an m1 unit: %d (%.1f%%)",
                nrow(court_units),
                paste(names(table(court_units$court_kind)), table(court_units$court_kind), collapse = " "),
                sum(!is.na(court_units$m1_unit)), 100 * mean(!is.na(court_units$m1_unit))))
## ---- 9. Evidence ---------------------------------------------------------------------
## One row per (constituent unit, court) and run of consecutive editions. An observation is
## carried forward to the day before the next edition, so the rows tile; start_earliest /
## end_latest hold the uncertainty. Nothing is carried back before the first edition.
##
## A court prints what it is made of only when that is not already in its name: after the
## 1918 and 1948 reforms most domsagor are one tingslag and the calendar prints no list at
## all. A unit printed in one edition of a court is therefore taken to belong to that court
## for the court's whole life, unless another court prints it in the meantime. Those
## editions are marked `inferred` and counted in `n_inferred`, so m3b can weigh them.
ed_pos <- setNames(seq_along(ED), ED)
claim <- uu %>%
  distinct(child, child_name = unit, hundred_id, court_id, year, unit_kind, partial, county,
           unit_source) %>%
  group_by(child, court_id, year) %>%
  summarise(child_name = last(child_name), hundred_id = first(hundred_id),
            unit_kind = last(unit_kind), partial = any(partial), county = last(na.omit(county)),
            printed = if (any(unit_source == "list")) "list" else last(unit_source),
            .groups = "drop") %>%
  mutate(observed = TRUE)
life <- court_units %>% select(court_id, cu_first = first_seen, cu_last = last_seen)
filled <- claim %>% distinct(child, court_id) %>% left_join(life, by = "court_id") %>%
  rowwise() %>%
  mutate(year = list(ED[ED >= cu_first & ED <= cu_last])) %>% ungroup() %>%
  tidyr::unnest(year) %>% select(child, court_id, year)
claims <- filled %>%
  left_join(claim, by = c("child", "court_id", "year")) %>%
  mutate(observed = !is.na(observed)) %>%
  group_by(child, court_id) %>%
  tidyr::fill(child_name, hundred_id, unit_kind, county, printed, .direction = "downup") %>%
  mutate(partial = tidyr::replace_na(partial, FALSE)) %>% ungroup()
# a printed claim beats an inferred one; two inferred claims on one child cancel out
claims <- claims %>% group_by(child, year) %>%
  filter(if (any(observed)) observed else n() == 1) %>% ungroup()
runs <- claims %>%
  arrange(child, court_id, year) %>%
  group_by(child, court_id) %>%
  mutate(brk = cumsum(c(TRUE, diff(ed_pos[as.character(year)]) != 1))) %>%
  group_by(child, court_id, brk) %>%
  summarise(child_name = last(child_name), hundred_id = first(hundred_id),
            unit_kind = last(unit_kind), partial = any(partial), county = last(na.omit(county)),
            printed = first(printed), y1 = min(year), y2 = max(year), n_ed = n(),
            n_obs = sum(observed),
            y_obs1 = if (any(observed)) min(year[observed]) else NA_integer_,
            y_obs2 = if (any(observed)) max(year[observed]) else NA_integer_,
            .groups = "drop")
# what the first and last edition said about this child, to date the bounds
seen <- claims %>% distinct(child, year)
first_seen_child <- seen %>% group_by(child) %>% summarise(fy = min(year), ly = max(year), .groups = "drop")
court_evidence <- runs %>%
  left_join(first_seen_child, by = "child") %>%
  mutate(start_date = d_start(y1),
         start_earliest = d_start(ifelse(y1 == fy, NA, prev_ed[as.character(y1)] + 1L)),
         start_precision = ifelse(y1 == fy, "not_after", "between_editions"),
         end_year = ifelse(is.na(next_ed[as.character(y2)]), 1990L, next_ed[as.character(y2)] - 1L),
         end_date = d_end(end_year),
         end_latest = as.Date(NA),
         end_precision = ifelse(y2 == ly & y2 == max(ED), "not_before", "between_editions")) %>%
  left_join(court_units %>% select(court_id, court_kind, court_name = name, m1_unit), by = "court_id") %>%
  transmute(child_unit = ifelse(is.na(hundred_id), NA_character_, hundred_id),
            child_name, child_county = county, child_kind = unit_kind, partial, printed,
            court_id, court_kind, court_name, court_m1_unit = m1_unit,
            start_date, start_earliest, start_precision, end_date, end_latest, end_precision,
            first_edition = y1, last_edition = y2, n_editions = n_ed,
            first_printed = y_obs1, last_printed = y_obs2, n_printed = n_obs,
            n_inferred = n_ed - n_obs,
            source = ifelse(is.na(y_obs1), "statskalender_inferred",
                            paste0("statskalender_", y_obs1)),
            relation = ifelse(court_kind == "radhusratt", "town_court", "court"))
# the hovrätt of a court, per run of editions (the court of appeal above the court)
court_hovratt <- rec_meta %>% filter(!is.na(hovratt)) %>%
  arrange(court_id, year) %>% group_by(court_id) %>%
  mutate(run = cumsum(hovratt != lag(hovratt, default = ""))) %>%
  group_by(court_id, run) %>%
  summarise(hovratt = last(hovratt), y1 = min(year), y2 = max(year), .groups = "drop") %>%
  mutate(start_date = d_start(y1),
         end_date = d_end(ifelse(is.na(next_ed[as.character(y2)]), 1990L,
                                 next_ed[as.character(y2)] - 1L)),
         start_precision = ifelse(y1 == min(ED), "not_after", "between_editions"),
         end_precision = ifelse(y2 == max(ED), "not_before", "between_editions"),
         source = paste0("statskalender_", y1)) %>%
  select(court_id, hovratt, start_date, end_date, start_precision, end_precision, source)

## A hovrätt created between two editions gets its own start date, not the edition's. Hovrätten för
## Västra Sverige and Hovrätten för Nedre Norrland were created on 1 January 1948 and appear first in
## the 1950 volume, so every court that moved to them was still under Göta or Svea in 1948 and 1949 —
## the Gesäter case in the held-out reading. Where a run's hovrätt is a unit that began after the
## previous edition, the run starts when the unit did, and the run before it ends the day before.
hov_units <- u1 %>% filter(type_id == "court_of_appeal") %>%
  transmute(h_key = canon_hovratt(name), h_start = start_date)
court_hovratt <- court_hovratt %>%
  mutate(h_key = canon_hovratt(hovratt)) %>%
  left_join(hov_units, by = "h_key") %>%
  group_by(court_id) %>% arrange(start_date, .by_group = TRUE) %>%
  # the previous run's own edition year, which is where the calendar last said something else
  mutate(prev_ed_year = lag(as.integer(format(start_date, "%Y"))),
         # the hovrätt began between the previous edition and this one, so the run starts then and
         # not at the edition that first printed it (the run moves earlier, hence h_start < start)
         moved = !is.na(h_start) & !is.na(prev_ed_year) & h_start < start_date &
                 as.integer(format(h_start, "%Y")) > prev_ed_year,
         start_date = if_else(moved, h_start, start_date),
         start_precision = if_else(moved, "exact", start_precision),
         end_date = if_else(!is.na(lead(moved)) & lead(moved),
                            lead(h_start) - 1, end_date),
         end_precision = if_else(!is.na(lead(moved)) & lead(moved), "exact", end_precision)) %>%
  ungroup() %>% select(-h_key, -h_start, -prev_ed_year, -moved)
message(sprintf("  court -> hovrätt runs: %d, of which dated by the hovrätt's own start: %d",
                nrow(court_hovratt), sum(court_hovratt$start_precision == "exact")))

## ---- 10. Corrections -----------------------------------------------------------------
## Deviations from the source register that research has established, each with its citation
## (DESIGN.md: they belong in `corrections`; kept here so m3c is self-contained and m1's
## corrections.csv, which another step owns, is not touched).
court_corrections <- tribble(
  ~id,   ~child_name,           ~court_name,                    ~start, ~end,  ~source, ~note,
  "C01", "Viske härad",         "Hallands norra domsaga",        1682,  1970,
  "FOLLOWUP.md C5 (review 2026-09-25); Sveriges statskalender 1866 p147-152, 1881 p108-111, 1905 p116-119, 1931 p166-174 all put Viske with Fjäre under the Hallands norra domsaga",
  "the earlier build put Viske härad under Varbergs rådhusrätt 1682-1970",
  "C02", "Finnerödja",          "Vadsbo domsaga",                1600,  1970,
  "swehist 1.1.1 step4f; Riksarkivet Förvaltningshistorik (Vadsbo härad, Skaraborgs län)",
  "The source puts Finnerödja in Södra Närke domsaga under Svea hovrätt; it belonged to Vadsbo härad under Göta hovrätt",
  "C03", "Gotlands norra härad", NA,                             1646,  1970,
  "swehist the earlier build (Gotland under Svea hovrätt from 1646); confirmed by the statskalender 1866-1970, which lists Gotland under Svea",
  "Court of appeal only: Gotland is under Svea hovrätt from 1646, not Göta until 1947",
  "C04", "Gotlands södra härad", NA,                             1646,  1970,
  "swehist the earlier build (Gotland under Svea hovrätt from 1646); confirmed by the statskalender 1866-1970",
  "Court of appeal only: Gotland is under Svea hovrätt from 1646, not Göta until 1947")
corr_rows <- court_corrections %>%
  filter(!is.na(court_name)) %>%
  mutate(child_unit = hundreds$unit_id[match(norm_key(sub("\\s+härad$", "", child_name)), hundreds$key)],
         court_id = court_units$court_id[match(norm_key(court_name), norm_key(court_units$name))]) %>%
  transmute(child_unit, child_name, child_county = NA_character_, child_kind = "härad",
            partial = FALSE, printed = "correction", court_id,
            court_kind = "domsaga", court_name,
            court_m1_unit = court_units$m1_unit[match(court_id, court_units$court_id)],
            start_date = d_start(start), start_earliest = as.Date(NA),
            start_precision = "exact", end_date = d_end(end), end_latest = as.Date(NA),
            end_precision = "exact", first_edition = NA_integer_, last_edition = NA_integer_,
            n_editions = NA_integer_, first_printed = NA_integer_, last_printed = NA_integer_,
            n_printed = NA_integer_, n_inferred = NA_integer_,
            source = "correction", relation = "court")
court_evidence <- bind_rows(court_evidence, corr_rows)
## m1-compatible view, which m3b reads: the link expressed with m1 unit ids only. Both are
## NA when the constituent unit or the court has no m1 counterpart (a Norrland tingslag, a
## domsaga the register does not have), so that a consumer joining on them drops the row
## instead of inventing a parent. The full link is child_unit -> court_id.
court_evidence <- court_evidence %>%
  mutate(hundred_unit = ifelse(is.na(child_unit) | is.na(court_m1_unit), NA_character_, child_unit),
         court_unit   = ifelse(is.na(child_unit) | is.na(court_m1_unit), NA_character_, court_m1_unit),
         .after = court_m1_unit)
message(sprintf("  links expressible with m1 ids (hundred_unit -> court_unit): %d of %d",
                sum(!is.na(court_evidence$hundred_unit)), nrow(court_evidence)))

## ---- 11. Report ----------------------------------------------------------------------
coverage <- stk %>% group_by(year) %>%
  summarise(rows = n(), courts = n_distinct(court_no), .groups = "drop") %>%
  left_join(uu %>% group_by(year) %>%
              summarise(units = n(), matched = sum(!is.na(hundred_id)),
                        units_harad = sum(is_harad), matched_harad = sum(is_harad & !is.na(hundred_id)),
                        units_listed = sum(unit_source == "list"),
                        matched_listed = sum(unit_source == "list" & !is.na(hundred_id)),
                        fuzzy = sum(match_how %in% c("fuzzy", "split"), na.rm = TRUE),
                        .groups = "drop"), by = "year") %>%
  left_join(rec_meta %>% group_by(year) %>%
              summarise(with_list = sum(unit_source == "list"),
                        from_name = sum(unit_source == "name"),
                        no_units  = sum(unit_source == "none"), .groups = "drop"), by = "year") %>%
  left_join(editions %>% select(year, work, from, to), by = "year") %>%
  mutate(across(c(units, matched, units_harad, matched_harad, units_listed, matched_listed, fuzzy),
                ~ tidyr::replace_na(.x, 0L)),
         share_matched = ifelse(units > 0, round(matched / units, 3), NA_real_),
         share_matched_harad = ifelse(units_harad > 0, round(matched_harad / units_harad, 3), NA_real_),
         share_matched_listed = ifelse(units_listed > 0, round(matched_listed / units_listed, 3), NA_real_),
         url = sprintf("https://runeberg.org/%s/%04d.html", work, from))

## Single-härad courts in m1 that the statskalender groups into a bigger domsaga
single <- m1_courts %>% filter(type_id == "magistrates_court", n_parts == 1) %>%
  mutate(hundred_id = hundreds$unit_id[match(key, hundreds$ckey)],
         y1 = as.integer(substr(start_date, 1, 4)), y2 = as.integer(substr(end_date, 1, 4)))
single_in_bigger <- single %>% filter(!is.na(hundred_id)) %>%
  inner_join(uu %>% filter(!is.na(hundred_id)) %>% select(hundred_id, year, court_id),
             by = "hundred_id", relationship = "many-to-many") %>%
  filter(year >= y1, year <= y2) %>%
  left_join(uu %>% filter(!is.na(hundred_id)) %>% count(court_id, year, name = "n_units"),
            by = c("court_id", "year")) %>%
  group_by(unit_id, name, y1, y2) %>%
  summarise(editions = paste(sort(unique(year)), collapse = ";"),
            max_units = max(n_units, na.rm = TRUE),
            courts = paste(unique(court_units$name[match(court_id, court_units$court_id)]), collapse = " | "),
            .groups = "drop") %>%
  filter(max_units > 1)

## Composition changes between adjacent editions
comp <- uu %>% filter(!is.na(hundred_id)) %>%
  distinct(hundred_id, year, court_id) %>%
  left_join(court_units %>% select(court_id, court_name = name), by = "court_id") %>%
  arrange(hundred_id, year)
changes <- comp %>% group_by(hundred_id) %>%
  mutate(prev_court = lag(court_id), prev_name = lag(court_name), prev_year = lag(year)) %>%
  filter(!is.na(prev_court), prev_court != court_id) %>% ungroup() %>%
  transmute(hundred = hundreds$name[match(hundred_id, hundreds$unit_id)],
            county = hundreds$county[match(hundred_id, hundreds$unit_id)],
            from_court = prev_name, to_court = court_name,
            between = paste0(prev_year, "-", year)) %>%
  arrange(between, county, hundred)

## How much of the country the court evidence covers, by year: the share of the m1 hundreds
## that are not towns and have a court at that date
hundreds_rural <- hundreds %>% filter(is.na(kind_word) | kind_word != "stad")
harad_coverage <- bind_rows(lapply(seq(1870, 1970, by = 10), function(y){
  e <- court_evidence %>%
    filter(!is.na(child_unit), relation == "court",
           as.integer(substr(start_date, 1, 4)) <= y, as.integer(substr(end_date, 1, 4)) >= y)
  tibble(year = y, hundreds = nrow(hundreds_rural),
         with_court = n_distinct(e$child_unit[e$child_unit %in% hundreds_rural$unit_id]),
         printed = n_distinct(e$child_unit[e$child_unit %in% hundreds_rural$unit_id &
                                             e$first_printed <= y & e$last_printed >= y])) %>%
    mutate(share = round(with_court / hundreds, 3))
}))
print(as.data.frame(harad_coverage))

limitations <- c(
  "Sveriges statskalender is not digitised on runeberg.org before 1864, so there is no court evidence for 1600-1865; m3b must keep the register's undated hundred -> court links there and treat them as undated.",
  "After 1971 the calendar prints only the tingsrätt's name, seat and hovrätt: its domkrets is a list of kommuner in SFS. The 1972-1984 editions therefore give units, not memberships.",
  "Rådhusrätter are taken only from the 1970 edition, whose court section lists them beside the domsagor. In 1866-1963 the town courts stand in the 'Städernas styrelser' section, whose town headers parse at only about 80-90% (and at 50% after 1940), which would make a court look abolished when it was only missed. Needs a decision: parse that section properly, or take the town courts from the m1 names (71 units are called 'X rådhusrätt').",
  "A domsaga printed as 'utgör 1 t:lag' has no list of härader; its units are then read from its own name (unit_source = 'name'), which fails for the domsagor named after a place or a region (Folkungabygdens, Linköpings, Bråbygdens). That is why the share of härader with a court falls after the 1918 and 1948 reforms (89-91% in 1870-1900, about 71% in 1960): the calendar stopped printing what a one-tingslag domsaga is made of. The remainder needs SFS (domkretsindelning) or the 1918- volumes 'Sveriges indelning i domsagor'.",
  "Öland (5 härader) and Gotland (5) never match: the calendar prints 'Ölands norra/södra mot' and 'Gotlands norra/södra härad', which the register does not have as units. Folkare, Lekeberg, Tiunda, the Ångermanland Nora and the Dalarna tingslag are missing from the register altogether (it has 297 hundreds, of which 69 are towns, for about 380 real härader and tingslag).",
  "The parse is OCR: 4.5% of the names printed as a härad do not reach an m1 hundred, through OCR damage or because the unit is missing from the register; those rows keep the printed name and child_unit = NA, so nothing is silently lost.",
  "A unit printed once under a court is carried through that court's whole life unless another court prints it (2,113 inferred edition-claims against 2,622 printed). One härad, Valle in Skaraborg, then ends up in two courts at once, because the 1866 and 1881 lists put it in both Vadsbo södra and Skånings-Vilske-Valle; it needs a source decision.",
  "A match to an m1 hundred is refused when the printed county and the hundred's own county (from its parishes' earliest forkod) differ. That is right for the Näs tingslag of Dalarna against Näs härad in Värmland, but it also refuses a härad that changed county before 1952; those rows are in m3c_unmatched_units.csv with match_how = 'county mismatch'.")

## m1 court units with no statskalender court in their lifetime, and how they are named
m1_unmatched <- m1_courts %>%
  filter(!unit_id %in% na.omit(court_units$m1_unit)) %>%
  mutate(y1 = as.integer(substr(start_date, 1, 4)), y2 = as.integer(substr(end_date, 1, 4)),
         in_period = y1 <= max(ED) & y2 >= min(ED),
         looks_like = ifelse(grepl("rådhusrätt", name), "radhusratt",
                             ifelse(grepl("domsaga", name), "domsaga", "hundred name"))) %>%
  select(unit_id, type_id, name, start_date, end_date, in_period, looks_like, n_parts) %>%
  arrange(desc(in_period), name)

report <- list(
  coverage = coverage,
  harad_coverage = harad_coverage,
  match_report = match_report,
  m1_unmatched = m1_unmatched,
  member_match = uu %>% count(unit_source, is_harad, match_how, name = "rows"),
  single_harad_courts = single_in_bigger,
  composition_changes = changes,
  unmatched_units = uu %>% filter(is.na(hundred_id)) %>%
    count(unit, unit_kind, unit_source, county, match_how, is_harad, name = "editions") %>%
    arrange(desc(is_harad), desc(editions), unit),
  hovratt_by_court = court_hovratt,
  corrections = court_corrections,
  limitations = limitations)

sources <- editions %>%
  transmute(year, work, first_page = from, last_page = to, parser,
            url = sprintf("https://runeberg.org/%s/%04d.html", work, from),
            title = ifelse(grepl("^sonkal", work), "Sveriges och Norges statskalender",
                           "Sveriges statskalender"),
            licence = "public domain (runeberg.org OCR)") %>%
  left_join(coverage %>% select(year, rows, courts, units, matched), by = "year")

## ---- 12. Checks and save --------------------------------------------------------------
message("  --- m3c checks ---")
check(nrow(stk) > 2000, sprintf("statskalender rows > 2000 (got %d)", nrow(stk)))
check(n_distinct(stk$year) == nrow(editions), "every edition parsed")
check(all(!is.na(court_units$court_id)), "every court unit has an id")
check(all(court_units$start_date <= court_units$end_date), "court unit start <= end")
check(all(court_evidence$start_date <= court_evidence$end_date), "evidence start <= end")
check(mean(!is.na(uu$hundred_id[uu$is_harad])) > 0.92,
      sprintf("more than 92%% of the units printed as a härad matched to an m1 hundred (%.1f%%)",
              100 * mean(!is.na(uu$hundred_id[uu$is_harad]))))
check(all(court_units$court_kind %in% names(KIND_PREFIX)), "known court kinds only")
ov <- court_evidence %>% filter(!is.na(child_unit), relation == "court", source != "correction") %>%
  inner_join(., ., by = "child_unit", relationship = "many-to-many") %>%
  filter(court_id.x < court_id.y, start_date.x <= end_date.y, start_date.y <= end_date.x)
message(sprintf("  härader with two courts at one date (statskalender rows only): %d", nrow(ov)))
for (y in editions$year) {
  r <- coverage %>% filter(year == y)
  message(sprintf("  %d  %3d courts  %3d units  %s matched (härader %s)  (%d listed, %d from the name)",
                  y, r$courts, r$units,
                  ifelse(is.na(r$share_matched), "    -", sprintf("%5.1f%%", 100 * r$share_matched)),
                  ifelse(is.na(r$share_matched_harad), "   -", sprintf("%5.1f%%", 100 * r$share_matched_harad)),
                  r$with_list, r$from_name))
}
message(sprintf("  composition changes between adjacent editions: %d", nrow(changes)))
message(sprintf("  m1 single-härad courts that the statskalender groups into a bigger domsaga: %d",
                nrow(single_in_bigger)))
message(sprintf("  m1 court units with no statskalender court: %d (%d of them live in 1866-1984)",
                nrow(m1_unmatched), sum(m1_unmatched$in_period)))
message(sprintf("  evidence rows: %d (%d on an m1 hundred, %d town courts, %d corrections)",
                nrow(court_evidence), sum(!is.na(court_evidence$child_unit)),
                sum(court_evidence$relation == "town_court"),
                sum(court_evidence$source == "correction")))
message(sprintf("  edition-claims: %d printed, %d inferred from the court continuing",
                sum(court_evidence$n_printed, na.rm = TRUE),
                sum(court_evidence$n_inferred, na.rm = TRUE)))

write_csv(coverage,          "data-raw/model/out/m3c_coverage.csv")
write_csv(match_report,      "data-raw/model/out/m3c_match_report.csv")
write_csv(single_in_bigger,  "data-raw/model/out/m3c_single_harad_courts.csv")
write_csv(changes,           "data-raw/model/out/m3c_composition_changes.csv")
write_csv(report$unmatched_units, "data-raw/model/out/m3c_unmatched_units.csv")
write_csv(m1_unmatched,      "data-raw/model/out/m3c_m1_unmatched.csv")
write_csv(court_units,       "data-raw/model/out/m3c_court_units.csv")
write_csv(court_evidence,    "data-raw/model/out/m3c_court_evidence.csv")
court_records <- rec_meta %>%
  select(year, court_id, court_name, name = cname, name_source, court_kind, county, hovratt,
         seat, unit_source, page, url)
saveRDS(list(court_units = court_units %>% select(-grp), court_evidence = court_evidence,
             court_names = court_names, court_hovratt = court_hovratt,
             court_records = court_records, court_members = uu %>% select(-ord),
             sources = sources, report = report),
        "data-raw/model/out/m3c.rds")
message("  Saved data-raw/model/out/m3c.rds")
timer_end(t0)
