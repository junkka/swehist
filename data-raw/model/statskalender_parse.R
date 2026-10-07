## statskalender_parse.R — reading Sveriges statskalender from runeberg.org
## Sourced by data-raw/model/m3d_deaneries.R. It parses the OCR'd page texts and reads nothing
## of swehist.
##
## The caller sets `stk_cache` to the directory the page texts are downloaded into before
## sourcing this file. Every parser returns one row per printed entry with its page and URL.
##
## Written for the held-out preparation in September 2026 and moved here when the build began to
## use the same volumes: the courts in m3c_courts.R, the deaneries in m3d_deaneries.R.

suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr)})

## 1. Download (runeberg "All text and index files": one text file per scanned page) --------
stk_download <- function(work){
  d <- file.path(stk_cache, gsub("/", "_", work))
  if (dir.exists(file.path(d, "Pages"))) return(d)
  z <- paste0(d, ".zip")
  url <- sprintf("https://runeberg.org/download.pl?mode=txtzip&work=%s", work)
  say("download %s", url)
  utils::download.file(url, z, mode = "wb", quiet = TRUE)
  utils::unzip(z, exdir = d)
  d
}
stk_url <- function(work, page) sprintf("https://runeberg.org/%s/%04d.html", work, page)

## 2. Page lines: runeberg markup removed ([-deleted-]{+inserted+}, HTML tags) -------------
stk_lines <- function(work, from, to){
  d <- stk_download(work)
  bind_rows(lapply(from:to, function(p){
    f <- file.path(d, "Pages", sprintf("%04d.txt", p))
    if (!file.exists(f)) return(NULL)
    t <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    t <- gsub("\\[-.*?-\\]", "", t, perl = TRUE)
    t <- gsub("\\{\\+(.*?)\\+\\}", "\\1", t, perl = TRUE)
    t <- gsub("<[^>]+>", "", t)
    t <- gsub("&amp;", "&", t, fixed = TRUE)
    t <- gsub("\r", "", t, fixed = TRUE)
    l <- strsplit(t, "\n")[[1]]
    tibble(page = p, text = l)
  }))
}
# Running heads, page numbers and section numbers such as "[190-191]", "559[649-650]", "83"
stk_is_head <- function(x){
  y <- trimws(x)
  keep <- grepl("län|Län|kontrakt|Kontrakt|stift|Stift|domsaga|fögderi|under", y)
  y == "" | grepl("^[\\[\\(]?[0-9IlJ ,.\\-—–^¾°SQ]*[\\]\\)J]?\\s*[0-9]*$", y, perl = TRUE) |
    grepl("^\\S{0,4}\\[[^\\]]{1,15}[\\]J]\\s*\\S{0,4}$", y, perl = TRUE) |
    (!keep & grepl("^\\S{0,5}\\[[^\\]]{1,15}[\\]J]\\s+[A-ZÅÄÖ][^\\[\\]]{2,60}$", y, perl = TRUE)) |
    (!keep & grepl("^[A-ZÅÄÖ][^\\[\\]]{2,60}\\s+\\S{0,5}\\[[^\\]]{1,15}[\\]J]\\S{0,5}$", y, perl = TRUE)) |
    grepl("^(Ecklesiastikstaten|Länsstyrelserna|Domsagor och häradshövdingar|Häradshöfding ?ar)\\.?$", y)
}
# County header lines ("i Upsala Län,", "3. Upsala län.", "Kalmar län."), OCR-tolerant
stk_county_line <- function(x){
  y <- trimws(gsub("^\\[[^\\]]*\\]\\s*", "", x, perl = TRUE))
  y <- sub("^\\W{0,3}[A-Za-z]{2,3}\\.\\s*[\\d,.]+[.:]?\\s*", "", y, perl = TRUE)     # "Inv. 96,094."
  y <- gsub("(?<=[A-Za-zåäö])0|0(?=[A-Za-zåäö])", "o", y, perl = TRUE)
  m <- str_match(y, paste0("^(?:[^A-Za-zÅÄÖ]{0,4}|[iJ][^A-Za-zÅÄÖ]{0,3}|(?:\\d{1,2}|[IU]\\d?)\\s*\\S?\\s+)",
                           "([A-ZÅÄÖWC][^,;:()]{2,34}?)\\s+(?:L|I|l|\\|)(?:[äaAüuå][nD]|aa)[,.;\"'-]?\\s*(?:\\(.*|f\\(.*|[^A-Za-z]*)$"))[, 2]
  m <- gsub("[^A-Za-zåäöÅÄÖ. -]", "", m)
  ifelse(is.na(m), NA_character_, trimws(gsub("\\s+", " ", m)))
}
# Join a section into one string with page (\u00b6) and county (\u00a7C:) markers, dehyphenated
stk_stream <- function(L, fixes = character()){
  L <- L %>% filter(!stk_is_head(text))
  for (k in names(fixes)) L$text <- gsub(k, fixes[[k]], L$text, fixed = TRUE)
  cty <- stk_county_line(L$text)
  L$text <- ifelse(is.na(cty), L$text, paste0(" \u00a7C:", cty, "\u00a7 "))
  L$text <- paste0(ifelse(L$page != lag(L$page, default = -1), sprintf(" \u00b6%04d\u00b6 ", L$page), ""), L$text)
  s <- paste(L$text, collapse = "\n")
  s <- gsub("([a-zåäöé])-\\s*\n\\s*(?!(och|o\\.)\\s)([a-zåäö])", "\\1\\3", s, perl = TRUE)
  s <- gsub("([a-zåäöé])-(?!(och|o\\.)\\s)([a-zåäö])", "\\1\\3", s, perl = TRUE)
  s <- gsub("[ \t]*\n[ \t]*", " ", s)
  s <- gsub("\\s+", " ", s)
  s <- gsub("(?<=[a-zåäö]) [0O]\\.? (?=[A-ZÅÄÖ])", " o. ", s, perl = TRUE)      # OCR 0. for o.
  s <- gsub(" oeh ", " och ", s, fixed = TRUE)
  s <- gsub(" (\\(ho|\\(l:o|cLo|d\\.o|d:0|d\\.0|d\\s:o)(?=[\\s.,;(])", " d:o", s, perl = TRUE)
  for (k in names(fixes)) s <- gsub(k, fixes[[k]], s, fixed = TRUE)
  s
}
# Page of each piece: the last page marker at or before its start
stk_pages <- function(pieces, first_page){
  pg <- first_page; out <- integer(length(pieces))
  for (i in seq_along(pieces)) {
    m <- str_match(pieces[i], "^\\s*\u00b6(\\d{4})\u00b6")[, 2]
    out[i] <- if (!is.na(m)) as.integer(m) else pg
    all <- str_match_all(pieces[i], "\u00b6(\\d{4})\u00b6")[[1]][, 2]
    if (length(all)) pg <- as.integer(all[length(all)])
  }
  out
}
stk_unmark <- function(x) trimws(gsub("\\s+", " ", gsub("\u00b6\\d{4}\u00b6", " ", x)))

## Units of a list such as "Sjuhundra, Lyhundra härader, Frötuna o. Länna, Bro o. Vätö
## skeppslag": split on , ; samt jemte; kind = trailing word. " o. " / "och" inside a piece is
## kept (a tingslag or skeppslag of two parishes); the benchmark splits it when needed.
stk_kinds <- "härader|härad|hårad|härads|h:ds|h:d|tingslaget|tingslag|t:lag|skeppslag|sk:lag|lappmarker|lappmarks|lappmark|bergslag|d:o|d\\.o|d:0|mot|möt|mots"
stk_units <- function(x){
  x <- gsub("\\([^()]*\\)", " ", x)
  x <- gsub("(?<!\\S)(Norra|Södra|Östra|Västra|Vestra|Westra|Öfre|Övre|Nedre)\\s+(och|oeh|o\\.)\\s+(Norra|Södra|Östra|Västra|Vestra|Westra|Öfre|Övre|Nedre)\\s+(\\S+)",
            "\\1 \\4, \\3 \\4", x, perl = TRUE)
  x <- gsub("(?<!\\S)(Öster|Wester|Vester|Väster)-?\\s+(och|o\\.)\\s+(Öster|Wester|Vester|Väster)-\\s*(\\S+)", "\\1-\\4, \\3-\\4", x, perl = TRUE)
  x <- gsub("\\s+(och\\s+)?återstående\\s+delen?\\s+a[fv]\\s+", ", en del af ", x, perl = TRUE)
  x <- gsub(",?\\s*(innefattande|hvilka|med [A-ZÅÄÖ]\\w+ (skärgård|stad)).*?(?=[,;]|$)", "", x, perl = TRUE, ignore.case = TRUE)
  x <- gsub("\\b(med|jemte)\\s+(?=[A-ZÅÄÖ])", ", ", x, perl = TRUE)
  p <- trimws(unlist(strsplit(x, "[,;]|\\bsamt\\b|\\bjemte\\b")))
  p <- gsub("^(och|o\\.)\\s+", "", p)
  p <- gsub("^[\u00b6\\d\\s]+", "", p, perl = TRUE)
  p <- gsub("[.:)\\}\\]]+$", "", p, perl = TRUE)
  p <- trimws(p[nchar(p) > 1])
  kind <- tolower(str_match(p, paste0("\\s(", stk_kinds, ")\\.?$"))[, 2])
  name <- p
  for (k in 1:2) name <- trimws(sub(paste0("\\s(", stk_kinds, ")\\.?$"), "", name, ignore.case = TRUE))
  name <- gsub("^(en del af|en del av|del af)\\s+", "", name)
  partial <- grepl("^(en )?del a[fv] ", p)
  keep <- nchar(name) > 1 & grepl("^[A-ZÅÄÖW]", name)
  tibble(unit = name, unit_kind = kind, partial = partial)[keep, ]
}

## 3. Domsagor 1866 and 1881: "Person, [titles,] 51; f. 13; (X domsaga,) units (adr. ...)." --
# OCR fixes found while reading the parsed lists against the OCR text (exact strings)
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
             "Carl^hamn, Ilobyj." = "Carlshamn, Hoby).", "6'udhents" = "Gudhems", "Ytler-Tjurbo" = "Ytter-Tjurbo",
             "Arbrå, Jerfsö d-o" = "Arbrå, Jerfsö d:o"),
  `1881` = c("Ök?iebo" = "Öknebo", "Bönö" = "Rönö", "Öster-Bekarne" = "Öster-Rekarne", "Basbo" = "Rasbo",
             "Vester-Bekarne" = "Vester-Rekarne", "Barnsbergs" = "Ramsbergs", "Hainsele" = "Ramsele",
             "Bödöns" = "Rödöns", "Bagunda" = "Ragunda", "Belsunds" = "Refsunds", "Båneå" = "Råneå",
             "Bedvägs" = "Redvägs", "Hahnstads" = "Halmstads", "Hoks" = "Höks", "Säjvedals" = "Säfvedals",
             "Lanehär ad;" = "Lane härad;", "Boslags" = "Roslags", "Akers" = "Åkers", "Asunda" = "Åsunda",
             "?ned Haparanda" = "med Haparanda", "Tima," = "Tuna,", " As, Gäsene" = " Ås, Gäsene",
             "Häradshöj'dinge-Embe tet" = "Häradshöfdinge-Embetet", "{Falu domsaga^" = "(Falu domsaga,)",
             "o U†neå" = "Umeå"))
parse_domsagor_old <- function(year, work, from, to, cut = NULL, cut_which = "last"){
  L <- stk_lines(work, from, to)
  s <- stk_stream(L, stk_fix[[as.character(year)]])
  s <- sub("^.*?Justitie-?\\s*[Ss]taten å landet\\.?", "", s)
  if (!is.null(cut)) {                                 # the next section ends the list
    m <- gregexpr(cut, s)[[1]]
    if (m[1] > 0) s <- substr(s, 1, (if (cut_which == "last") m[length(m)] else m[1]) - 1)
  }
  # hovrätt headers
  s <- gsub("\\b([abc])\\)\\s*under\\s+(K\\.|Kongl\\.)\\s*([^;]{3,40}?)\\s*;", " \u00a7H:\\3\u00a7 ", s, perl = TRUE)
  # entries end with the address "(adr. ...)" or "(d:o)" or "(tillf. Domhafv.-s adr. ...)",
  # or without an address at "härader." / "d:o." before the next person
  s <- gsub("\\b((?:härader|härad|d:o|tingslag|skeppslag|lappmarker|Mot)\\s*\\.)\\s+(?=[A-ZÅÄÖ\u00b6])", "\\1 \u00a7E\u00a7 ", s, perl = TRUE)
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
  e$page <- stk_pages(e$entry, from)
  e$entry <- stk_unmark(e$entry)
  e$entry <- gsub("[;,]?\\s*hvilken\\s+s\\S+\\s.*?(Emb\\S*|Embetet)\\s*[,;]?", " ", e$entry, perl = TRUE)
  # units start after the years: 1866 "51; f. 13;", 1881 "f. 40; 79;"; else after "Vakant"
  anchor <- if (year < 1870) "(?<![A-Za-zåäö])o?f\\.\\s*\"?[^\\s;:]{1,5}\\s*[;:,]" else
    "(?<![A-Za-zåäö])[fi£]\\.?\\s*[^\\s;]{1,4}\\s*[;:]\\s*[^\\s;,]{1,4}\\s*[;:,]"
  m <- regexpr(anchor, e$entry, perl = TRUE)
  mv <- regexpr("Vakant\\s*[;,]", e$entry)
  e$units <- ifelse(m > 0, substring(e$entry, m + attr(m, "match.length")),
                    ifelse(mv > 0, substring(e$entry, mv + attr(mv, "match.length")), NA_character_))
  e <- e %>% filter(!is.na(units), nchar(trimws(units)) > 1)
  e$units <- sub("^[\\s;:,.'\"]*((QQ|\\(\\]Q|Q|o)\\s*[;:,.]\\s*)?", "", e$units, perl = TRUE)
  e$units <- sub("^[^A-ZÅÄÖ(\\{\\[]*", "", e$units, perl = TRUE)
  dm <- str_match(e$units, "^[\\(\\{\\[]?\\s*([^(){}]{3,60}?)[\\s,.;\\^]*[\\)\\}]")
  has_name <- !is.na(dm[, 1]) & (grepl("[DB]om", dm[, 2], ignore.case = TRUE) | grepl("^[\\(\\{]", e$units, perl = TRUE))
  e$domsaga <- ifelse(has_name, trimws(gsub("[,.;]+$", "", dm[, 2])), NA_character_)
  e$units <- ifelse(has_name, substring(e$units, nchar(dm[, 1]) + 1), e$units)
  e$units <- sub("^[\\s,.;)}\\]]+", "", e$units, perl = TRUE)
  # "Oppunda o. Villåttinge härads domsaga" names the domsaga and its härader
  hd <- str_match(e$units, "^\\s*([^,;()]+?)\\s+härads\\s+domsaga\\b")
  e$domsaga <- ifelse(is.na(e$domsaga) & !is.na(hd[, 1]), trimws(hd[, 1]), e$domsaga)
  e$units <- ifelse(!is.na(hd[, 1]), paste(hd[, 2], "härad"), e$units)
  e$domsaga <- gsub("\\bBomsaga\\b", "Domsaga", e$domsaga)
  e$domsaga_no <- seq_len(nrow(e))
  out <- bind_rows(lapply(seq_len(nrow(e)), function(i){
    u <- stk_units(e$units[i])
    if (!nrow(u)) u <- tibble(unit = NA_character_, unit_kind = NA_character_, partial = FALSE)
    bind_cols(e[rep(i, nrow(u)), c("hovratt", "county_raw", "domsaga_no", "domsaga", "page")], u)
  }))
  # "d:o" = same kind as the previous unit
  for (i in seq_len(nrow(out))) if (out$unit_kind[i] %in% c("d:o", "d.o", "d:0") && i > 1) out$unit_kind[i] <- out$unit_kind[i - 1]
  out %>% mutate(year = year, source_text = e$entry[match(domsaga_no, e$domsaga_no)],
                 url = stk_url(work, page), .before = 1)
}

## 4. Domsagor 1931: "X domsaga, 32,950 inv., omfattar 2 t:lag: A t:lag, tingsst. P; B h:d, ...
##    — 1,600 kr." or "utgör 1 t:lag" (then the härader are those in the name, unit_source name)
parse_domsagor_1931 <- function(year, work, from, to){
  L <- stk_lines(work, from, to)
  s <- stk_stream(L, c("Frös åkers" = "Frösåkers", "t: lag" = "t:lag", "t dag" = "t:lag", "t.lag" = "t:lag",
                       "t:las," = "t:lag,", "k:d," = "h:d,", "h:dtingsst." = "h:d, tingsst.",
                       "Opptmda" = "Oppunda", "Arvidsjatirs" = "Arvidsjaurs", "Jokk7)iokks" = "Jokkmokks",
                       "Korpilo??ibolo" = "Korpilombolo", "Karesua7ido lapp771 arks" = "Karesuando lappmarks"))
  s <- gsub("under\\s+(Svea|Göta|Skåne och Blekinge|Övre Norrlands|hovrätten över Skåne och Blekinge)\\S*\\s+hovrätt(en)?\\S*", " \u00a7H:\\1\u00a7 ", s, perl = TRUE)
  m <- gregexpr("(?<=[.)»\u00a7\u00b6\\d]\\s)([A-ZÅÄÖ][^.;:\u00a7\u00b6»—]{1,80}?\\sdomsaga)\\s*[,.»]?\\s*\\d[\\d.,]*\\s*inv", s, perl = TRUE)[[1]]
  st <- as.integer(m); en <- c(st[-1] - 1, nchar(s))
  seg <- substring(s, st, en)
  pre <- substring(s, 1, st)
  county <- vapply(pre, function(x) { a <- str_match_all(x, "\u00a7C:([^\u00a7]+)\u00a7")[[1]]; if (nrow(a)) a[nrow(a), 2] else NA_character_ }, "", USE.NAMES = FALSE)
  hov <- vapply(pre, function(x) { a <- str_match_all(x, "\u00a7H:([^\u00a7]+)\u00a7")[[1]]; if (nrow(a)) a[nrow(a), 2] else NA_character_ }, "", USE.NAMES = FALSE)
  pages <- stk_pages(substring(s, 1, en), from)                   # page at the end of the prefix
  pages <- vapply(seq_along(st), function(i) { a <- str_match_all(substring(s, 1, st[i]), "\u00b6(\\d{4})\u00b6")[[1]]; as.integer(a[nrow(a), 2]) }, 1L)
  seg <- stk_unmark(gsub("\u00a7[^\u00a7]*\u00a7", " ", seg))
  name <- trimws(str_match(seg, "^(.*?domsaga)")[, 2])
  bind_rows(lapply(seq_along(seg), function(i){
    om <- str_match(seg[i], "omfatta[rn]\\s*\\d*\\s*t:lag\\s*:\\s*(.*?)(\\s+[—-]\\s*\\d|\\s\\d[\\d,.]*\\s*kr\\.|$)")[, 2]
    if (!is.na(om)) {
      pcs <- trimws(strsplit(om, ";")[[1]])
      pcs <- sub(",?\\s*tingsst\\S*\\s.*$", "", pcs)
      u <- bind_rows(lapply(pcs, stk_units)); src <- "list"
    } else {
      x <- sub("\\s*domsaga$", "", name[i])
      x <- gsub("\\s+och\\s+", ", ", sub("\\s+härads?$", "", x))
      u <- stk_units(x); src <- "name"
    }
    if (!nrow(u)) u <- tibble(unit = NA_character_, unit_kind = NA_character_, partial = FALSE)
    tibble(year = year, hovratt = hov[i], county_raw = county[i], domsaga_no = i, domsaga = name[i],
           page = pages[i], u, unit_source = src, source_text = substr(seg[i], 1, 300))
  })) %>% mutate(url = stk_url(work, page), .after = year)
}

## 5. Fögderier 1866 and 1881: "1:a Fögderiet. (A, B och C härader samt D skeppslag)." or
##    "A, B och C häraders Fögderi." A fögderi named after a town or region ("Laholms Fögderi")
##    has no listed härader: one row with unit NA.
parse_fogderier_old <- function(year, work, from, to){
  L <- stk_lines(work, from, to)
  s <- stk_stream(L, c("I4- 6flteb0rSS 0Ch B0lmS läD-" = "14. Göteborgs och Bohus Län.", "Westtaänlanas" = "Westmanlands",
                       "Cdlmar\t(Calmar)" = "8. Calmar Län. (Calmar)", "WeiManlan^" = "Wester-Norrlands",
                       "Fogderi" = "Fögderi", "Närdingbundra" = "Närdinghundra", "Okuebo" = "Öknebo",
                       "S vartlösa" = "Svartlösa", "Wedho" = "Wedbo", "FLandbörds" = "Handbörds",
                       "BräJcne" = "Bräkne", "JVemmenhögs" = "Wemmenhögs", "Lysings, Bals" = "Lysings, Dals",
                       "Finspång a" = "Finspånga", "L&nghundia" = "Långhundra", "Hasunda" = "Hagunda",
                       "Mraders" = "häraders", "Bjiire" = "Bjäre", "Gåinge" = "Göinge", "Qultbergs" = "Gullbergs",
                       "Ingelitads" = "Ingelstads", "Oxi»" = "Oxie", "Sådra" = "Södra", "hurads" = "härads"))
  re_ord <- "(\\S{1,3}\\s*[:;]\\s*[ae]\\s+Fögderiet)\\.?\\s*\\(([^)]*)\\)"
  re_nam <- "(?<=[.)\u00a7\u00b6\\d]\\s|^)([A-ZÅÄÖW][^.\u00a7\u00b6()]{0,120}?)\\s+Fögderi\\b"
  mo <- gregexpr(re_ord, s, perl = TRUE)[[1]]
  mn <- gregexpr(re_nam, s, perl = TRUE)[[1]]
  hits <- bind_rows(
    if (mo[1] > 0) tibble(pos = as.integer(mo), len = attr(mo, "match.length"), kind = "ordinal"),
    if (mn[1] > 0) tibble(pos = as.integer(mn), len = attr(mn, "match.length"), kind = "named")) %>%
    arrange(pos)
  # a named hit inside an ordinal hit is the same header
  hits <- hits %>% filter(!(kind == "named" & grepl("Fögderiet", substring(s, pos, pos + len + 2))))
  bind_rows(lapply(seq_len(nrow(hits)), function(i){
    h <- substr(s, hits$pos[i], hits$pos[i] + hits$len[i] - 1)
    pre <- substr(s, 1, hits$pos[i])
    a <- str_match_all(pre, "\u00a7C:([^\u00a7]+)\u00a7")[[1]]; cty <- if (nrow(a)) a[nrow(a), 2] else NA_character_
    b <- str_match_all(pre, "\u00b6(\\d{4})\u00b6")[[1]]; pg <- if (nrow(b)) as.integer(b[nrow(b), 2]) else from
    h <- stk_unmark(h)
    if (hits$kind[i] == "ordinal") {
      nm <- str_match(h, re_ord)[, 2]; lst <- str_match(h, re_ord)[, 3]
    } else {
      nm <- sub("\\s+Fögderi$", "", h)
      lst <- if (grepl("\\s(häraders|härads|härader|härad|mots?|möts?)$", nm)) nm else NA_character_
      lst <- sub("\\s+(häraders|härads)$", " härader", lst)
      lst <- sub("\\s+(mots?|möts?)$", " mot", lst)
      nm <- paste(nm, "Fögderi")
    }
    u <- if (is.na(lst)) tibble(unit = NA_character_, unit_kind = NA_character_, partial = FALSE) else stk_units(lst)
    if (!nrow(u)) u <- tibble(unit = NA_character_, unit_kind = NA_character_, partial = FALSE)
    tibble(year = year, county_raw = cty, fogderi_no = i, fogderi = trimws(nm), page = pg, u,
           unit_source = if (is.na(lst)) "none" else "header", source_text = h)
  })) %>% mutate(url = stk_url(work, page), .after = year)
}

## 6. Fögderier 1931: "X fögderi." then Häradsskrivare and Landsfiskaler, each landsfiskal with
##    his district: "..., f. 76; 21, Väddö d:o (adr. Väddö)." / "tf. 25, Frösåkers distr. (adr."
parse_fogderier_1931 <- function(year, work, from, to){
  L <- stk_lines(work, from, to)
  s <- stk_stream(L, c("När-dinghtmdra" = "Närdinghundra", "Ärlinghu?idra" = "Ärlinghundra",
                       "Vallen-tu7ia" = "Vallentuna", "Hani?ige" = "Haninge"))
  mf <- gregexpr("(?<=[.)\u00a7\u00b6\\d]\\s)([A-ZÅÄÖ][^.\u00a7\u00b6()\\[\\]]{1,60}?\\s+fögderi)\\s*\\.", s, perl = TRUE)[[1]]
  st <- as.integer(mf); en <- c(st[-1] - 1, nchar(s))
  bind_rows(lapply(seq_along(st), function(i){
    seg <- substring(s, st[i], en[i])
    seg <- sub("\u00a7C:.*$", "", seg)                            # stop at the next county
    pre <- substr(s, 1, st[i])
    a <- str_match_all(pre, "\u00a7C:([^\u00a7]+)\u00a7")[[1]]; cty <- if (nrow(a)) a[nrow(a), 2] else NA_character_
    b <- str_match_all(pre, "\u00b6(\\d{4})\u00b6")[[1]]; pg <- if (nrow(b)) as.integer(b[nrow(b), 2]) else from
    nm <- trimws(str_match(seg, "^(.*?fögderi)")[, 2])
    seg <- stk_unmark(seg)
    d <- str_match_all(seg, "[,.;]\\s*([A-ZÅÄÖ][^,;()\\[\\]]{1,40}?)\\s+(distr\\.|d:o|distrikt)\\s*\\((?:\\^)?adr")[[1]][, 2]
    d <- trimws(gsub("\\s+", " ", d))
    d <- d[!grepl("^(Landsfiskal|Landskanslist|Häradsskrivare|Kronolänsman|Vakant)", d)]
    u <- if (length(d)) tibble(unit = d, unit_kind = "landsfiskalsdistrikt", partial = FALSE) else
      tibble(unit = NA_character_, unit_kind = NA_character_, partial = FALSE)
    tibble(year = year, county_raw = cty, fogderi_no = i, fogderi = nm, page = pg, u,
           unit_source = if (length(d)) "landsfiskal" else "none", source_text = substr(seg, 1, 200))
  })) %>% mutate(url = stk_url(work, page), .after = year)
}

## 7. Kyrkor (Ecklesiastikstaten). Stockholm's own consistory is not included.
## Stift and kontrakt headers; a pastorat is named by its mother parish (1866, 1881) or by all its
## parishes (1931: "Gamla Uppsala o. Ärentuna. (Upps. l.)", county abbreviation in parentheses,
## else the kontrakt's county). Entries before "Kyrkoherdar" (the consistory) are skipped.
stk_stift_re <- "^(?:\\[[^\\]]*\\]\\s*|\\S{0,2}\\s+)?([A-ZÅÄÖWC][A-Za-zåäöÅÄÖ]+s?)\\s+(Erke-?\\s*|Ärke-?\\s*|ärke)?-?\\s*[Ss]tift\\.?\\s*(\\[\\d+\\])?\\s*$"
stk_first_names <- paste("Pehr|Per|Carl|Karl|Johan|Anders|Nils|Lars|Erik|Eric|Gustaf|Olof|Jonas|Magnus|Anton|Johannes",
                         "Fredrik|Axel|Claes|Sven|Jöns|Harald|Israel|Biskopen|Christopher|Simon|Petter|Peter|Jacob|Jakob|Daniel",
                         "Otto|Adolf|August|Isak|Hans|Mårten|Samuel|Abraham|Andreas|Bengt|Carolus|Emanuel|Ernst|Knut|Ludvig|Mathias", sep = "|")
stk_title_re <- "\\b(Dokt|Mag|Fil|Th|Theol|Prost|Kyrkoh|Kontrakts|Kommin|Vakant|Domprost|Adjunkt|Pastor|Kapellpred|Hofpred)\\b"

parse_kyrkor_1866 <- function(year, work, from, to){
  L <- stk_lines(work, from, to) %>% filter(!stk_is_head(text))
  L$text <- gsub("\\s+", " ", gsub("\t", " \t ", L$text))
  cut <- which(grepl("Stockholms Stads Konsistorium", L$text))
  if (length(cut)) L <- L[seq_len(cut[1] - 1), ]
  stift <- NA_character_; kontrakt <- NA_character_; active <- FALSE; rows <- list(); open <- 0
  for (i in seq_len(nrow(L))) {
    t <- trimws(L$text[i])
    cont <- open > 0                                     # the rest of an address "(adr. ..." of the line above
    open <- if (cont) 0 else as.integer(str_count(t, "\\(") > str_count(t, "\\)"))
    if (cont && !grepl("\\t", t)) next
    st <- str_match(t, stk_stift_re)[, 2]
    if (!is.na(st)) { stift <- st; kontrakt <- NA_character_; active <- FALSE; next }
    if (grepl("^Kyrkoherdar", t)) { active <- TRUE; next }
    if (!active) next
    k <- str_match(t, "^\\W{0,2}([A-ZÅÄÖWI][^;()]{1,70}?)\\s+Kontrakt[.,]?\\s*$")[, 2]
    if (grepl("^Domprosteriet\\.?$", t)) k <- "Domprosteriet"
    if (!is.na(k)) { kontrakt <- trimws(gsub("\\s+", " ", k)); next }
    m <- str_match(t, "^([A-ZÅÄÖW][^,\t]{1,40}?)\\s*[,.]\\s*\t?\\s*(\"?Vakant|[A-ZÅÄÖ])")
    if (is.na(m[1, 1])) next
    nm <- trimws(m[1, 2])
    if (grepl(stk_title_re, nm) || length(strsplit(nm, " ")[[1]]) > 4 || grepl("-\\s*o$|\\d|Skogs-", nm)) next
    if (grepl(paste0("^(", stk_first_names, ")\\s"), nm)) next
    # a person line: two capitalised words followed by a year ("Anders Norrsell, Prost, 24")
    if (grepl("^[A-ZÅÄÖ][a-zåäö]+ [A-ZÅÄÖ][a-zåäö]+$", nm) && !grepl("\t", t) &&
        grepl(paste0("^", nm, ",\\s*(", substr(stk_title_re, 4, 200), ")?[^,]*,\\s*\\d"), t)) next
    rows[[length(rows) + 1]] <- tibble(page = L$page[i], stift_raw = stift, kontrakt = kontrakt, pastorat = nm,
                                       source_text = t)
  }
  bind_rows(rows) %>% mutate(year = year, url = stk_url(work, page), county_raw = NA_character_, .before = 1)
}

parse_kyrkor_1881 <- function(year, work, from, to){
  L <- stk_lines(work, from, to)
  cut <- which(grepl("Stockholms Stads Konsistorium", L$text))
  if (length(cut)) L <- L[seq_len(cut[1] - 1), ]
  L$text <- gsub("^\\s*\\[[^\\]]{1,8}[\\]J]\\s*", "", L$text, perl = TRUE)
  L$text <- ifelse(!is.na(str_match(trimws(L$text), stk_stift_re)[, 2]),
                   paste0(" §S:", str_match(trimws(L$text), stk_stift_re)[, 2], "§ "), L$text)
  s <- stk_stream(L, c("Ko?itrakt" = "Kontrakt"))
  ev <- bind_rows(
    { m <- str_locate_all(s, "§S:([^§]+)§")[[1]]; tibble(pos = m[, 1], kind = rep("stift", nrow(m)), val = str_match(substring(s, m[, 1], m[, 2]), "S:([^§]+)")[, 2]) },
    { m <- str_locate_all(s, "Kyrkoherdar\\s*,")[[1]]; tibble(pos = m[, 1], kind = rep("start", nrow(m)), val = NA_character_) },
    { m <- str_locate_all(s, "(?<=[.)§¶]\\s)[A-ZÅÄÖW][^.;()§¶]{1,70}?\\s+Kontrakt(?:\\.|\\s+(?=[A-ZÅÄÖ]))|Domprosteriet\\.")[[1]]
      tibble(pos = m[, 1], kind = rep("kontrakt", nrow(m)), val = sub("\\s+Kontrakt\\.?\\s*$|\\.$", "", substring(s, m[, 1], m[, 2]))) },
    { re <- "[,;]\\s*(?:i|1|l|î|t)\\s+([A-ZÅÄÖW][^()\u00a7.;]{1,70}?)\\s*(?:[.,]?\\s*\\((?:adr|d:o|tillf|Domhafv)|\\.\\s)"
      m <- str_locate_all(s, re)[[1]]
      tibble(pos = m[, 1], kind = rep("pastorat", nrow(m)), val = str_match(substring(s, m[, 1], m[, 2]), re)[, 2]) }) %>%
    arrange(pos)
  stift <- NA_character_; kontrakt <- NA_character_; active <- FALSE; rows <- list()
  for (i in seq_len(nrow(ev))) {
    if (ev$kind[i] == "stift") { stift <- ev$val[i]; kontrakt <- NA_character_; active <- FALSE }
    else if (ev$kind[i] == "start") active <- TRUE
    else if (ev$kind[i] == "kontrakt" && active) kontrakt <- trimws(ev$val[i])
    else if (ev$kind[i] == "pastorat" && active) {
      pre <- substr(s, 1, ev$pos[i]); b <- str_match_all(pre, "¶(\\d{4})¶")[[1]]
      rows[[length(rows) + 1]] <- tibble(page = if (nrow(b)) as.integer(b[nrow(b), 2]) else from,
        stift_raw = stift, kontrakt = kontrakt, pastorat = sub(",.*$", "", stk_unmark(ev$val[i])),
        source_text = stk_unmark(substr(s, max(1, ev$pos[i] - 80), ev$pos[i] + 60)))
    }
  }
  out <- bind_rows(rows)
  out <- out %>% filter(!grepl(paste0("^(", stk_first_names, ")\\s|\\d|Biskop"), pastorat))
  out %>% mutate(year = year, url = stk_url(work, page), county_raw = NA_character_, .before = 1)
}

parse_kyrkor_1931 <- function(year, work, from, to){
  L <- stk_lines(work, from, to) %>% filter(!stk_is_head(text))
  L$text <- trimws(gsub("\\s+", " ", L$text))
  cut <- which(grepl("Stockholms st.ds konsistorium\\.$", L$text))
  if (length(cut)) L <- L[seq_len(cut[1] - 1), ]
  # a kontrakt name broken before "kontrakt."
  k1 <- which(grepl("^kontrakt\\.", L$text))
  k1 <- k1[k1 > 1]
  if (length(k1)) { L$text[k1] <- paste(L$text[k1 - 1], L$text[k1]); L <- L[-(k1 - 1), ] }
  # logical lines: join while parentheses are open or the line ends with "o." / "," / "-"
  hdr <- grepl("(ärke)?stift\\.$|kontrakt\\.", L$text) & !grepl("\\d\\d;|adr\\.", L$text)
  out <- list(); buf <- ""; pg <- NA
  for (i in seq_len(nrow(L))) {
    t <- L$text[i]
    if (hdr[i] && buf != "") { out[[length(out) + 1]] <- tibble(page = pg, text = buf); buf <- "" }
    if (buf == "") pg <- L$page[i]
    buf <- if (buf == "") t else if (grepl("-$", buf)) paste0(sub("-$", "", buf), t) else paste(buf, t)
    open <- str_count(buf, "\\(") > str_count(buf, "\\)")
    if (open || grepl("(\\so\\.|,|-|\\soch)$", buf)) next
    out[[length(out) + 1]] <- tibble(page = pg, text = buf); buf <- ""
  }
  X <- bind_rows(out)
  person <- paste0("(\\bf\\.\\s*\\d|\\bKyrkoh|\\bKy\\S*koh|\\bKommin|\\bKo\\S*m\\S*n\\b|Vakant|adr\\.|Pastorsadj|Kapellpred|adjunkt|",
                   "\\bf\\.\\s*$|\\d\\d;|T\\. K\\.|T\\. o\\. Fil|Fil\\. [KDL]|Ständig|\\bProst\\b|Kontr\\.|\\b[LRK][NV]O\\b|\\bstift\\b)")
  stift <- NA_character_; kontrakt <- NA_character_; kcounty <- NA_character_; rows <- list()
  for (i in seq_len(nrow(X))) {
    t <- sub("^\\[[^\\]]{1,10}[\\]J]\\s*", "", X$text[i], perl = TRUE)
    st <- str_match(t, "^([A-ZÅÄÖ][a-zåäö]+s?)\\s+(ärke)?stift\\.$")[, 2]
    if (!is.na(st)) { stift <- st; kontrakt <- NA_character_; next }
    k <- str_match(t, "^([A-ZÅÄÖ][^()]{1,60}?)\\s+kontrakt\\.\\s*(\\(([^)]*)\\))?\\s*$")
    if (!is.na(k[1, 1])) {
      kontrakt <- trimws(k[1, 2]); kcounty <- k[1, 4]
      if (is.na(kcounty) && i < nrow(X) && grepl("^\\([^)]*[1l]\\.\\)$", X$text[i + 1])) kcounty <- gsub("[()]", "", X$text[i + 1])
      next
    }
    if (is.na(kontrakt) || grepl(person, t) || grepl("^\\(", t) || !grepl("^[A-ZÅÄÖ]", t)) next
    if (grepl("^[A-ZÅÄÖ][a-zåäöéü]+, (?!(Norra|Södra|Östra|Västra|Stora|Lilla|Övre|Nedre) )[A-ZÅÄÖ][a-zåäöéü]+ [A-ZÅÄÖ][a-zåäöéü]+", t, perl = TRUE) ||
        grepl("\\.pred|Förste|Kommi|\\d", t)) next
    if (grepl("^(Kontraktsprost|Domkapitlet|Stiftssekreterare|Stiftsnotarie|Biträdande|Pastor|Ledamöter|Preses)", t)) next
    cm <- str_match_all(t, "\\(([^)]*(?:[1l]\\.|län))\\)")[[1]]
    cty <- if (nrow(cm)) paste(unique(cm[, 2]), collapse = "; ") else kcounty
    nm <- trimws(gsub("\\s*\\([^)]*\\)\\s*", " ", t))
    nm <- sub("[.,]+$", "", nm)
    if (!nzchar(nm) || nchar(nm) > 120) next
    rows[[length(rows) + 1]] <- tibble(page = X$page[i], stift_raw = stift, kontrakt = kontrakt,
                                       pastorat = nm, county_raw = cty, source_text = X$text[i])
  }
  bind_rows(rows) %>% mutate(year = year, url = stk_url(work, page), .before = 1)
}

# Pastorat -> parishes: "Gamla Uppsala o. Ärentuna", "Lagga, Östuna o. Fundbo", "Börstil och
# Östhammars stad". One row per parish; the first is the mother parish.
stk_pastorat_parishes <- function(k){
  k <- k %>% mutate(pastorat_no = row_number())
  k$pastorat <- gsub("([A-ZÅÄÖ][^,]*?)\\s+st.ds\\S*\\s+o\\.\\s+landsförsamlingar(\\s+samt)?", "\\1 stadsförsamling, \\1 landsförsamling", k$pastorat, perl = TRUE)
  bind_rows(lapply(seq_len(nrow(k)), function(i){
    x <- gsub("\\s*(\\[[^\\]]*\\]|\\{[^}]*\\}|\\([^)]*\\))", "", k$pastorat[i], perl = TRUE)
    x <- trimws(gsub("\\s+", " ", gsub("[\"«~'/?]", " ", x)))
    p <- trimws(strsplit(gsub("\\s+(o\\.|och|&)\\s+", ", ", x), ",")[[1]])
    p <- sub("[.]+$", "", p)
    p <- p[nchar(p) > 1]
    tibble(k[rep(i, length(p)), ], parish = p, is_mother = seq_along(p) == 1)
  }))
}
