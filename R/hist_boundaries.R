# hist_boundaries.R — Internal helpers for date parsing and county towns

#' Towns in Sweden
#'
#' Get Swedish county towns for a specified year 1600-1990.
#'
#' @param date a date, a year or a vector with date/year range
#' @param format format of return object: "sf" (default) or "meta"
#' @export

county_towns <- function(date, format = c("sf", "meta")){
  format <- match.arg(format)

  date_l <- get_date(date)
  x <- date_l$x
  y <- date_l$y

  if (x > 1990 || x < 1600 || y > 1990 || y < 1600)
    stop("Date must be between 1600-01-01 and 1990-12-31")

  env <- environment()
  data(hist_town, package = "swehist", envir = env)
  res <- filter(hist_town, from <= x, tom >= y)

  switch(format,
    sf   = res,
    meta = as_tibble(sf::st_drop_geometry(res))
  )
}


#' Get year
#'
#' Transforms an atomic vector to an integer year.
#'
#' @param x An object to be converted
#' @param ... other parameters for methods
#' @keywords internal

get_year <- function(x, ...) UseMethod("get_year", x)

#' @exportS3Method
get_year.integer <- function(x, ...) year_from_number(x)

#' @exportS3Method
get_year.numeric <- function(x, ...) year_from_number(x)

#' @exportS3Method
get_year.character <- function(x, ...){
  x <- trimws(x)
  if (grepl("^[0-9]{1,4}$", x)) return(as.integer(x))
  if (grepl("^[0-9]{8}$", x)) return(as.integer(substr(x, 1, 4)))
  y <- suppressWarnings(as.integer(lubridate::year(lubridate::ymd(x, quiet = TRUE, ...))))
  if (length(y) == 1 && !is.na(y)) return(y)
  # Any other format (e.g. "06/06/1866"): take the first 4-digit number
  m <- regmatches(x, regexpr("[0-9]{4}", x))
  if (length(m) == 0) return(NA_integer_)
  as.integer(m)
}

#' @exportS3Method
get_year.default <- function(x, ...){
  as.integer(lubridate::year(x))
}

# Years are 1-4 digits; 8-digit numbers are read as YYYYMMDD
year_from_number <- function(x){
  if (is.na(x)) return(NA_integer_)
  if (x < 10000) return(as.integer(x))
  as.integer(substr(format(x, scientific = FALSE, trim = TRUE), 1, 4))
}


get_date <- function(date){
  if (length(date) == 0 || length(date) > 2)
    stop("date must be a single date/year or a range of two", call. = FALSE)
  yrs <- vapply(seq_along(date), function(i) get_year(date[i]), integer(1))
  if (anyNA(yrs))
    stop("could not read a year from date: ", paste(date, collapse = ", "),
         call. = FALSE)
  if (length(date) == 1) {
    x <- yrs
    y <- x
    period <- FALSE
  } else {
    y <- yrs[1]
    x <- yrs[2]
    if (y > x) stop("Range start must be before end", call. = FALSE)
    period <- TRUE
  }
  list(x = x, y = y, period = period)
}
