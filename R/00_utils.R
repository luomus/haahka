library(assertthat)
library(shiny)
library(tidyverse)

# Return full language names instead of a language code.
#
get_languages <- function(x) {

  langs <- list("fi" = "Suomi",
                "en" = "English",
                "se" = "Svenska")

  assertthat::assert_that(x %in% names(langs))

  return(langs[[x]])
}

# Get month names in a given language
#
get_months <- function(lang = "en", format) {

  supported_languages <- c("en", "fi", "se")

  assertthat::assert_that(lang %in% supported_languages,
                          msg = paste0("lang must be one of: ",
                                       paste(supported_languages,
                                             collapse = ", ")))
  assertthat::assert_that(format %in% c("long", "short"),
                          msg = "format must be either long or short")

  month_names <- list(
    "short" = list(
      "en" = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
               "Oct", "Nov", "Dec"),
      "fi" = c("Tammi", "Helmi", "Maalis", "Huhti", "Touko", "Kesä",
               "Heinä", "Elo", "Syys", "Loka", "Marras", "Joulu"),
      "se" = c("Jan", "Feb", "Mar", "Apr", "Maj", "Jun", "Jul", "Aug", "Sep",
               "Okt", "Nov", "Dec")
    ),
    "long" = list(
      "en" = c("January", "February", "March", "April", "May", "June",
               "July", "August", "September", "October", "November",
               "December"),
      "fi" = c("Tammikuu", "Helmikuu", "Maaliskuu", "Huhtikuu",
               "Toukokuu", "Kesäkuu", "Heinäkuu", "Elokuu", "Syyskuu",
               "Lokakuu", "Marraskuu", "Joulukuu"),
      "se" = c("Januari", "Februari", "Mars", "April", "Maj", "Juni",
               "Juli", "Augusti", "September", "Oktober", "November",
               "December")
    )

  )
  return(month_names[[format]][[lang]])
}

# Return numeric timestamp value based on a Date. Needed for highcharts
# x-axis settings and plotting.
#
get_timestamp <- function(x) {
  return(datetime_to_timestamp(as.Date(x, tz = "UTC")))
}

# Check that a value is odd numeric
#
is_odd <- function(x) {
  assertthat::assert_that(is.numeric(x), length(x) == 1)
  x %% 2 == 1
}

# Make a date label for tooltips with language support
#
make_date_label <- function(x, lang) {

  assertthat::see_if(inherits(x, "Date"))

  # Get the name of month in a given language
  month_name <- get_months(lang, "long")[lubridate::month(x)]
  label <- glue::glue("{month_name} {day(x)}")
  return(label)
}

# Parse author
#
# Convert normal name to citation format, e.g. "Aki Aintila" to "Aintila, A"
# FIXME: won't work with names other than "FIRSTNAME LASTNAME"
#
parse_author <- function(x) {
  tokens <- unlist(strsplit(x, " "))
  initial <- paste0(substring(tokens[1], 1, 1), ".")
  return(paste(c(tokens[2], initial), collapse = ", "))
}


# Parse description
parse_description <- function(text, lang) {

  .ans <<- NULL

  on.exit(rm(".ans", pos = ".GlobalEnv"))

  lapply(text, get_text, h = 3, lang = lang)

  .ans

}

get_text <- function(x, h, lang) {

  if (!is.null(x[["title"]]) || !is.null(x[["content"]])) {

    get_title(x, h, lang)

    get_content(x, h, lang)

    h <- h + 1

  }

  for (i in seq_along(x)) {

    if (is.list(x[[i]])) {

      get_text(x[[i]], h, lang)

    }

  }

}

get_title <- function(x, h, lang) {

  if (!is.null(x[["title"]])) {

    .ans <<- c(.ans, paste0("<h", h, ">", x[["title"]][[lang]], "</h", h, ">"))

  }

}

get_content <- function(x, h, lang) {

  if (!is.null(x[["content"]])) {

    .ans <<- c(.ans, x[["content"]][[lang]])

  }

}

# Capitalize the first letter of a sentence.
#
simple_cap <- Vectorize(
  function(x) {
    s <- strsplit(x, " ")[[1]]
    s <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
               sep = "", collapse = " ")
    return(s)
  }, SIMPLIFY = TRUE, USE.NAMES = FALSE)

# Calculate a tiled average (non-overlapping windows) for yearly observations.
#
# An average is calculated over a specified window and assigned to the day in
# the middle of the windows. For example, if size is 5, then the average is
# assigned to day at index 3 etc.
#
#
# @param x a tsibble
# @parame day string name of the variable containing datetime values
# @param value string name of the variable containing numeric observation values
# @param size numeric size of the window over which average is calculated. Must
#             be an odd value.
#
# @return a tibble with dates and corresponding (rounded) averages.
#
tile_observations <- function(x, day, value, size) {

  # Coerce size to integer
  size <- as.integer(size)

  # x must be a tsibble
  assertthat::see_if(inherits(x, "tbl_ts"))

  # size must be an odd integer
  assertthat::on_failure(is_odd) <- function(call, env) {
    paste0(deparse(call$x), " is even. size must be odd.")
  }

  assertthat::assert_that(is_odd(size))

  # If size is 1, no need to calculate anything
  if (size == 1) {
    days <- x[[day]]
    avgs <- x[[value]]
  } else {
    # NOTE: the number of days is hard coded here and include leap day
    n_days <- 366

    # Construct a day index, i.e. index vector defining which days are to be
    # kept. Staring point is the middle (median) value of the window.
    day_index <- seq(median(1:size), nrow(x), by = size)

    # Find the remainder given the size
    remainder <- n_days %% size
    # If there is a remainder, the last tile is not the same size as the others
    # and the index vector needs to be augmented. Simple add 366 as the
    # last value in the index vector.
    if (remainder != 0) {
      day_index <- c(day_index, n_days)
    }

    # Get correct days based on the index vector
    days <- x[[day]][day_index]
    # Calculate the averages
    avgs <- tapply(
      x[[value]], rep(seq_along(days), each = size)[seq_len(nrow(x))], mean,
      na.rm = TRUE
    )
    avgs <- round(avgs, 2)
  }

  return(tibble::tibble(day = days, value_avgs = avgs))
}
