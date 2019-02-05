library(assertthat)
library(shiny)
library(tsibble)
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
  
  assertthat::see_if(is(x, "Date"))
  
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


# Parse description file
# 
parse_description <- function(style_name, text) {
  if (is.na(style_name) || is.na(text)) {
    return(invisible(NULL))
  }
  style_name <- tolower(style_name)
  
  # NOTE: style is hard coded and needs to be adjusted if the style changes
  # Check both the element style and content
  if (style_name == "no spacing" & text != "") {
    citation_pattern <- "\\((.+?)\\)"
    
    if (grepl(citation_pattern, text)) {
      citations <- unlist(stringr::str_extract_all(text, citation_pattern))
      for (citation in citations) {
        i_citation <- gsub("\\(", "<i>", citation)
        i_citation <- gsub("\\)", "</i>", i_citation)
        text <- gsub(citation, i_citation, text)
      }
      # Contsruct valid HTML
      text <- paste0("<p class='endnote'>", text, "</p>")
      element <- shiny::HTML(text)
    } else {
      element <- shiny::p(text, class = "description")  
    }
    
  } else if (style_name == "endnote text" || style_name == "list paragraph" && text != "") {
    # Make hyperlinks acutal hyperlinks
    if (grepl("http", text)) {
      # Get rid of the unneed prefix/suffix characters
      text <- gsub("(<|>)", "", text)
      url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
      url <- stringr::str_extract(text, url_pattern)
      # Contsruct valid HTML
      text <- gsub(url, paste0("<a href='", url, "'>", url, "</a>"), text)
      text <- paste0("<p class='endnote'>", text, "</p>")
      element <- shiny::HTML(text)
    } else {
      element <- shiny::p(text, class = "endnote")
    }
    
  } else if (style_name == "heading 3" & text != "") {
    element <- shiny::h4(text, class = "description")
  } else {
    element <- invisible(NULL)
  }
  return(element)
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
  assertthat::see_if(is(x) == "tbl_ts")
  
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
    avgs <- tsibble::tile_dbl(x[[value]], ~ mean(., na.rm = TRUE), 
                              .size = size)
    avgs <- round(avgs, 2)
  }
  
  return(tibble::tibble(day = days, value_avgs = avgs))
}
