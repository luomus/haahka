#' Get languages
#'
#' Return full language names instead of a language code.
#'
#' @param x Language shortcode.
#'
#' @importFrom assertthat assert_that
#' @export
get_languages <- function(x) {

  langs <- list("fi" = "Suomi", "en" = "English", "se" = "Svenska")

  assertthat::assert_that(x %in% names(langs))

  langs[[x]]

}

#' Get months
#'
#' Get month names in a given language.
#'
#' @param lang Language shortcode.
#' @param format Format.
#'
#' @importFrom assertthat assert_that
#' @export
get_months <- function(lang = "en", format) {

  supported_languages <- c("en", "fi", "se")

  assertthat::assert_that(
    lang %in% supported_languages,
    msg = paste0(
      "lang must be one of: ",
      paste(supported_languages, collapse = ", ")
    )
  )

  assertthat::assert_that(
    format %in% c("long", "short"),
    msg = "format must be either long or short"
  )

  month_names <- list(
    "short" = list(
      "en" = c(
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
        "Nov", "Dec"
      ),
      "fi" = c(
        "Tammi", "Helmi", "Maalis", "Huhti", "Touko", "Kes\u00e4", "Hein\u00e4",
        "Elo", "Syys", "Loka", "Marras", "Joulu"
      ),
      "se" = c(
        "Jan", "Feb", "Mar", "Apr", "Maj", "Jun", "Jul", "Aug", "Sep", "Okt",
        "Nov", "Dec"
      )
    ),
    "long" = list(
      "en" = c(
        "January", "February", "March", "April", "May", "June", "July",
        "August", "September", "October", "November", "December"
      ),
      "fi" = c(
        "Tammikuu", "Helmikuu", "Maaliskuu", "Huhtikuu", "Toukokuu",
        "Kes\u00e4kuu", "Hein\u00e4kuu", "Elokuu", "Syyskuu", "Lokakuu",
        "Marraskuu", "Joulukuu"
      ),
      "se" = c(
        "Januari", "Februari", "Mars", "April", "Maj", "Juni", "Juli",
        "Augusti", "September", "Oktober", "November", "December"
      )
    )
  )

  month_names[[format]][[lang]]

}

#' Get timestamp
#'
#' Return numeric timestamp value based on a Date. Needed for highcharts
#' x-axis settings and plotting.
#'
#' @param x Date.
#'
#' @importFrom highcharter datetime_to_timestamp
#' @export
get_timestamp <- function(x) {

  highcharter::datetime_to_timestamp(as.Date(x, tz = "UTC"))

}

#' Check is odd
#'
#' Check that a value is odd numeric.
#'
#' @param x Value.
#'
#' @importFrom assertthat assert_that
#' @export
is_odd <- function(x) {

  assertthat::assert_that(is.numeric(x), length(x) == 1)

  x %% 2 == 1

}

#' Make date label
#'
#' Create a date label.
#'
#' @param x Date.
#' @param lang Language shortcode.
#'
#' @importFrom assertthat see_if
#' @importFrom glue glue
#' @importFrom lubridate day month
#' @export
make_date_label <- function(x, lang) {

  assertthat::see_if(inherits(x, "Date"))

  month_name <- get_months(lang, "long")[lubridate::month(x)]

  dayx <- lubridate::day(x)

  glue::glue("{month_name} {dayx}")

}

#' Parse author
#'
#' Parse author name.
#'
#' @param x Author name.
#'
#' @export
parse_author <- function(x) {

  tokens <- unlist(strsplit(x, " "))

  initial <- paste0(substring(tokens[1], 1, 1), ".")

  paste(c(tokens[2], initial), collapse = ", ")

}

#' Parse description
#'
#' Parse description text.
#'
#' @param text Description text.
#' @param lang Language shortcode.
#'
#' @export
parse_description <- function(text, lang) {

  assign("ans", NULL, envir = parse_env)

  on.exit(remove(list = "ans", envir = parse_env))

  lapply(text, get_text, h = 3, lang = lang)

  get("ans", envir = parse_env)

}

#' @noRd

parse_env <- new.env()

#' @noRd
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

#' @noRd
get_title <- function(x, h, lang) {

  if (!is.null(x[["title"]])) {

    ans <- get("ans", envir = parse_env)

    ans <- c(ans, paste0("<h", h, ">", x[["title"]][[lang]], "</h", h, ">"))

    assign("ans", ans, envir = parse_env)

  }

}

#' @noRd
get_content <- function(x, h, lang) {

  if (!is.null(x[["content"]])) {

    ans <- get("ans", envir = parse_env)

    ans <- c(ans, x[["content"]][[lang]])

    assign("ans", ans, envir = parse_env)

  }

}

#' Calculate a tiled average (non-overlapping windows) for yearly observations.
#'
#' An average is calculated over a specified window and assigned to the day in
#' the middle of the windows. For example, if size is 5, then the average is
#' assigned to day at index 3 etc.
#'
#' @param x A tsibble.
#' @param day String name of the variable containing datetime values.
#' @param value String name of the variable containing numeric observation
#'   values.
#' @param size Numeric size of the window over which average is calculated.
#'   Must be an odd value.
#'
#' @return A tibble with dates and corresponding (rounded) averages.
#' @importFrom assertthat assert_that on_failure see_if
#' @importFrom stats median
#' @importFrom tibble tibble
#' @export
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
    day_index <- seq(stats::median(1:size), nrow(x), by = size)

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

  tibble::tibble(day = days, value_avgs = avgs)

}
