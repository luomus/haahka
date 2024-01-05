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
#' @importFrom lubridate day month
#' @export
make_date_label <- function(x, lang) {

  assertthat::see_if(inherits(x, "Date"))

  month_name <- get_months(lang, "long")[lubridate::month(x)]

  dayx <- lubridate::day(x)

  paste(month_name, dayx)

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

  size <- as.integer(size)

  assertthat::see_if(inherits(x, "tbl_ts"))

  assertthat::on_failure(is_odd) <- function(call, env) {

    paste0(deparse(call[["x"]]), " is even. size must be odd.")

  }

  assertthat::assert_that(is_odd(size))

  if (size == 1) {

    days <- x[[day]]

    avgs <- x[[value]]

  } else {

    n_days <- 366

    day_index <- seq(stats::median(1:size), nrow(x), by = size)

    remainder <- n_days %% size

    if (remainder != 0) {

      day_index <- c(day_index, n_days)

    }

    days <- x[[day]][day_index]

    avgs <- tapply(
      x[[value]],
      rep(seq_along(days), each = size)[seq_len(nrow(x))],
      mean,
      na.rm = TRUE
    )

    avgs <- round(avgs, 2)

  }

  tibble::tibble(day = days, value_avgs = avgs)

}

#' Get species names
#'
#' Get species common names in a locale.
#'
#' @param lang Language shortcode.
#' @param sp_data Species names.
#'
#' @importFrom dplyr select
#'
#' @export
get_species_names <- function(lang, sp_data) {

  if (!is.null(lang)) {

    if (lang == "fi") {

      name_field <- "FIN_name"

    } else if (lang == "en") {

      name_field <- "ENG_name"

    } else if (lang == "se") {

      name_field <- "SWE_name"

    }

    spps <- dplyr::pull(sp_data, "Sci_name")

    sp_names <- dplyr::pull(sp_data, name_field)

    names(spps) <- paste0(sp_names, " (", spps, ")")

    spps

  }

}

#' Create popup
#'
#' Create an information popup in locale.
#'
#' @param session Shiny app session.
#' @param filebody Species names.
#' @param lang Language shortcode.
#'
#' @importFrom shiny includeMarkdown tagList
#' @importFrom shinyWidgets sendSweetAlert
#'
#' @export
create_popup <- function(session, filebody, lang) {

  content_file <- file.path("www", "infos", paste0(filebody, lang, ".md"))

  if (file.exists(content_file)) {

    shinyWidgets::sendSweetAlert(
      session = session,
      title = NULL,
      text = shiny::tagList(shiny::includeMarkdown(content_file)),
      html = TRUE
    )

  }

}

#' Get record value
#'
#' Get record value for a species.
#'
#' @param season Character. "Spring" or "Autumn".
#' @param type Character. Migration "Migr" or "Local".
#' @param value Character. "date_string" or "Sum".
#' @param records Records table.
#' @param i18n Translation function.
#'
#' @importFrom dplyr .data filter pull
#'
#' @export
get_value <- function(season, type, value, records, i18n) {

  season <- switch(season, Spring = "s", Autumn = "a")

  type <- switch(type, Migr = "m", Local = "l")

  record_value <- paste0(season, type)

  idx <- paste0(record_value, "_")

  value <- switch(value, date_string = "date", Sum = record_value)

  res <- dplyr::filter(records, as.logical(.data[[idx]]))
  res <- dplyr::pull(res, value)

  if (is.numeric(res)) {

    res <- format(res, big.mark = " ")[[1L]]

  } else {

    if (season == "a") {

      res <- res + 100

    }

    res <- paste(res, collapse = i18n()[["t"]](" ja "))

  }

  if (length(res) == 0) {

    res <- "-"

  }

  res

}
