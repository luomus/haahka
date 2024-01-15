#' Get languages
#'
#' Return full language names instead of a language code.
#'
#' @param x Language shortcode.
#'
#' @export
get_languages <- function(x) {

  langs <- list("fi" = "Suomi", "en" = "English", "se" = "Svenska")

  stopifnot(x %in% names(langs))

  langs[[x]]

}

#' Get months
#'
#' Get month names in a given language.
#'
#' @param lang Language shortcode.
#' @param format Format.
#'
#' @export
get_months <- function(lang = "en", format) {

  supported_languages <- c("en", "fi", "se")

  cond <- lang %in% supported_languages

  names(cond) <- paste0(
    "lang must be one of: ", paste(supported_languages, collapse = ", ")
  )
  stopifnot(cond)

  stopifnot(
    "format must be either long or short" = format %in% c("long", "short")
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

#' Make date label
#'
#' Create a date label.
#'
#' @param x Date.
#' @param lang Language shortcode.
#'
#' @export
make_date_label <- function(x, lang) {

  stopifnot(inherits(x, "Date"))

  m <- as.integer(format(x, "%m"))

  month_name <- get_months(lang, "long")[m]

  dayx <- as.integer(format(x, "%d"))

  paste(month_name, dayx)

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
#' the middle of the window.
#'
#' @param x A data.frame.
#' @param value String name of the variable containing numeric observation
#'   values.
#'
#' @return A data.frame with dates and corresponding (rounded) averages.
#'
#' @export
tile_observations <- function(x, value) {

  day_index <- seq(3, nrow(x), by = 5)

  day_index <- c(day_index, 366)

  days <- x[["day"]][day_index]

  avgs <- tapply(
    x[[value]],
    rep(seq_along(days), each = 5)[seq_len(nrow(x))],
    mean,
    na.rm = TRUE
  )

  avgs <- round(avgs, 2)

  data.frame(day = days, value_avgs = avgs)

}

#' Get species names
#'
#' Get species common names in a locale.
#'
#' @param lang Language shortcode.
#' @param sp_data Species names.
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

    spps <- sp_data[["Sci_name"]]

    sp_names <- sp_data[[name_field]]

    names(spps) <- paste0(sp_names, " (", spps, ")")

    spps

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
#' @param and Character. "and" string.
#'
#' @export
get_value <- function(season, type, value, records, and = "and") {

  season <- switch(season, Spring = "s", Autumn = "a")

  type <- switch(type, Migr = "m", Local = "l")

  record_value <- paste0(season, type)

  idx <- paste0(record_value, "_")

  row <- records[which(as.logical(records[[idx]])), ]

  num <- row[[record_value]]

  date <- row[["date"]]

  res <- switch(value, date_string = date, Sum = num)

  if (is.numeric(res)) {

    res <- format(res, big.mark = " ")[[1L]]

  } else {

    if (season == "a") {

      res <- res + 100

    }

    res <- paste(res, collapse = and)

  }

  if (num == 0) {

    res <- "-"

  }

  res

}

#' Get taxa
#'
#' Get taxon metadata.
#'
#' @export
haahka_taxa <- function() {

  branch <-  Sys.getenv("BRANCH")

  ans <- taxa

  if (branch != "main") {

    ans <- taxa[1:10, ]

  }

  ans

}
