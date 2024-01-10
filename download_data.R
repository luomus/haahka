suppressPackageStartupMessages({

  library(DBI, warn.conflicts = FALSE, quietly = TRUE)
  library(dbplyr, warn.conflicts = FALSE, quietly = TRUE)
  library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
  library(finbif, warn.conflicts = FALSE, quietly = TRUE)
  library(haahka, warn.conflicts = FALSE, quietly = TRUE)
  library(RPostgres, warn.conflicts = FALSE, quietly = TRUE)
  library(tidyr, warn.conflicts = FALSE, quietly = TRUE)

})

con <- DBI::dbConnect(RPostgres::Postgres(), dbname = Sys.getenv("DB_NAME"))

options(
  finbif_api_url = Sys.getenv("FINBIF_API_URL"),
  finbif_warehouse_query = Sys.getenv("FINBIF_WAREHOUSE"),
  finbif_email = Sys.getenv("FINBIF_EMAIL"),
  finbif_rate_limit = Inf,
  finbif_hide_progress = TRUE,
  finbif_use_cache = FALSE,
  finbif_use_cache_metadata = TRUE
)

taxa <- read_taxa("taxa.rds")

filter <- c(collection = "HR.2931")

if (!DBI::dbExistsTable(con, "last_update")) {

  dplyr::copy_to(
    con, data.frame(tbl = character(), date = character()), "last_update",
    unique_indexes = list("tbl"), temporary = FALSE
  )

}

last_update <- dplyr::tbl(con, "last_update")
last_update <- dplyr::filter(last_update, .data[["tbl"]] == "events")
last_update <- dplyr::pull(last_update, "date")
last_update <- as.Date(last_update)

if (!isTRUE(last_update > finbif::fb_last_mod(filter = filter))) {

  events <- finbif::finbif_occurrence(
    filter = filter,
    select = c("document_id", "year", day = "ordinal_day_start"),
    aggregate = "event", n = "all", aggregate_counts = FALSE
  )
  events <- dplyr::mutate(
    events,
    period = dplyr::case_when(
      .data[["year"]] < 2000 ~ "p1",
      dplyr::between(.data[["year"]], 2000, 2009) ~ "p2",
      dplyr::between(.data[["year"]], 2010, 2019) ~ "p3",
      .data[["year"]] > 2019 ~ "p4"
    )
  )
  events <- dplyr::copy_to(
    con, df = events, name = "events", temporary = FALSE, overwrite = TRUE
  )

  last_update_tbl <- dplyr::tbl(con, "last_update")

  last_update_row <- dbplyr::copy_inline(
    con, data.frame(tbl = "events", date = as.character(Sys.Date()))
  )

  dplyr::rows_upsert(
    last_update_tbl,
    last_update_row,
    by = "tbl",
    copy = TRUE,
    in_place = TRUE
  )

} else {

  events <- dplyr::tbl(con, "events")

}

n_periods <- 4L

dplyr::copy_to(
  con,
  data.frame(
    day = rep(1:366, each = n_periods), period = paste0("p", seq_len(n_periods))
  ),
  "periods"
)

years <- dplyr::distinct(events, .data[["year"]])
years <- dplyr::collect(years)
years <- dplyr::pull(years, "year")

dplyr::copy_to(
  con,
  data.frame(day = rep(1:366, each = length(years)), year = years),
  "days"
)

for (i in seq_len(nrow(taxa))) {

  sp <- taxa[[i, "Species_Abb"]]
  type <- taxa[[i, "type"]]
  spb <- taxa[[i, "spb"]]
  spe <- taxa[[i, "spe"]]
  aub <- taxa[[i, "aub"]] - 100L
  aue <- taxa[[i, "aue"]] - 100L

  if (aue < 1) aue <- 366L + aue

  message(sprintf("INFO [%s] Checking %s...", format(Sys.time()), sp))

  last_update <- dplyr::tbl(con, "last_update")
  last_update <- dplyr::filter(last_update, .data[["tbl"]] == !!sp)
  last_update <- dplyr::pull(last_update, "date")
  last_update <- as.Date(last_update)

  last_mod <- finbif::finbif_last_mod(sp, filter = filter)

  if (length(last_mod) > 0L && !isTRUE(last_update > last_mod)) {

    message(sprintf("INFO [%s] Updating %s...", format(Sys.time()), sp))

    data_tbl_name <- paste0(sp, "_data")

    if (DBI::dbExistsTable(con, data_tbl_name)) {

      DBI::dbRemoveTable(con, data_tbl_name)

    }

    records_tbl_name <- paste0(sp, "_records")

    if (DBI::dbExistsTable(con, records_tbl_name)) {

      DBI::dbRemoveTable(con, records_tbl_name)

    }

    stats_tbl_name <- paste0(sp, "_stats")

    counts <- finbif::finbif_occurrence(
      sp,
      filter = filter,
      select = "document_id",
      facts = c("Local", "Migr", "Observed"),
      n = "all"
    )
    counts <- dplyr::copy_to(
      con, df = counts, name = sp, temporary = FALSE, overwrite = TRUE
    )
    counts <- dplyr::right_join(counts, events, by = "document_id")
    counts <- dplyr::mutate(
      counts,
      Observed = ifelse(
        is.na(.data[["Observed"]]), FALSE, as.logical(.data[["Observed"]])
      ),
      Local = ifelse(
        .data[["Observed"]],
        NA_integer_,
        ifelse(is.na(.data[["Local"]]), 0L, as.integer(.data[["Local"]]))
      ),
      Migr = ifelse(is.na(.data[["Migr"]]), 0L, as.integer(.data[["Migr"]]))
    )

    data <- dplyr::group_by(counts, dplyr::all_of("day"))
    data <- dplyr::summarise(
      data,
      paik = mean(.data[["Local"]], na.rm = TRUE),
      muutto = mean(.data[["Migr"]], na.rm = TRUE)
    )

    data_long <- dplyr::group_by(counts, day, period)
    data_long <- dplyr::summarise(
      data_long,
      paik = mean(.data[["Local"]], na.rm = TRUE),
      muutto = mean(.data[["Migr"]], na.rm = TRUE),
      .groups = "keep"
    )
    data_long <- dplyr::mutate(
      data_long, total = .data[["paik"]] + .data[["muutto"]]
    )
    data_long <- dplyr::right_join(
      data_long, tbl(con, "periods"), by = c("day", "period")
    )

    data_wide <- dplyr::ungroup(data_long, dplyr::all_of("period"))
    data_wide <- tidyr::pivot_wider(
      data_wide,
      id_cols = dplyr::all_of("day"),
      names_from = dplyr::all_of("period"),
      names_sep = "",
      values_from = dplyr::all_of(c("paik", "muutto", "total")),
    )
    data_wide <- dplyr::full_join(data_wide, data, by = "day")

    dplyr::compute(data_wide, data_tbl_name, temporary = FALSE)

    data_long <- dplyr::ungroup(data_long, dplyr::all_of("day"))
    data_long <- dbplyr::window_order(data_long, .data[["day"]])
    data_long <- dplyr::mutate(
      data_long,
      n = switch(
        !!type,
        m = .data[["muutto"]],
        p = .data[["paik"]],
        pm = .data[["total"]]
      ),
      sn = ifelse(
        dplyr::between(.data[["day"]], !!spb, !!spe), .data[["n"]], 0L
      ),
      s = ifelse(
        max(.data[["sn"]], na.rm = TRUE) == 0,
        FALSE,
        cumsum(ifelse(is.na(.data[["sn"]]), 0, .data[["sn"]])) /
          sum(.data[["sn"]], na.rm = TRUE) > .5
      ),
      an = ifelse(
        .data[["day"]] <= 266L,
        dplyr::lead(.data[["n"]], 100L),
        dplyr::lag(.data[["n"]], 266L)
      ),
      an = ifelse(
        dplyr::between(.data[["day"]], !!aub, !!aue), .data[["an"]], 0L
      ),
      a = ifelse(
        max(.data[["an"]], na.rm = TRUE) == 0,
        FALSE,
        cumsum(ifelse(is.na(.data[["an"]]), 0, .data[["an"]])) /
          sum(.data[["an"]], na.rm = TRUE) > .5
      ),
    )
    data_long <- dplyr::summarise(
      data_long,
      N = sum(.data[["total"]], na.rm = TRUE),
      aphen = min(.data[["day"]][a], na.rm = TRUE) + 100L,
      sphen = min(.data[["day"]][s], na.rm = TRUE)
    )
    data_long <- tidyr::pivot_wider(
      data_long,
      names_from = dplyr::all_of("period"),
      names_sep = "",
      values_from = dplyr::all_of(c("N", "aphen", "sphen"))
    )
    data_long <- dplyr::mutate(
      data_long,
      slopeLong = ifelse(
        .data[["Np1"]] == 0,
        NA_real_,
        round((.data[["Np4"]] - .data[["Np1"]]) / .data[["Np1"]] * 100L)
      ),
      slopeShort = ifelse(
        .data[["Np3"]] == 0,
        NA_real_,
        round((.data[["Np4"]] - .data[["Np3"]]) / .data[["Np3"]] * 100L)
      )
    )
    data_long <- dplyr::collect(data_long)

    dplyr::copy_to(
      con,
      df = data_long,
      name = paste0(sp, "_stats"),
      temporary = FALSE,
      overwrite = TRUE
    )

    records <- dplyr::right_join(
      counts, dplyr::tbl(con, "days"), by = c("day", "year")
    )
    records <- dplyr::group_by(records, dplyr::all_of("year"))
    records <- dbplyr::window_order(records, .data[["day"]])
    records <- dplyr::mutate(
      records,
      am = ifelse(
        .data[["day"]] <= 266L,
        dplyr::lead(.data[["Migr"]], 100L),
        dplyr::lag(.data[["Migr"]], 266L)
      ),
      al = ifelse(
        .data[["day"]] <= 266L,
        dplyr::lead(.data[["Local"]], 100L),
        dplyr::lag(.data[["Local"]], 266L)
      ),
    )
    records <- dplyr::ungroup(records)
    records <- dplyr::mutate(
      records,
      sm = ifelse(
        dplyr::between(.data[["day"]], !!spb, !!spe), .data[["Migr"]], 0L
      ),
      am = ifelse(
        dplyr::between(.data[["day"]], !!aub, !!aue), .data[["am"]], 0L
      ),
      sl = ifelse(
        dplyr::between(.data[["day"]], !!spb, !!spe), .data[["Local"]], 0L
      ),
      al = ifelse(
        dplyr::between(.data[["day"]], !!aub, !!aue), .data[["al"]], 0L
      ),
      sm_ = ifelse(.data[["sm"]] == max(.data[["sm"]], na.rm = TRUE), 1, 0),
      am_ = ifelse(.data[["am"]] == max(.data[["am"]], na.rm = TRUE), 1, 0),
      sl_ = ifelse(.data[["sl"]] == max(.data[["sl"]], na.rm = TRUE), 1, 0),
      al_ = ifelse(.data[["al"]] == max(.data[["al"]], na.rm = TRUE), 1, 0),
      sm__ = dplyr::dense_rank(
        (.data[["year"]] + .data[["day"]] / 1000) * .data[["sm_"]]
      ),
      am__ = dplyr::dense_rank(
        (.data[["year"]] + .data[["day"]] / 1000) * .data[["am_"]]
      ),
      sl__ = dplyr::dense_rank(
        (.data[["year"]] + .data[["day"]] / 1000) * .data[["sl_"]]
      ),
      al__ = dplyr::dense_rank(
        (.data[["year"]] + .data[["day"]] / 1000) * .data[["al_"]]
      ),
      sm_ = .data[["sm__"]] == max(.data[["sm__"]], na.rm = TRUE) |
        .data[["sm__"]] == max(.data[["sm__"]], na.rm = TRUE) - 1L &
          .data[["sm_"]] == 1,
      am_ = .data[["am__"]] == max(.data[["am__"]], na.rm = TRUE) |
        .data[["am__"]] == max(.data[["am__"]], na.rm = TRUE) - 1L &
          .data[["am_"]] == 1,
      sl_ = .data[["sl__"]] == max(.data[["sl__"]], na.rm = TRUE) |
        .data[["sl__"]] == max(.data[["sl__"]], na.rm = TRUE) - 1L &
          .data[["sl_"]] == 1,
      al_ = .data[["al__"]] == max(.data[["al__"]], na.rm = TRUE) |
        .data[["al__"]] == max(.data[["al__"]], na.rm = TRUE) - 1L &
          .data[["al_"]] == 1
    )
    records <- dplyr::filter(
      records, .data[["sm_"]] | .data[["am_"]] | .data[["sl_"]] | .data[["al_"]]
    )

    dplyr::compute(records, records_tbl_name, temporary = FALSE)

    dplyr::rows_upsert(
      dplyr::tbl(con, "last_update"),
      by = "tbl",
      data.frame(tbl = sp, date = as.character(Sys.Date())),
      copy = TRUE,
      in_place = TRUE
    )

  }

}

DBI::dbDisconnect(con)
