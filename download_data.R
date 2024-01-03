library(DBI, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(finbif, warn.conflicts = FALSE, quietly = TRUE)
library(RPostgres, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)

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

taxa <- readRDS("taxa.rds")

filter <- c(collection = "HR.2931")

if (!DBI::dbExistsTable(con, "last_update")) {

  dplyr::copy_to(
    con, data.frame(tbl = character(), date = character()), "last_update",
    unique_indexes = list("tbl"), temporary = FALSE
  )

}

last_update <-
  dplyr::tbl(con, "last_update") |>
  dplyr::filter(tbl == "events") |>
  dplyr::pull(date) |>
  as.Date()

if (!isTRUE(last_update > finbif::fb_last_mod(filter = filter))) {

  events <-
    finbif::finbif_occurrence(
      filter = filter,
      select = c("document_id", "year", day = "ordinal_day_start"),
      aggregate = "event", n = "all", aggregate_counts = FALSE
    ) |>
    dplyr::mutate(
      period = case_when(
        year < 2000 ~ "p1",
        dplyr::between(year, 2000, 2009) ~ "p2",
        dplyr::between(year, 2010, 2019) ~ "p3",
        year > 2019 ~ "p4"
      )
    ) |>
    dplyr::copy_to(
      con, df = _, name = "events", temporary = FALSE, overwrite = TRUE
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

years <-
  dplyr::distinct(events, year) |>
  dplyr::collect() |>
  dplyr::pull(year)

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

  message(sprintf("INFO [%s] Checking %s...", Sys.time(), sp))

  last_update <-
    dplyr::tbl(con, "last_update") |>
    dplyr::filter(tbl == !!sp) |>
    dplyr::pull(date) |>
    as.Date()

  last_mod <- finbif::finbif_last_mod(sp, filter = filter)

  if (length(last_mod) > 0L && !isTRUE(last_update > last_mod)) {

    message(sprintf("INFO [%s] Updating %s...", Sys.time(), sp))

    data_tbl_name <- paste0(sp, "_data")

    if (DBI::dbExistsTable(con, data_tbl_name)) {

      DBI::dbRemoveTable(con, data_tbl_name)

    }

    records_tbl_name <- paste0(sp, "_records")

    if (DBI::dbExistsTable(con, records_tbl_name)) {

      DBI::dbRemoveTable(con, records_tbl_name)

    }

    stats_tbl_name <- paste0(sp, "_stats")

    counts <-
      finbif::finbif_occurrence(
        sp,
        filter = filter,
        select = "document_id",
        facts = c("Local", "Migr", "Observed"),
        n = "all"
      ) |>
      dplyr::copy_to(
        con, df = _, name = sp, temporary = FALSE, overwrite = TRUE
      ) |>
      dplyr::right_join(events, by = "document_id") |>
      dplyr::mutate(
        Observed = ifelse(is.na(Observed), FALSE, as.logical(Observed)),
        Local = ifelse(
          Observed, NA_integer_, ifelse(is.na(Local), 0L, as.integer(Local))
        ),
        Migr = ifelse(is.na(Migr), 0L, as.integer(Migr))
      )

    data <-
      dplyr::group_by(counts, day) |>
      dplyr::summarise(
        paik = mean(Local, na.rm = TRUE), muutto = mean(Migr, na.rm = TRUE)
      )

    data_long <-
      dplyr::group_by(counts, day, period) |>
      dplyr::summarise(
        paik = mean(Local, na.rm = TRUE),
        muutto = mean(Migr, na.rm = TRUE),
        .groups = "keep"
      ) |>
      dplyr::mutate(total = paik + muutto) |>
      dplyr::right_join(tbl(con, "periods"), by = c("day", "period"))

    dplyr::ungroup(data_long, period) |>
      tidyr::pivot_wider(
        id_cols = day, names_from = period, names_sep = "",
        values_from = c(paik, muutto, total),
      ) |>
      dplyr::full_join(data, by = "day") |>
      dplyr::compute(data_tbl_name, temporary = FALSE) |>
      invisible()

    dplyr::ungroup(data_long, day) |>
      dbplyr::window_order(day) |>
      dplyr::mutate(
        n = switch(!!type, m = muutto, p = paik, pm = total),
        sn = ifelse(dplyr::between(day, !!spb, !!spe), n, 0L),
        s = ifelse(
          max(sn, na.rm = TRUE) == 0,
          FALSE,
          cumsum(ifelse(is.na(sn), 0, sn)) / sum(sn, na.rm = TRUE) > .5
        ),
        an = ifelse(day <= 266L, lead(n, 100L), dplyr::lag(n, 266L)),
        an = ifelse(dplyr::between(day, !!aub, !!aue), an, 0L),
        a = ifelse(
          max(an, na.rm = TRUE) == 0,
          FALSE,
          cumsum(ifelse(is.na(an), 0, an)) / sum(an, na.rm = TRUE) > .5
        ),
      ) |>
      dplyr::summarise(
        N = sum(total, na.rm = TRUE),
        aphen = min(day[a], na.rm = TRUE) + 100L,
        sphen = min(day[s], na.rm = TRUE)
      ) |>
      tidyr::pivot_wider(
        names_from = period, names_sep = "", values_from = c(N, aphen, sphen)
      ) |>
      dplyr::mutate(
        slopeLong = ifelse(Np1 == 0, NA_real_, round((Np4 - Np1) / Np1 * 100L)),
        slopeShort = ifelse(Np3 == 0, NA_real_, round((Np4 - Np3) / Np3 * 100L))
      ) |>
      dplyr::collect() |>
      dplyr::copy_to(
        con, df = _, name = paste0(sp, "_stats"), temporary = FALSE,
        overwrite = TRUE
      ) |>
      invisible()

    dplyr::right_join(counts, dplyr::tbl(con, "days"), by = c("day", "year")) |>
      dplyr::group_by(year) |>
      dbplyr::window_order(day) |>
      dplyr::mutate(
        am = ifelse(
          day <= 266L, dplyr::lead(Migr, 100L), dplyr::lag(Migr, 266L)
        ),
        al = ifelse(
          day <= 266L, dplyr::lead(Local, 100L), dplyr::lag(Local, 266L)
        ),
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        sm = ifelse(dplyr::between(day, !!spb, !!spe), Migr, 0L),
        am = ifelse(dplyr::between(day, !!aub, !!aue), am, 0L),
        sl = ifelse(dplyr::between(day, !!spb, !!spe), Local, 0L),
        al = ifelse(dplyr::between(day, !!aub, !!aue), al, 0L),
        sm_ = ifelse(sm == max(sm, na.rm = TRUE), 1, 0),
        am_ = ifelse(am == max(am, na.rm = TRUE), 1, 0),
        sl_ = ifelse(sl == max(sl, na.rm = TRUE), 1, 0),
        al_ = ifelse(al == max(al, na.rm = TRUE), 1, 0),
        sm__ = dplyr::dense_rank((year + day / 1000) * sm_),
        am__ = dplyr::dense_rank((year + day / 1000) * am_),
        sl__ = dplyr::dense_rank((year + day / 1000) * sl_),
        al__ = dplyr::dense_rank((year + day / 1000) * al_),
        sm_ = sm__ == max(sm__, na.rm = TRUE) |
          sm__ == max(sm__, na.rm = TRUE) - 1L & sm_ == 1,
        am_ = am__ == max(am__, na.rm = TRUE) |
          am__ == max(am__, na.rm = TRUE) - 1L & am_ == 1,
        sl_ = sl__ == max(sl__, na.rm = TRUE) |
          sl__ == max(sl__, na.rm = TRUE) - 1L & sl_ == 1,
        al_ = al__ == max(al__, na.rm = TRUE) |
          al__ == max(al__, na.rm = TRUE) - 1L & al_ == 1
      ) |>
      dplyr::filter(sm_ | am_ | sl_ | al_) |>
      dplyr::compute(records_tbl_name, temporary = FALSE)

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
