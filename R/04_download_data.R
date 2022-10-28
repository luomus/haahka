library(DBI)
library(dplyr)
library(dbplyr)
library(finbif)
library(RSQLite)
library(tidyr)

options(
  finbif_api_url = Sys.getenv("FINBIF_API_URL"),
  finbif_warehouse_query = Sys.getenv("FINBIF_WAREHOUSE"),
  finbif_email = Sys.getenv("FINBIF_EMAIL"),
  finbif_rate_limit = 100L,
  finbif_hide_progress = TRUE,
  finbif_use_cache = FALSE
)

taxa <- readRDS("taxa.rds")

filter <- c(collection = "HR.2931")

con <- dbConnect(SQLite(), "data/db-staging.sqlite")

if (!dbExistsTable(con, "last_update")) {

  copy_to(
    con, data.frame(tbl = character(), date = character()), "last_update",
    unique_indexes = list("tbl"), temporary = FALSE
  )

}

last_update <-
  tbl(con, "last_update") |>
  filter(tbl == "events") |>
  pull(date) |>
  as.Date()

if (!isTRUE(last_update > fb_last_mod(filter = filter))) {

  events <-
    fb_occurrence(
      filter = filter,
      select = c("document_id", "year", day = "ordinal_day_start"),
      aggregate = "event", n = "all", aggregate_counts = FALSE
    ) |>
    mutate(
      period = case_when(
        year < 2000 ~ "p1",
        between(year, 2000, 2009) ~ "p2",
        between(year, 2010, 2019) ~ "p3",
        year > 2019 ~ "p4"
      )
    ) |>
    copy_to(
      con, df = _, name = "events", temporary = FALSE, overwrite = TRUE
    )

  rows_upsert(
    tbl(con, "last_update"),
    by = "tbl",
    data.frame(tbl = "events", date = as.character(Sys.Date())),
    copy = TRUE,
    in_place = TRUE
  )

} else {

  events <- tbl(con, "events")

}

n_periods <- 4L

copy_to(
  con,
  data.frame(
    day = rep(1:366, each = n_periods),
    period = paste0("p", seq_len(n_periods))),
  "periods"
)

years <-
  distinct(events, year) |>
  collect() |>
  pull(year)

copy_to(
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
    tbl(con, "last_update") |>
    filter(tbl == !!sp) |>
    pull(date) |>
    as.Date()

  last_mod <- fb_last_mod(sp, filter = filter)

  if (length(last_mod) > 0L && !isTRUE(last_update > last_mod)) {

    message(sprintf("INFO [%s] Updating %s...", Sys.time(), sp))

    data_tbl_name <- paste0(sp, "_data")

    if (dbExistsTable(con, data_tbl_name)) dbRemoveTable(con, data_tbl_name)

    records_tbl_name <- paste0(sp, "_records")

    if (dbExistsTable(con, data_tbl_name)) dbRemoveTable(con, records_tbl_name)

    stats_tbl_name <- paste0(sp, "_stats")

    counts <-
      fb_occurrence(
        sp,
        filter = filter,
        select = "document_id",
        facts = c("Local", "Migr", "Observed"),
        n = "all"
      ) |>
      copy_to(con, df = _, name = sp, temporary = FALSE, overwrite = TRUE) |>
      right_join(events, by = "document_id") |>
      mutate(
        Observed = ifelse(is.na(Observed), FALSE, as.logical(Observed)),
        Local = ifelse(
          Observed, NA_integer_, ifelse(is.na(Local), 0L, as.integer(Local))
        ),
        Migr = ifelse(is.na(Migr), 0L, as.integer(Migr))
      )

    data <-
      group_by(counts, day) |>
      summarise(
        paik = mean(Local, na.rm = TRUE), muutto = mean(Migr, na.rm = TRUE)
      )

    data_long <-
      group_by(counts, day, period) |>
      summarise(
        paik = mean(Local, na.rm = TRUE),
        muutto = mean(Migr, na.rm = TRUE),
        .groups = "keep"
      ) |>
      mutate(total = paik + muutto) |>
      right_join(tbl(con, "periods"), by = c("day", "period"))

    data_long |>
    ungroup(period) |>
    pivot_wider(
      id_cols = day, names_from = period, names_sep = "",
      values_from = c(paik, muutto, total),
    ) |>
    full_join(data, by = "day") |>
    compute(data_tbl_name, temporary = FALSE) |>
    invisible()

    data_long |>
    ungroup(day) |>
    window_order(day) |>
    mutate(
      n = switch(!!type, m = muutto, p = paik, pm = total),
      sn = ifelse(between(day, !!spb, !!spe), n, 0L),
      s = cumsum(ifelse(is.na(sn), 0, sn)) / sum(sn, na.rm = TRUE) > .5,
      an = ifelse(day <= 266L, lead(n, 100L), lag(n, 266L)),
      an = ifelse(between(day, !!aub, !!aue), an, 0L),
      a = cumsum(ifelse(is.na(an), 0, an)) / sum(an, na.rm = TRUE) > .5,
    ) |>
    summarise(
      N = sum(total, na.rm = TRUE),
      aphen = min(day[a], na.rm = TRUE) + 100L,
      sphen = min(day[s], na.rm = TRUE)
    ) |>
    pivot_wider(
      names_from = period, names_sep = "", values_from = c(N, aphen, sphen)
    ) |>
    mutate(
      slopeLong = round((Np4 - Np1) / Np1 * 100L),
      slopeShort = round((Np4 - Np3) / Np3 * 100L)
    ) |>
    collect() |>
    copy_to(
      con, df = _, name = paste0(sp, "_stats"), temporary = FALSE,
      overwrite = TRUE
    ) |>
    invisible()

    counts |>
    right_join(tbl(con, "days"), by = c("day", "year")) |>
    group_by(year) |>
    window_order(day) |>
    mutate(
      am = ifelse(day <= 266L, lead(Migr, 100L), lag(Migr, 266L)),
      al = ifelse(day <= 266L, lead(Local, 100L), lag(Local, 266L)),
    ) |>
    ungroup() |>
    mutate(
      sm = ifelse(between(day, !!spb, !!spe), Migr, 0L),
      am = ifelse(between(day, !!aub, !!aue), am, 0L),
      sl = ifelse(between(day, !!spb, !!spe), Local, 0L),
      al = ifelse(between(day, !!aub, !!aue), al, 0L),
      sm_ = sm == max(sm, na.rm = TRUE),
      am_ = am == max(am, na.rm = TRUE),
      sl_ = sl == max(sl, na.rm = TRUE),
      al_ = al == max(al, na.rm = TRUE),
      sm__ = dense_rank((year + day / 1000) * sm_),
      am__ = dense_rank((year + day / 1000) * am_),
      sl__ = dense_rank((year + day / 1000) * sl_),
      al__ = dense_rank((year + day / 1000) * al_),
      sm_ = sm__ == max(sm__, na.rm = TRUE) | sm__ == max(sm__, na.rm = TRUE) - 1L & sm_,
      am_ = am__ == max(am__, na.rm = TRUE) | am__ == max(am__, na.rm = TRUE) - 1L & am_,
      sl_ = sl__ == max(sl__, na.rm = TRUE) | sl__ == max(sl__, na.rm = TRUE) - 1L & sl_,
      al_ = al__ == max(al__, na.rm = TRUE) | al__ == max(al__, na.rm = TRUE) - 1L & al_
    ) |>
    filter(sm_ | am_ | sl_ | al_) |>
    compute(records_tbl_name, temporary = FALSE)

    rows_upsert(
      tbl(con, "last_update"),
      by = "tbl",
      data.frame(tbl = sp, date = as.character(Sys.Date())),
      copy = TRUE,
      in_place = TRUE
    )

  }

}

dbDisconnect(con)

file.copy("data/db-staging.sqlite", "data/db.sqlite", overwrite = TRUE)
