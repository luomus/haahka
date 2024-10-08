suppressPackageStartupMessages({

  library(haahka, warn.conflicts = FALSE, quietly = TRUE)
  library(httr, warn.conflicts = FALSE, quietly = TRUE)

})

download_descriptions <- function(taxon) {

  taxon_id <- httr::RETRY(
    "GET",
    url = "https://laji.fi",
    path = file.path("taxa", "search"),
    query = list(query = taxon)
  )

  taxon_id <- httr::content(taxon_id)

  taxon_id <- taxon_id[[1L]]

  taxon_id <- taxon_id[["id"]]

  res <- httr::RETRY(
    "GET",
    url = "https://laji.fi",
    path = file.path("taxa", taxon_id, "descriptions"),
    query = list(lang = "multi")
  )

  message(
    sprintf("INFO [%s] Updated %s description", format(Sys.time()), taxon)
  )

  res <- httr::content(res)

  default <- which(vapply(res, getElement, "", "id") == "default")

  if (length(default) > 0L) {

    res <- list(res[[default]])

  }

  res

}

taxa <- haahka::haahka_taxa()
taxa <- taxa[["Species_Abb"]]
descriptions <- lapply(taxa, download_descriptions)
names(descriptions) <- taxa
saveRDS(descriptions, "var/data/descriptions.rds")
