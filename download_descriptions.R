library(httr, warn.conflicts = FALSE, quietly = TRUE)

download_descriptions <- function(taxon) {

  taxon_id <- httr::RETRY(
    "GET",
    url = "https://laji.fi",
    path = file.path("api", "taxa", "search"),
    query = list(query = taxon)
  )

  taxon_id <- httr::content(taxon_id)

  taxon_id <- taxon_id[[1L]]

  taxon_id <- taxon_id[["id"]]

  res <- httr::RETRY(
    "GET",
    url = "https://laji.fi",
    path = file.path("api", "taxa", taxon_id, "descriptions"),
    query = list(lang = "multi")
  )

  httr::content(res)

}

taxa <- readRDS("taxa.rds")
taxa <- taxa[["Species_Abb"]]
descriptions <- lapply(taxa, download_descriptions)
names(descriptions) <- taxa
saveRDS(descriptions, "var/data/descriptions.rds")
