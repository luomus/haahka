#!/usr/bin/env Rscript

# Download a taxon description from laji.fi
#
# @param taxon taxon shortcode

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
    path = file.path("api", "taxa", taxon_id, "descriptions")
  )

  httr::content(res)

}

# Process downloads ---------------------------------------------------
taxa <- readRDS("taxa.rds")
taxa <- taxa[["Species_Abb"]]
descriptions <- lapply(taxa, download_descriptions)
names(descriptions) <- taxa
saveRDS(descriptions, "data/descriptions.rds")
