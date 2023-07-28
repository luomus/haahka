#!/usr/bin/env Rscript

# Download a file from laji.fi
#
# @param taxon taxon shortcode
# @param path character string local path where to save the file
#
download_file <- function(taxon, path = "var/data/sp_images/org") {

  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

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
    path = file.path("api", "taxa", taxon_id, "media")
  )

  res <- httr::content(res)

  if (length(res) > 0L) {

    res <- res[[1L]]

    download.file(
      res[["largeURL"]],
      file.path(
        path,
        paste(taxon, tools::file_ext(res[["largeURL"]]), sep = ".")
      )
    )

  }

  res

}

# Process downloads ---------------------------------------------------
if (!dir.exists("var/data/sp_images/org")) dir.create("var/data/sp_images/org")
taxa <- readRDS("taxa.rds")
taxa <- taxa[["Species_Abb"]]
meta_data <- lapply(taxa, download_file)
names(meta_data) <- taxa
saveRDS(meta_data, "var/data/photo_metadata.rds")
