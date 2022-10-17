#!/usr/bin/env Rscript

# Download a file from laji.fi
#
# @param taxon taxon shortcode
# @param path character string local path where to save the file
#
download_file <- function(taxon, path = "www/img/sp_images") {
  
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
taxa <- scan("data/taxa.txt", "character")
meta_data <- lapply(scan("data/taxa.txt", "character"), download_file)
names(meta_data) <- taxa
saveRDS(meta_data, "data/photo_metadata.rds")
