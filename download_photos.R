suppressPackageStartupMessages({

  library(haahka, warn.conflicts = FALSE, quietly = TRUE)
  library(httr, warn.conflicts = FALSE, quietly = TRUE)
  library(tools, warn.conflicts = FALSE, quietly = TRUE)
  library(utils, warn.conflicts = FALSE, quietly = TRUE)

})

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

    ans <- utils::download.file(
      res[["largeURL"]],
      file.path(
        path,
        paste(taxon, tools::file_ext(res[["largeURL"]]), sep = ".")
      ),
      quiet = TRUE
    )

    if (ans == 0) {

      message(sprintf("INFO [%s] Updated %s image", format(Sys.time()), taxon))

    } else {

      message(
        sprintf("ERROR [%s] %s image update failed", format(Sys.time()), taxon)
      )

    }

  }

  res

}

if (!dir.exists("var/data/sp_images/org")) {

  dir.create("var/data/sp_images/org", recursive = TRUE)

}

taxa <- haahka::haahka_taxa()
taxa <- taxa[["Species_Abb"]]
meta_data <- lapply(taxa, download_file)
names(meta_data) <- taxa
saveRDS(meta_data, "var/data/photo_metadata.rds")
