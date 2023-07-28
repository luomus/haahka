res <- tryCatch(
  {

    source("download_photos.R")
    source("resize_photos.R")
    source("download_descriptions.R")
    source("download_data.R")

    message(sprintf("INFO [%s] Job complete", Sys.time()))

    "true"

  },
  error = function(e) {

    message(sprintf("ERROR [%s] %s", Sys.time(), e$message))

    "false"

  }
)

cat(res, file = "var/status/success.txt")

cat(format(Sys.time(), usetz = TRUE), file = "var/status/last-update.txt")
