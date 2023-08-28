log_file_name <- sprintf("var/logs/update-%s.txt", Sys.Date())

log_file <- file(log_file_name, open = "wt")

sink(log_file)

sink(log_file, type = "message")

con <- pool::dbPool(RPostgres::Postgres(), dbname = Sys.getenv("DB_NAME"))

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

pool::poolClose(con)

sink(type = "message")

sink()

file.copy(log_file_name, "var/logs/update-latest.txt", TRUE)
