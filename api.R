suppressPackageStartupMessages({

  library(future, quietly = TRUE)
  library(RPostgres, quietly = TRUE)
  library(pool, quietly = TRUE)

})

con <- dbPool(Postgres(), dbname = Sys.getenv("DB_NAME"))

#* @filter secret
function(req, res) {

  secret <- identical(req[["argsQuery"]][["secret"]], Sys.getenv("JOB_SECRET"))

  if (grepl("job", req[["PATH_INFO"]]) && !secret) {

    res[["status"]] <- 401
    list(error = "Access token required")

  } else {

    forward()

  }

}

#* @get /job
#* @serializer unboxedJSON
function() {

  on.exit({

    sink(type = "message")

    sink()

  })

  log_file_name <- sprintf("var/logs/job-%s.txt", Sys.Date())

  log_file <- file(log_file_name, open = "wt")

  sink(log_file)

  sink(log_file, type = "message")

  promises::future_promise({
    con <- pool::dbPool(RPostgres::Postgres(), dbname = Sys.getenv("DB_NAME"))
    source("update.R")
    pool::poolClose(con)
  })

  "success"

}

#* Check the liveness of the API
#* @head /healthz
#* @get /healthz
#* @tag status
#* @response 200 A json object
#* @serializer unboxedJSON
function() {
  ""
}

#* @assets ./var/logs /logs
list()

#* @assets ./var/status /status
list()

#* @assets ./var/data /data
list()
