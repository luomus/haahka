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

  callr::r_bg(
    source,
    args = list(file = "update.R"),
    poll_connection = FALSE,
    cleanup = FALSE
  )

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
