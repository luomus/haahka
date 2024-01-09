suppressPackageStartupMessages({

  library(callr, warn.conflicts = FALSE, quietly = TRUE)
  library(utils, warn.conflicts = FALSE, quietly = TRUE)
  library(rapidoc, warn.conflicts = FALSE,  quietly = TRUE)

})

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

#* @get /favicon.ico
#* @serializer contentType list(type="image/x-icon")
function() {

  readBin("favicon.ico", "raw", n = file.info("favicon.ico")$size)

}

#* @get /robots.txt
#* @serializer contentType list(type="text/plain")
function() {

  readBin("robots.txt", "raw", n = file.info("robots.txt")$size)

}

#* @assets ./var/data /data
list()

#* @assets ./var/logs /logs
list()

#* @assets ./var/status /status
list()

#* @plumber
function(pr) {

  version <- as.character(utils::packageVersion("haahka"))

  pr_set_api_spec(
    pr,
    function(spec) {

      spec[[c("info","version")]] <- version

      spec[[c("paths", "/favicon.ico")]] <- NULL
      spec[[c("paths", "/healthz")]] <- NULL
      spec[[c("paths", "/job")]] <- NULL
      spec[[c("paths", "/robots.txt")]] <- NULL
      spec[[c("paths", "/")]] <- NULL

      spec

    }
  )

  pr$setDocs(
    "rapidoc",
    bg_color ="#141B15",
    text_color = "#FFFFFF",
    primary_color = "#55AAE2",
    render_style = "read",
    slots = paste0(
      '<img ',
      'slot="logo" ',
      'src="../public/logo.png" ',
      'width=36px style=\"margin-left:7px\"/>'
    ),
    heading_text = paste("Haahka", version),
    regular_font = "Roboto, Helvetica Neue, Helvetica, Arial, sans-serif",
    font_size = "largest",
    sort_tags = "false",
    sort_endpoints_by = "summary",
    allow_spec_file_load = "false"
  )

}
