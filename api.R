#* @apiTitle Haahka
#* @apiTOS https://laji.fi/en/about/845
#* @apiContact list(name = "laji.fi support", email = "helpdesk@laji.fi")
#* @apiLicense list(name = "MIT", url = "https://opensource.org/licenses/MIT")

suppressPackageStartupMessages({

  library(callr, warn.conflicts = FALSE, quietly = TRUE)
  library(utils, warn.conflicts = FALSE, quietly = TRUE)
  library(rapidoc, warn.conflicts = FALSE, quietly = TRUE)

})

version <- as.character(utils::packageVersion("haahka"))

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

#* @get /api/job
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
#* @head /api/healthz
#* @get /api/healthz
#* @response 200 A json object
#* @serializer unboxedJSON
function() {
  ""
}

#* Get API spec
#* @get /api/__docs__/openapi.json
#* @serializer unboxedJSON
function(req) {

  spec <- req[["pr"]][["getApiSpec"]]()

  spec[[c("info", "version")]] <- version

  spec[[c("paths", "/__docs__/")]] <- NULL
  spec[[c("paths", "/__docs__/index.html")]] <- NULL
  spec[[c("paths", "/api/__docs__/")]] <- NULL
  spec[[c("paths", "/api/__docs__/openapi.json")]] <- NULL
  spec[[c("paths", "/api/healthz")]] <- NULL
  spec[[c("paths", "/api/job")]] <- NULL
  spec[[c("paths", "/api/")]] <- NULL
  spec[[c("paths", "/api")]] <- NULL
  spec[[c("paths", "/openapi.json")]] <- NULL
  spec[[c("paths", "/__swagger__/")]] <- NULL
  spec[[c("paths", "/__swagger__/index.html")]] <- NULL
  spec[[c("paths", "/__swagger__/openapi.json")]] <- NULL

  spec

}

#* Get API docs
#* @get /api/__docs__/
#* @serializer html
function() {

  rapidoc::rapidoc_spec(
    spec_url = "./openapi.json",
    bg_color = "#141B15",
    text_color = "#FFFFFF",
    primary_color = "#55AAE2",
    render_style = "read",
    slots = paste0(
      "<img ",
      "slot=\"logo\" ",
      "src=\"../../img/halias_logo.gif\" ",
      "width=36px style=\"margin-left:7px\"/>"
    ),
    heading_text = paste("Haahka", version),
    regular_font = "Roboto, Helvetica Neue, Helvetica, Arial, sans-serif",
    font_size = "largest",
    sort_tags = "false",
    sort_endpoints_by = "summary",
    allow_spec_file_load = "false"
  )

}

#* @get /api
function(res) {

  res$status <- 303L
  res$setHeader("Location", "/api/__docs__/")

}

#* @get /api/
function(res) {

  res$status <- 303L
  res$setHeader("Location", "/api/__docs__/")

}

#* @assets ./var/data /api/data
list()

#* @assets ./var/logs /api/logs
list()

#* @assets ./var/status /api/status
list()

#* @assets /usr/local/lib/R/site-library/rapidoc/dist /api/__docs__/
list()
