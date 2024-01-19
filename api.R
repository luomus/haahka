#* @apiTitle Haahka
#* @apiDescription Bird Migration browser HTTP API.
#* @apiTOS https://laji.fi/en/about/845
#* @apiContact list(name = "laji.fi support", email = "helpdesk@laji.fi")
#* @apiLicense list(name = "MIT", url = "https://opensource.org/licenses/MIT")
#* @apiTag graphics Get graphics

suppressPackageStartupMessages({

  library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
  library(callr, warn.conflicts = FALSE, quietly = TRUE)
  library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
  library(haahka, warn.conflicts = FALSE, quietly = TRUE)
  library(plumber, warn.conflicts = FALSE, quietly = TRUE)
  library(pool, warn.conflicts = FALSE, quietly = TRUE)
  library(rapidoc, warn.conflicts = FALSE, quietly = TRUE)
  library(RPostgres, warn.conflicts = FALSE, quietly = TRUE)
  library(shiny.i18n, warn.conflicts = FALSE, quietly = TRUE)
  library(utils, warn.conflicts = FALSE, quietly = TRUE)

})

version <- as.character(utils::packageVersion("haahka"))

con <- pool::dbPool(RPostgres::Postgres(), dbname = Sys.getenv("DB_NAME"))

translator <- shiny.i18n::Translator[["new"]](
  translation_json_path = "translation.json"
)

#* @filter secret
function(req, res) {

  secret <- identical(req[["argsQuery"]][["secret"]], Sys.getenv("JOB_SECRET"))

  if (grepl("job", req[["PATH_INFO"]]) && !secret) {

    res[["status"]] <- 401
    list(error = "Access token required")

  } else {

    plumber::forward()

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

#* Get graphics for a species
#* @tag graphics
#* @get /api/plot/<type:str>/<sp:str>
#* @param type:str migration, local or change
#* @param sp:str Taxon code
#* @param locale:str Locale
#* @serializer png list(width = 1200, height = 600, bg = "transparent")
#* @response 200 A png file response
function(type, sp, locale = "fi") {

  data <- dplyr::tbl(con, paste0(sp, "_data"))

  data <- dplyr::collect(data)

  type <- tolower(type)

  title <- switch(
    type,
    migration = "Muuttajamäärien keskiarvot",
    local = "Paikallisten määrien keskiarvot",
    change = "Runsauksien muutokset"
  )

  type <- switch(type, migration = "muutto", local = "paik", change = "totalp")

  month_labels <- haahka::get_months(locale, "short")

  translator[["set_translation_language"]](locale)

  ylab <-  translator[["t"]]("Yksilöä / havaintopäivä")

  title <- translator[["t"]](title)

  plot <-
    ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      data.frame(
        xmin = seq(as.Date("2000-02-01"), by = "2 months", length.out = 6),
        xmax = c(
          seq(as.Date("2000-03-01"), by = "2 months", length.out = 5),
          as.Date("2000-12-31")
        ),
        ymin = 0,
        ymax = Inf
      ),
      fill = "#f0f0f566"
    )

  if (type == "totalp") {

    plot_data <- haahka::tile_observations(data, paste0(type, 1))

    for (i in 1:3) {

      plot_data <- merge(
        plot_data,
        haahka::tile_observations(data, paste0(type, i + 1)),
        by = "day"
      )

    }

    plot_data <- tidyr::pivot_longer(
      plot_data, -dplyr::all_of("day"), names_to = "epoch"
    )

    plot_data[["epoch"]] <- factor(
      plot_data[["epoch"]],
      c("totalp1", "totalp2", "totalp3", "totalp4"),
      ordered = TRUE
    )

    plot <-
      plot +
      ggplot2::geom_line(
        ggplot2::aes(day, .data[["value"]], colour = .data[["epoch"]]),
        plot_data,
        lwd = 2,
        col = "#1f78b4"
      ) +
      ggplot2::scale_color_manual(
        labels = c("1979-1999", "2000-2009", "2010-2019", "2020-"),
        values = c("#1f78b4", "#ff7f0e", "#2ca02c", "#d62728")
      )

  } else {

    plot_data <- haahka::tile_observations(data, type)

    plot <-
      plot +
      ggplot2::geom_line(
        ggplot2::aes(day, .data[[type]]), plot_data, lwd = 2, col = "#1f78b4"
      )

  }

  plot <-
    plot +
    ggplot2::scale_x_date(
      breaks = seq(as.Date("2000-01-01"), by = "month", length.out = 12),
      labels = month_labels,
      limits = as.Date(c("2000-01-01", "2000-12-31")),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 1 / 3))
    ) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(ylab) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_gray(base_size = 24) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        colour = "#333333",
        size = 36,
        margin = ggplot2::margin(0, 0, 12, 0)
      ),
      plot.margin = ggplot2::unit(c(25, 20, 30, 30), "pt"),
      axis.text.x = ggplot2::element_text(colour = "#666666", size = 20),
      axis.line.x = ggplot2::element_line(colour = "#ccd6eb"),
      axis.ticks.x = ggplot2::element_line(colour = "#ccd6eb"),
      axis.ticks.length.x = ggplot2::unit(20, "pt"),
      axis.title.y = ggplot2::element_text(
        colour = "#666666", margin = ggplot2::margin(0, 12, 0, 0)
      ),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(0, 12, 0, 0), colour = "#666666", size = 20
      ),
      panel.background = ggplot2::element_rect(fill = "transparent"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#e6e6e6"),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA)
    )

  print(plot)

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

  suffix <- switch(Sys.getenv("BRANCH"), dev = "-dev", "")

  spec_url <- sprintf(
    "https://haahka%s.laji.fi/api/__docs__/openapi.json", suffix
  )

  rapidoc::rapidoc_spec(
    spec_url = spec_url,
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
    allow_spec_file_load = "false",
    allow_server_selection = "false",
    allow_authentication = "false"
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
