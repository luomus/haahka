suppressPackageStartupMessages({

  library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
  library(haahka, warn.conflicts = FALSE, quietly = TRUE)
  library(highcharter, warn.conflicts = FALSE, quietly = TRUE)
  library(logger, warn.conflicts = FALSE, quietly = TRUE)
  library(markdown, warn.conflicts = FALSE, quietly = TRUE)
  library(pool, warn.conflicts = FALSE, quietly = TRUE)
  library(RPostgres, warn.conflicts = FALSE, quietly = TRUE)
  library(shiny, warn.conflicts = FALSE, quietly = TRUE)
  library(shiny.i18n, warn.conflicts = FALSE, quietly = TRUE)
  library(shinycssloaders, warn.conflicts = FALSE, quietly = TRUE)
  library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE)
  library(shinydashboardPlus, warn.conflicts = FALSE, quietly = TRUE)
  library(shinyWidgets, warn.conflicts = FALSE, quietly = TRUE)
  library(tidyr, warn.conflicts = FALSE, quietly = TRUE)
  library(utils, warn.conflicts = FALSE, quietly = TRUE)

})


api <- paste0(
  "http://", Sys.getenv("API_HOSTNAME"), ":", Sys.getenv("API_PORT"), "/api"
)

logger::log_layout(layout_glue_colors)

logger::log_threshold(TRACE)

download.file(
  paste0(api, "/data/sp_images.zip"), "www/img/sp_images.zip", quiet = TRUE
)

utils::unzip("www/img/sp_images.zip", exdir = "www/img/sp_images")

unlink("www/img/sp_images.zip")

con <- pool::dbPool(RPostgres::Postgres(), dbname = Sys.getenv("DB_NAME"))

sp_data <- readRDS("taxa.rds")

translator <- shiny.i18n::Translator[["new"]](
  translation_json_path = "translation.json"
)

metadata <- readRDS(url(paste0(api, "/data/photo_metadata.rds")))

descriptions <- readRDS(url(paste0(api, "/data/descriptions.rds")))

desc <- utils::packageDescription("haahka")
version <- desc[["Version"]]
repo_url <- desc[["URL"]]
author <- desc[["Author"]]
author_email <- desc[["AuthorEmail"]]
feedback <- "helpdesk@laji.fi"
data_url <- "https://tun.fi/HR.2931"
default_species <- "CYGCYG"
time_units <- 30 * 24 * 3600 * 1000
xmin <- highcharter::datetime_to_timestamp(as.Date("2000-01-01", tz = "UTC"))
xmax <- highcharter::datetime_to_timestamp(as.Date("2000-12-31", tz = "UTC"))
x_yearly_labels <- list(
  "fi" = haahka::get_months("fi", "short"),
  "en" = haahka::get_months("en", "short"),
  "se" = haahka::get_months("se", "short")
)
pb_list <- list(
  list(
    from = haahka::get_timestamp("2000-02-01"),
    to = haahka::get_timestamp("2000-03-01"),
    color = "rgba(240, 240, 245, 0.4)"
  ),
  list(
    from = haahka::get_timestamp("2000-04-01"),
    to = haahka::get_timestamp("2000-05-01"),
    color = "rgba(240, 240, 245, 0.4)"
  ),
  list(
    from = haahka::get_timestamp("2000-06-01"),
    to = haahka::get_timestamp("2000-07-01"),
    color = "rgba(240, 240, 245, 0.4)"
  ),
  list(
    from = haahka::get_timestamp("2000-08-01"),
    to = haahka::get_timestamp("2000-09-01"),
    color = "rgba(240, 240, 245, 0.4)"
  ),
  list(
    from = haahka::get_timestamp("2000-10-01"),
    to = haahka::get_timestamp("2000-11-01"),
    color = "rgba(240, 240, 245, 0.4)"
  ),
  list(
    from = haahka::get_timestamp("2000-12-01"),
    to = haahka::get_timestamp("2000-12-31"),
    color = "rgba(240, 240, 245, 0.4)"
  )
)

choices <- translator[["get_languages"]]()
names(choices) <- vapply(choices, haahka::get_languages, "")

ui <- function(request) {
  shinydashboardPlus::dashboardPage(
    title = "Haahka - muuttolintuselain",
    shinydashboardPlus::dashboardHeader(
      title = shiny::tags[["a"]](
        href = "https://haahka.laji.fi",
        shiny::tags[["img"]](src = "browser_logo.png", height = "40")
      )
    ),
    shinydashboardPlus::dashboardSidebar(
      collapsed = FALSE,
      shiny::selectInput(
        "language",
        label = "Kieli / Språk / Language",
        choices = choices
      ),
      shinydashboard::sidebarMenuOutput("render_sidebarmenu"),
      shiny::hr(),
      shiny::uiOutput("render_sponsors"),
      shiny::uiOutput("render_sidebarfooter")
    ),
    shinydashboard::dashboardBody(
      shiny::tags[["head"]](
        shiny::tags[["link"]](
          rel = "stylesheet", type = "text/css", href = "custom.css"
        ),
        shiny::tags[["script"]](
          defer = NA,
          `data-domain` = "haahka.laji.fi",
          src = "https://plausible.io/js/script.js"
        )
      ),
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "species",
          shiny::fluidPage(
            shiny::tags[["script"]](
              '
                $(document).on("shiny:connected", function(e) {
                  var jsWidth = screen.width;
                  Shiny.onInputChange("GetScreenWidth",jsWidth);
                });
              '
            ),
            shiny::fluidRow(
              shiny::column(
                6,
                shinydashboardPlus::box(
                  width = 12, shiny::uiOutput("render_species")
                ),
                shinydashboardPlus::box(
                  width = 12,
                  shinycssloaders::withSpinner(
                    shiny::uiOutput("render_description"),
                    type = 8,
                    size = 0.5
                  )
                )
              ),
              shiny::column(
                6,
                shinydashboardPlus::box(
                  width = 12,
                  shinycssloaders::withSpinner(
                    highcharter::highchartOutput("migration", height = "300px"),
                    type = 8,
                    size = 0.5
                  ),
                  shiny::actionButton(
                    inputId = "migration_info",
                    label = NULL,
                    icon = shiny::icon("info", class = "icon-info"),
                    class = "btn-info btn-small-info"
                  )
                ),
                shinydashboardPlus::box(
                  width = 12,
                  shinycssloaders::withSpinner(
                    highcharter::highchartOutput("local", height = "300px"),
                    type = 8,
                    size = 0.5
                  ),
                  shiny::actionButton(
                    inputId = "local_info",
                    label = NULL,
                    icon = shiny::icon("info", class = "icon-info"),
                    class = "btn-info btn-small-info"
                  )
                ),
                shinydashboardPlus::box(
                  width = 12,
                  shinycssloaders::withSpinner(
                    highcharter::highchartOutput("change", height = "300px"),
                    type = 8,
                    size = 0.5
                  ),
                  shiny::actionButton(
                    inputId = "change_info",
                    label = NULL,
                    icon = shiny::icon("info", class = "icon-info"),
                    class = "btn-info btn-small-info"
                  )
                ),
                shinydashboardPlus::box(
                  width = 12,
                  shiny::uiOutput("render_median"),
                  shiny::actionButton(
                    inputId = "median_info",
                    label = NULL,
                    icon = shiny::icon("info", class = "icon-info"),
                    class = "btn-info btn-small-info"
                  )
                ),
                shiny::uiOutput("change_numbers"),
                shiny::uiOutput("records")
              )
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "help", shiny::uiOutput("render_helpsections")
        )
      )
    )
  )
}

server <- function(input, output, session) {

  shiny::setBookmarkExclude(
    c(
      "bm1",
      "change_info",
      "GetScreenWidth",
      "language-selectized",
      "local_info",
      "median_info",
      "migration_info",
      "sidebarCollapsed",
      "sidebarItemExpanded",
      "species-selectized"
    )
  )

  logger::log_info("Started new session {session[['token']]}")

  i18n <- shiny::reactive({

    lang <- input[["language"]]

    if (length(lang) > 0) {

      logger::log_debug("Language changed to: {lang}")

      translator[["set_translation_language"]](lang)

    }

    translator

  })

  get_current_sp <- shiny::reactive({

    shiny::req(input[["species"]])

    dplyr::filter(sp_data, .data[["Sci_name"]] == input[["species"]])

  })

  get_current_data <- shiny::reactive({

    sp_current <- get_current_sp()

    dplyr::tbl(con, paste0(sp_current[["Species_Abb"]], "_data"))

  })

  get_current_meta <- shiny::reactive({

    sp_current <- get_current_sp()

    metadata[[sp_current[["Species_Abb"]]]]

  })

  get_current_description <- shiny::reactive({

    sp_current <- get_current_sp()

    descriptions[[sp_current[["Species_Abb"]]]]

  })

  get_current_records <- shiny::reactive({

    sp_current <- get_current_sp()

    dplyr::tbl(con, paste0(sp_current[["Species_Abb"]], "_records"))

  })

  get_current_stats <- shiny::reactive({

    sp_current <- get_current_sp()

    dplyr::tbl(con, paste0(sp_current[["Species_Abb"]], "_stats"))

  })

  output[["render_sidebarmenu"]] <- shinydashboard::renderMenu({

    shiny::req(input[["language"]])

    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem(
        i18n()[["t"]]("Lajikohtaiset havainnot"),
        tabName = "species",
        selected = TRUE,
        icon = shiny::icon("binoculars")
      ),
      shinydashboard::menuItem(
        i18n()[["t"]]("Ohjeet"),
        tabName = "help",
        icon = shiny::icon("question")
      )
    )
  })

  output[["render_sponsors"]] <- shinydashboard::renderMenu({

    shiny::req(input[["language"]])

    shiny::tagList(
      shiny::div(
        class = "sponsors",
        i18n()[["t"]]("Haahkaa tukevat"),
        shiny::br(),
        shiny::a(
          href = "https://kordelin.fi/en/frontpage",
          shiny::img(src = "img/kordelin_logo_300_173.png")
        ),
        shiny::br(),
        shiny::a(
          href = "https://www.tringa.fi",
          shiny::img(src = "img/tringa_logo_300_300.png")
        ),
        shiny::br(),
        shiny::a(
          href = "https://laji.fi/",
          shiny::img(src = "https://cdn.laji.fi/images/logos/LAJI_FI_valk.png")
        ),
        shiny::br(),
        shiny::a(
          href = "https://luomus.fi/",
          shiny::img(
            src =
              "https://cdn.laji.fi/images/partners/luomus_fi_blue_smaller.gif"
          )
        )
      )
    )

  })

  output[["render_sidebarfooter"]] <- shinydashboard::renderMenu({

    shiny::req(input[["language"]])

    app_prefix <- i18n()[["t"]]("Sovellusversio")

    shiny::tagList(
      shiny::HTML("<footer>"),
      shiny::div(
        class = "footer-content",
        shiny::strong("Muuttolintuselain Haahka"),
        shiny::br(),
        paste0(app_prefix, ": ", version),
        shiny::br(),
        i18n()[["t"]]("Palaute: "),
        shiny::a(href = paste0("mailto:", feedback), feedback),
        shiny::br(),
        shiny::br(),
        shiny::a(href = repo_url, gsub("https://", "", repo_url)),
        shiny::br(),
        shiny::br(),
        "© 2018 ",
        shiny::a(href = paste0("mailto:", author_email), author),
        shiny::br(),
        shiny::br(),
        shiny::a(href = "https://opensource.org/licenses/MIT", "MIT"),
        paste0(" ", tolower(i18n()[["t"]]("Lisenssi")))
      ),
      shiny::HTML("</footer>")
    )
  })

  output[["render_species"]] <- shiny::renderUI({

    shiny::req(input[["language"]])

    name_field <- input[["language"]]

    spps <- haahka::get_species_names(name_field, sp_data)

    selected_sp <- dplyr::filter(
      sp_data, .data[["Species_Abb"]] == default_species
    )
    selected_sp <- dplyr::pull(selected_sp, "Sci_name")
    selected_sp <- spps[which(spps == selected_sp)]

    shiny::div(
      id = "large",
      shiny::selectInput(
        "species",
        label = i18n()[["t"]](
          "Valitse laji listasta tai tyhjennä kenttä ja kirjoita lajinimi"
        ),
        choices = spps,
        selected = selected_sp
      )
    )

  })

  output[["render_citation"]] <- renderUI({

    shiny::req(input[["language"]])

    current_sp <- get_current_sp()

    now <- format(Sys.time(), format = "%Y-%m-%d")

    title <- i18n()[["t"]]("Viittausohje")

    text_fi <- paste(
      paste0(current_sp[["FIN_name"]], "."),
      "Helsingin Seudun Lintutieteellinen Yhdistys Tringa ry. ",
      "Hangon lintuaseman aineisto: päiväsummat.",
      paste0("[", data_url, "]"),
      "[Viitattu",
      now,
      "]"
    )

    text_se <- paste(
      paste0(current_sp[["SWE_name"]], "."),
      "Helsingforstraktens Ornitologiska Förening Tringa rf.",
      "Dataset från Hangö fågelstation: dagliga totalantal.",
      paste0("[", data_url, "]"),
      "[Citerad",
      now,
      "]"
    )

    text_en <- paste(
      paste0(current_sp[["ENG_name"]], "."),
      "Ornithological society of Helsinki Tringa ry.",
      "Data of the Hanko Bird Observatory: Day counts.",
      paste0("[", data_url, "]"),
      "[Cited",
      now,
      "]"
    )

    payload <- switch(
      input[["language"]],
      fi = shiny::tagList(
        shiny::h4(title), shiny::p(text_fi, shiny::br(), text_en)
      ),
      se = shiny::tagList(
        shiny::h4(title), shiny::p(text_se, shiny::br(), text_en)
      ),
      shiny::tagList(shiny::h4(title), shiny::p(text_en))
    )

    shiny::tagList(shiny::div(payload, class = "description"))

  })

  output[["render_image"]] <- shiny::renderUI({

    current_sp <- get_current_sp()

    current_meta <- get_current_meta()

    sp_abbr <- current_sp[["Species_Abb"]]

    img_file <- list.files(
      file.path("www", "img", "sp_images"),
      pattern = paste0("(", sp_abbr, ")"),
      full.names = TRUE
    )

    payload <- shiny::p(
      i18n()[["t"]]("Kuvausteksti tulossa myöhemmin"), class = "description"
    )

    cond <- length(img_file) > 0 && file.exists(img_file)

    if (cond) {

      caption <- current_meta[["caption"]]

      file_basename <- basename(img_file)

      payload <- shiny::div(
        shiny::img(
          src = file.path("img", "sp_images", file_basename),
          width = "90%",
          class = "description"
        ),
        shiny::p(shiny::HTML(caption), class = "description"),
        shiny::br()
      )

    }

    payload

  })

  output[["render_description"]] <- shiny::renderUI({

    current_sp <- get_current_sp()

    current_desc <- get_current_description()

    description <- haahka::parse_description(current_desc, input[["language"]])

    sp_abbr <- current_sp[["Species_Abb"]]

    sci_name <- current_sp[["Sci_name"]]

    common_name <- switch(
      input[["language"]],
      en = current_sp[["ENG_name"]],
      fi = current_sp[["FIN_name"]],
      se = current_sp[["SWE_name"]]
    )

    shiny::withTags(
      shiny::div(
        shiny::h2(common_name, class = "description"),
        shiny::h3(sci_name, class = "description sci-name"),
        shiny::br(),
        shiny::uiOutput("render_image"),
        shiny::div(shiny::HTML(description), class = "description"),
        shiny::br(),
        shiny::uiOutput("render_citation")
      )
    )

  })

  output[["migration"]] <- highcharter::renderHighchart({

    obs_current <- get_current_data()

    plot_data <- dplyr::select(obs_current, dplyr::all_of(c("day", "muutto")))
    plot_data <- dplyr::collect(plot_data)
    plot_data <- dplyr::mutate(
      plot_data, day = as.Date(paste(2000, .data[["day"]]), format = "%Y %j")
    )
    plot_data <- dplyr::arrange(plot_data, .data[["day"]])
    plot_data <- haahka::tile_observations(plot_data, "muutto")

    if (!is.null(plot_data)) {

      hcoptslang <- getOption("highcharter.lang")

      hcoptslang[["shortMonths"]] <- x_yearly_labels[[input[["language"]]]]

      options(highcharter.lang = hcoptslang)

      hc <- highcharter::hchart(
        plot_data,
        type = "line",
        highcharter::hcaes(.data[["day"]], .data[["value_avgs"]]),
        name = i18n()[["t"]]("Muuttajamäärien keskiarvot"),
        color = "#1f78b4"
      )
      hc <- highcharter::hc_yAxis(
        hc, title = list(text = i18n()[["t"]]("Yksilöä / havaintopäivä"))
      )
      hc <- highcharter::hc_xAxis(
        hc,
        title = list(text = ""),
        type = "datetime",
        min = xmin,
        max = xmax,
        dateTimeLabelFormats = list(month = "%b"),
        tickInterval = time_units,
        plotBands = pb_list
      )
      hc <- highcharter::hc_plotOptions(
        hc,
        line = list(marker = list(enabled = input[["show_markers"]])),
        spline = list(marker = list(enabled = input[["show_markers"]]))
      )
      hc <- highcharter::hc_title(
        hc, text = i18n()[["t"]]("Muuttajamäärien keskiarvot")
      )
      hc <- highcharter::hc_tooltip(
        hc,
        crosshairs = TRUE,
        backgroundColor = "#FCFFC5",
        xDateFormat = "%b %d"
      )
      hc <- highcharter::hc_exporting(hc, enabled = TRUE)

      highcharter::hc_chart(hc, zoomType = "xy")

    }

  })

  output[["local"]] <- highcharter::renderHighchart({

    obs_current <- get_current_data()

    plot_data <- dplyr::select(obs_current, dplyr::all_of(c("day", "paik")))
    plot_data <- dplyr::collect(plot_data)
    plot_data <- dplyr::mutate(
      plot_data, day = as.Date(paste(2000, .data[["day"]]), format = "%Y %j")
    )
    plot_data <- dplyr::arrange(plot_data, .data[["day"]])
    plot_data <- haahka::tile_observations(plot_data, "paik")

    if (!is.null(plot_data)) {

      hcoptslang <- getOption("highcharter.lang")

      hcoptslang[["shortMonths"]] <- x_yearly_labels[[input[["language"]]]]

      options(highcharter.lang = hcoptslang)

      hc <- highcharter::hchart(
        plot_data,
        type = "line",
        highcharter::hcaes(.data[["day"]], .data[["value_avgs"]]),
        name = i18n()[["t"]]("Paikallisten määrien keskiarvot"),
        color = "#1f78b4"
      )
      hc <- highcharter::hc_yAxis(
        hc, title = list(text = i18n()[["t"]]("Yksilöä / havaintopäivä"))
      )
      hc <- highcharter::hc_xAxis(
        hc,
        title = list(text = ""),
        type = "datetime",
        min = xmin,
        max = xmax,
        dateTimeLabelFormats = list(month = "%b"),
        tickInterval = time_units,
        plotBands = pb_list
      )
      hc <- highcharter::hc_title(
        hc, text = i18n()[["t"]]("Paikallisten määrien keskiarvot")
      )
      hc <- highcharter::hc_tooltip(
        hc,
        crosshairs = TRUE,
        backgroundColor = "#FCFFC5",
        xDateFormat = "%b %d"
      )
      hc <- highcharter::hc_exporting(hc, enabled = TRUE)

      highcharter::hc_chart(hc, zoomType = "xy")

    }

  })

  output[["change"]] <- highcharter::renderHighchart({

    obs_current <- get_current_data()

    if (!is.null(obs_current)) {

      plot_data_p1 <- dplyr::select(
        obs_current, dplyr::all_of(c("day", "totalp1"))
      )
      plot_data_p1 <- dplyr::collect(plot_data_p1)
      plot_data_p1 <- dplyr::mutate(
        plot_data_p1,
        day = as.Date(paste(2000, .data[["day"]]), format = "%Y %j")
      )
      plot_data_p1 <- dplyr::arrange(plot_data_p1, .data[["day"]])
      plot_data_p1 <- haahka::tile_observations(plot_data_p1, "totalp1")
      plot_data_p1 <- dplyr::rename(
        plot_data_p1, totalp1 = dplyr::all_of("value_avgs")
      )

      plot_data_p2 <- dplyr::select(
        obs_current, dplyr::all_of(c("day", "totalp2"))
      )
      plot_data_p2 <- dplyr::collect(plot_data_p2)
      plot_data_p2 <- dplyr::mutate(
        plot_data_p2,
        day = as.Date(paste(2000, .data[["day"]]), format = "%Y %j")
      )
      plot_data_p2 <- dplyr::arrange(plot_data_p2, .data[["day"]])
      plot_data_p2 <- haahka::tile_observations(plot_data_p2, "totalp2")
      plot_data_p2 <- dplyr::rename(
        plot_data_p2, totalp2 = dplyr::all_of("value_avgs")
      )

      plot_data_p3 <- dplyr::select(
        obs_current, dplyr::all_of(c("day", "totalp3"))
      )
      plot_data_p3 <- dplyr::collect(plot_data_p3)
      plot_data_p3 <- dplyr::mutate(
        plot_data_p3,
        day = as.Date(paste(2000, .data[["day"]]), format = "%Y %j")
      )
      plot_data_p3 <- dplyr::arrange(plot_data_p3, .data[["day"]])
      plot_data_p3 <- haahka::tile_observations(plot_data_p3, "totalp3")
      plot_data_p3 <- dplyr::rename(
        plot_data_p3, totalp3 = dplyr::all_of("value_avgs")
      )

      plot_data_p4 <- dplyr::select(
        obs_current, dplyr::all_of(c("day", "totalp4"))
      )
      plot_data_p4 <- dplyr::collect(plot_data_p4)
      plot_data_p4 <- dplyr::mutate(plot_data_p4,
        day = as.Date(paste(2000, .data[["day"]]), format = "%Y %j")
      )
      plot_data_p4 <- dplyr::arrange(plot_data_p4, .data[["day"]])
      plot_data_p4 <- haahka::tile_observations(plot_data_p4, "totalp4")
      plot_data_p4 <- dplyr::rename(
        plot_data_p4, totalp4 = dplyr::all_of("value_avgs")
      )

      plot_data <- dplyr::left_join(
        plot_data_p1, plot_data_p2, by = c("day" = "day")
      )
      plot_data <- dplyr::left_join(
        plot_data, plot_data_p3, by = c("day" = "day")
      )
      plot_data <- dplyr::left_join(
        plot_data, plot_data_p4, by = c("day" = "day")
      )
      plot_data <- tidyr::pivot_longer(
        plot_data, -dplyr::all_of("day"), names_to = "epoch"
      )
      plot_data <- dplyr::mutate(
        plot_data,
        epoch = factor(
          .data[["epoch"]],
          c("totalp1", "totalp2", "totalp3", "totalp4"),
          ordered = TRUE
        )
      )

      hcoptslang <- getOption("highcharter.lang")

      hcoptslang[["shortMonths"]] <- x_yearly_labels[[input[["language"]]]]

      options(highcharter.lang = hcoptslang)

      hc <- highcharter::hchart(
        plot_data,
        type = "line",
        highcharter::hcaes(
          .data[["day"]], .data[["value"]], group = .data[["epoch"]]
        ),
        name = c("1979-1999", "2000-2009", "2010-2019", "2020-"),
        color = c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF", "#D62728FF")
      )
      hc <- highcharter::hc_yAxis(
        hc, title = list(text = i18n()[["t"]]("Yksilöä / havaintopäivä"))
      )
      hc <- highcharter::hc_xAxis(
        hc,
        title = list(text = ""),
        type = "datetime",
        min = xmin,
        max = xmax,
        dateTimeLabelFormats = list(month = "%b"),
        tickInterval = time_units,
        plotBands = pb_list
      )
      hc <- highcharter::hc_plotOptions(
        hc, line = list(marker = list(enabled = FALSE))
      )
      hc <- highcharter::hc_title(
        hc, text = i18n()[["t"]]("Runsauksien muutokset")
      )
      hc <- highcharter::hc_tooltip(
        hc,
        crosshairs = TRUE,
        backgroundColor = "#FCFFC5",
        shared = TRUE,
        xDateFormat = "%b %d"
      )
      hc <- highcharter::hc_exporting(hc, enabled = TRUE)

      highcharter::hc_chart(hc, zoomType = "xy")

    }

  })

  output[["change_numbers"]] <- renderUI({

    stats_current <- get_current_stats()
    stats_current <- dplyr::collect(stats_current)

    long <- stats_current[["slopeLong"]]

    short <- stats_current[["slopeShort"]]

    lt_number_color <- "gray"
    lt_number_icon <- shiny::icon(NULL)
    lt_number <- "-"

    lgt <- !is.na(long) & long > 0

    if (lgt) {

      lt_number_color <- "green"
      lt_number_icon <- shiny::icon("caret-up")
      lt_number <- paste0("+", long, "%")

    }

    llt <- !is.na(long) & long < 0

    if (llt) {

      lt_number_color <- "red"
      lt_number_icon <- shiny::icon("caret-down")
      lt_number <- paste0(long, "%")

    }

    st_number_color <- "gray"
    st_number_icon <- shiny::icon(NULL)
    st_number <- "-"

    sgt <- !is.na(short) & short > 0

    if (sgt) {

      st_number_color <- "green"
      st_number_icon <- shiny::icon("caret-up")
      st_number <- paste0("+", short, "%")

    }

    slt <- !is.na(short) & short < 0

    if (slt) {

      st_number_color <- "red"
      st_number_icon <- shiny::iconicon("caret-down")
      st_number <- paste0(short, "%")

    }

    shinydashboardPlus::box(
      width = 12,
      solidHeader = FALSE,
      title = i18n()[["t"]]("Runsauden muutokset"),
      background = NULL,
      status = "danger",
      footer = shiny::tagList(
        shiny::p(
          i18n()[["t"]](
            paste(
              "Pitkän aikavälin muutos",
              "=",
              "keskirunsauden muutos aikajaksolta 1979-1999 aikajaksolle 2020-."
            )
          ),
          shiny::br(),
          i18n()[["t"]](
            paste(
              "Lyhyen aikavälin muutos",
              "=",
              "keskirunsauden muutos aikajaksolta 2010-2019 aikajaksolle 2020-."
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shinydashboardPlus::descriptionBlock(
            number = lt_number,
            numberColor = lt_number_color,
            numberIcon = lt_number_icon,
            header = "",
            text = i18n()[["t"]]("Pitkän aikavälin muutos"),
            rightBorder = TRUE,
            marginBottom = TRUE
          )
        ),
        shiny::column(
          width = 6,
          shinydashboardPlus::descriptionBlock(
            number = st_number,
            numberColor = st_number_color,
            numberIcon = st_number_icon,
            header = "",
            text = i18n()[["t"]]("Lyhyen aikavälin muutos"),
            rightBorder = TRUE,
            marginBottom = TRUE
          )
        ),
        shiny::column(
          width = 12,
          shinydashboardPlus::descriptionBlock(
            header = i18n()[["t"]]("Päivittäiset keskirunsaudet yhteensä"),
            rightBorder = TRUE,
            marginBottom = FALSE
          )
        ),
        shiny::column(
          width = 3,
          shinydashboardPlus::descriptionBlock(
            header = format(round(stats_current[["Np1"]], 0), big.mark = " "),
            text = "1979-1999",
            rightBorder = TRUE,
            marginBottom = FALSE
          )
        ),
        shiny::column(
          width = 3,
          shinydashboardPlus::descriptionBlock(
            header = format(round(stats_current[["Np2"]], 0), big.mark = " "),
            text = "2000-2009",
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        ),
        shiny::column(
          width = 3,
          shinydashboardPlus::descriptionBlock(
            header = format(round(stats_current[["Np3"]], 0), big.mark = " "),
            text = "2010-2019",
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        ),
        shiny::column(
          width = 3,
          shinydashboardPlus::descriptionBlock(
            header = format(round(stats_current[["Np4"]], 0), big.mark = " "),
            text = "2020-",
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        )
      )
    )
  })

  output[["render_median"]] <- shiny::renderUI({

    screen_size <- input[["GetScreenWidth"]]

    height <- NULL

    if (screen_size > 600) {

      height <- "200px"

    }

    shinycssloaders::withSpinner(
      highcharter::highchartOutput("migration_medians", height = height),
      type = 8,
      size = 0.5
    )

  })

  output[["migration_medians"]] <- highcharter::renderHighchart({

    sp_current <- get_current_sp()

    origin <- as.Date("2000-01-01")

    plot_data <- get_current_stats()
    plot_data <- dplyr::collect(plot_data)
    plot_data <- dplyr::select(
      plot_data,
      dplyr::all_of(
        c(
          "sphenp1",
          "sphenp2",
          "sphenp3",
          "sphenp4",
          "aphenp1",
          "aphenp2",
          "aphenp3",
          "aphenp4"
        )
      )
    )
    plot_data <- tidyr::pivot_longer(
      plot_data, dplyr::everything(), names_to = "variable"
    )
    plot_data <- tidyr::separate(
      plot_data, col = "variable", into = c("season", "epoch"), sep = "phen"
    )
    plot_data <- dplyr::mutate(
      plot_data,
      season = ifelse(
        .data[["season"]] == "s",
        tolower(i18n()[["t"]]("Kevät")),
        ifelse(.data[["season"]] == "a", tolower(i18n()[["t"]]("Syys")), NA)
      ),
      epoch = factor(
        .data[["epoch"]],
        levels = c("p1", "p2", "p3", "p4"),
        labels = rev(c("1979-1999", "2000-2009", "2010-2019", "2020-")),
        ordered = TRUE
      ),
      epochnum = as.numeric(.data[["epoch"]]) - 1,
      date = origin + .data[["value"]],
      date_print = haahka::make_date_label(
        .data[["date"]], input[["language"]]
      )
    )

    hcoptslang <- getOption("highcharter.lang")

    hcoptslang[["shortMonths"]] <- x_yearly_labels[[input[["language"]]]]

    options(highcharter.lang = hcoptslang)

    hc <- highcharter::hchart(
      plot_data,
      type = "scatter",
      highcharter::hcaes(
        .data[["date"]],
        .data[["epochnum"]],
        group = .data[["epoch"]]
      ),
      name = c("1979-1999", "2000-2009", "2010-2019", "2020-"),
      color = c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF", "#D62728FF")
    )
    hc <- highcharter::hc_yAxis(
      hc,
      title = list(text = ""),
      min = 0,
      max = 3,
      categories = rev(levels(plot_data[["epoch"]]))
    )
    hc <- highcharter::hc_xAxis(
      hc,
      title = list(text = ""),
      type = "datetime",
      min = xmin,
      max = xmax,
      dateTimeLabelFormats = list(month = "%b"),
      tickInterval = time_units,
      plotBands = pb_list
    )
    hc <- highcharter::hc_plotOptions(
      hc, scatter = list(marker = list(symbol = "circle", radius = 8))
    )
    hc <- highcharter::hc_title(
      hc, text = i18n()[["t"]]("Muuton ajoittumisen mediaanipäivämäärä")
    )
    hc <- highcharter::hc_tooltip(
      hc,
      crosshairs = TRUE,
      backgroundColor = "#FCFFC5",
      shared = TRUE,
      xDateFormat = "%b %d",
      pointFormat = paste0(
        "{point.season}",
        ifelse(input[["language"]] == "fi", "", " "),
        tolower(i18n()[["t"]]("Muuton ajoittumisen mediaanipäivämäärä")),
        ":",
        "<br> {point.date_print}"
      )
    )
    hc <- highcharter::hc_exporting(hc, enabled = TRUE)

    highcharter::hc_chart(hc, zoomType = "xy")

  })

  output[["records"]] <- shiny::renderUI({

    records_current <- get_current_records()
    records_current <- dplyr::collect(records_current)
    records_current <- dplyr::filter(records_current, !is.na(.data[["period"]]))
    records_current <- dplyr::mutate(
      records_current,
      date = as.Date(paste(.data[["year"]], .data[["day"]]), "%Y %j")
    )

    payload <- NULL

    if (nrow(records_current) > 0) {

      payload <- shinydashboardPlus::box(
        width = 12,
        solidHeader = FALSE,
        title = i18n()[["t"]]("Runsausennätykset"),
        status = "info",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::tagList(shiny::h4(i18n()[["t"]]("Kevät"), class = "record")),
            shiny::column(
              width = 6,
              shiny::tagList(
                shiny::h5(i18n()[["t"]]("Muuttavat"), class = "record"),
                shiny::div(
                  class = "record",
                  shiny::column(
                    width = 3,
                    shiny::icon("trophy", class = "icon-record icon-gold")
                  ),
                  shiny::column(
                    width = 9,
                    shiny::p(
                      get_value("Spring", "Migr", "Sum", records_current, i18n),
                      class = "record-number"
                    )
                  ),
                  shiny::column(
                    width = 3, shiny::icon("calendar", class = "icon-record")
                  ),
                  shiny::column(
                    width = 9,
                    shiny::p(
                      get_value(
                        "Spring", "Migr", "date_string", records_current, i18n
                      )
                    )
                  )
                )
              )
            ),
            shiny::column(
              width = 6,
              shiny::tagList(
                shiny::h5(i18n()[["t"]]("Paikalliset"), class = "record"),
                shiny::div(
                  class = "record",
                  shiny::column(
                    width = 3,
                    shiny::icon("trophy", class = "icon-record icon-gold")
                  ),
                  shiny::column(
                    width = 9,
                    shiny::p(
                      get_value(
                        "Spring", "Local", "Sum", records_current, i18n
                      ),
                      class = "record-number"
                    )
                  ),
                  shiny::column(
                    width = 3, shiny::icon("calendar", class = "icon-record")
                  ),
                  shiny::column(
                    width = 9,
                    shiny::p(
                      get_value(
                        "Spring", "Local", "date_string", records_current, i18n
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::tagList(shiny::h4(i18n()[["t"]]("Syksy"), class = "record")),
            shiny::column(
              width = 6,
              shiny::tagList(
                shiny::h5(i18n()[["t"]]("Muuttavat"), class = "record"),
                shiny::div(
                  class = "record",
                  shiny::column(
                    width = 3,
                    shiny::icon("trophy", class = "icon-record icon-gold")
                  ),
                  shiny::column(
                    width = 9,
                    shiny::p(
                      get_value("Autumn", "Migr", "Sum", records_current, i18n),
                      class = "record-number"
                    )
                  ),
                  shiny::column(
                    width = 3, shiny::icon("calendar", class = "icon-record")
                  ),
                  shiny::column(
                    width = 9,
                    shiny::p(
                      get_value(
                        "Autumn", "Migr", "date_string", records_current, i18n
                      )
                    )
                  )
                )
              )
            ),
            shiny::column(
              width = 6,
              shiny::tagList(
                shiny::h5(i18n()[["t"]]("Paikalliset"), class = "record"),
                shiny::div(
                  class = "record",
                  shiny::column(
                    width = 3,
                    shiny::icon("trophy", class = "icon-record icon-gold")
                  ),
                  shiny::column(
                    width = 9,
                    shiny::p(
                      get_value(
                        "Autumn", "Local", "Sum", records_current, i18n
                      ),
                      class = "record-number"
                    )
                  ),
                  shiny::column(
                    width = 3, shiny::icon("calendar", class = "icon-record")
                  ),
                  shiny::column(
                    width = 9,
                    shiny::p(
                      get_value(
                        "Autumn", "Local", "date_string", records_current, i18n
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

    }

    payload

  })

  output[["render_helpsections"]] <- shiny::renderUI({

    data_help_file <- file.path(
      "www", "helps", paste0("data_help-", input[["language"]], ".md")
    )

    data_help_content <- ""

    if (file.exists(data_help_file)) {

      data_help_content <- shiny::tagList(
        shiny::div(
          class = "help-container", shiny::includeMarkdown(data_help_file)
        )
      )

    }

    app_help_file <- file.path(
      "www", "helps", paste0("app_help-", input[["language"]], ".md")
    )

    app_help_content <- ""

    if (file.exists(app_help_file)) {

      app_help_content <- shiny::tagList(
        shiny::div(
          class = "help-container", shiny::includeMarkdown(app_help_file)
        )
      )

    }

    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shinydashboardPlus::userBox(
            title = shinydashboardPlus::userDescription(
              title = i18n()[["t"]]("Aineisto"),
              subtitle = i18n()[["t"]](
                "Hangon lintuaseman pitkäaikaisaineisto"
              ),
              type = NULL,
              image = "database.png"
            ),
            width = 12,
            background = "navy",
            closable = FALSE,
            collapsible = FALSE,
            data_help_content
          )
        ),
        shiny::column(
          width = 6,
          shinydashboardPlus::userBox(
            title = shinydashboardPlus::userDescription(
              title = i18n()[["t"]]("Verkkosovellus"),
              subtitle = "Muuttolintuselain Haahka",
              type = NULL,
              image = "settings.png"
            ),
            width = 12,
            background = "navy",
            closable = FALSE,
            collapsible = FALSE,
            app_help_content
          )
        )
      )
    )
  })

  shiny::observe({

    shiny::reactiveValuesToList(input)

    session[["doBookmark"]]()

  })

  shiny::onBookmarked(function(url) shiny::updateQueryString(url))

  shiny::observeEvent(
    input[["species"]], {
      logger::log_debug("Species changed to: {input[['species']]}")
    }
  )

  shiny::observeEvent(
    input[["language"]], {
      shiny::updateSelectInput(
        session, "language", selected = input[["language"]]
      )
    }
  )

  shiny::observeEvent(
    i18n(),
    shiny::updateSelectInput(
      session,
      "species",
      label =  i18n()[["t"]]("Valitse laji"),
      choices = get_species_names(input[["language"]], sp_data),
      selected = input[["species"]]
    )
  )

  shiny::observeEvent(
    input[["migration_info"]],
    {
      shiny::req(input[["language"]])
      haahka::create_popup(session, "migration_info-", input[["language"]])
    }
  )

  shiny::observeEvent(
    input[["local_info"]],
    {
      shiny::req(input[["language"]])
      haahka::create_popup(session, "local_info-", input[["language"]])
    }
  )

  shiny::observeEvent(
    input[["change_info"]],
    {
      shiny::req(input[["language"]])
      haahka::create_popup(session, "change_info-", input[["language"]])
    }
  )

  shiny::observeEvent(
    input[["median_info"]],
    {
      shiny::req(input[["language"]])
      haahka::create_popup(session, "median_info-", input[["language"]])
    }
  )

  session[["onSessionEnded"]](
    function() logger::log_info("Session {session[['token']]} stopped")
  )

}

shiny::shinyApp(ui, server, enableBookmarking = "url")
