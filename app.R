library(ggsci)
library(glue)
library(highcharter)
library(httr2)
library(logger)
library(lubridate)
library(markdown)
library(officer)
library(pool)
library(RPostgres)
library(shiny)
library(shiny.i18n)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(tidyverse)
library(tsibble)
library(yaml)
library(haahka)

req <- request(
  paste0("http://", Sys.getenv("API_HOSTNAME"), ":", Sys.getenv("API_PORT"))
)

# Set up logger ----------------------------------------------------------------

logger::log_layout(layout_glue_colors)
logger::log_threshold(TRACE)

# Images -----------------------------------------------------------------------

download.file(paste0(req$url, "/data/sp_images.zip"), "www/img/sp_images.zip")

unzip("www/img/sp_images.zip", exdir = "www/img/sp_images")

unlink("www/img/sp_images.zip")

# Connect database -------------------------------------------------------------

con <- dbPool(Postgres(), dbname = Sys.getenv("DB_NAME"))

# Read species definition data
sp_data <- readRDS("taxa.rds")

# Translation data
translator <- shiny.i18n::Translator$new(
  translation_json_path = "translation.json"
)

# Text and image metadata
metadata <- readRDS(url(paste0(req$url, "/data/photo_metadata.rds")))
descriptions <- readRDS(url(paste0(req$url, "/data/descriptions.rds")))

# Get the app metadata from the DESCRIPTION file
METADATA <- yaml::yaml.load_file("DESCRIPTION")
VERSION <- METADATA[["Version"]]
REPO_URL <- METADATA[["URL"]]
LICENSE <- METADATA[["License"]]
AUTHOR <- METADATA[["Author"]]
AUTHOREMAIL <- METADATA[["AuthorEmail"]]

# Where should feedback be sent
FEEDBACK <- "helpdesk@laji.fi"

# FIXME: hard coded for now
DATA_URL <- "https://tun.fi/HR.2931"

# Define the size of the tiling window for data averaging
WINDOW_SIZE <- 5

# Which species is selected by default?
DEFAULT_SPECIES <- "CYGCYG"

# Highcharts options
## How many milliseconds in a year?
X_AXIS_TIME_UNITS = 30 * 24 * 3600 * 1000
# Year x-axis limits
XMIN <- datetime_to_timestamp(as.Date('2000-01-01', tz = 'UTC'))
XMAX <- datetime_to_timestamp(as.Date('2000-12-31', tz = 'UTC'))
# Year x-axis labels in languages different than English
X_YEARLY_LABELS <- list(
  "fi" = get_months("fi", "short"),
  "en" = get_months("en", "short"),
  "se" = get_months("se", "short")
)

# Color of the plotBands (background bars for months)
PB_COLOR <- "rgba(240, 240, 245, 0.4)"

# Define x-axis ranges for plotBands
PB_LIST <- list(
  list(from = get_timestamp("2000-02-01"), to = get_timestamp("2000-03-01"),
    color = PB_COLOR
  ),
  list(from = get_timestamp("2000-04-01"), to = get_timestamp("2000-05-01"),
    color = PB_COLOR
  ),
  list(from = get_timestamp("2000-06-01"), to = get_timestamp("2000-07-01"),
    color = PB_COLOR
  ),
  list(from = get_timestamp("2000-08-01"), to = get_timestamp("2000-09-01"),
    color = PB_COLOR
  ),
  list(from = get_timestamp("2000-10-01"), to = get_timestamp("2000-11-01"),
    color = PB_COLOR
  ),
  list(from = get_timestamp("2000-12-01"), to = get_timestamp("2000-12-31"),
    color = PB_COLOR
  )
)

CHOICES <- translator$get_languages()
names(CHOICES) <- purrr::map_chr(CHOICES, get_languages)

# This Javascript is needed for resizing the median day graph dynamically
# depeding on the size of the current viewport
jscode <- '
  $(document).on("shiny:connected", function(e) {
    var jsWidth = screen.width;
    Shiny.onInputChange("GetScreenWidth",jsWidth);
  });
'

# UI ---------------------------------------------------------------------------
ui <- function(request) {
  dashboardPage(

    title = "Haahka - muuttolintuselain",

    # ui-header ----------------------------------------------------------------
    dashboardHeader(
      title = tags$a(href = "https://haahka.laji.fi",
        tags$img(src = "browser_logo.png", height = "40")
      )
    ),
    # ui-sidebar ---------------------------------------------------------------
    dashboardSidebar(
      collapsed = FALSE,
      selectInput(
        "language",
        label = "Kieli / Språk / Language",
        choices = CHOICES
      ),
      sidebarMenuOutput("render_sidebarmenu"),
      hr(),
      uiOutput("render_sponsors"),
      uiOutput("render_sidebarfooter")
    ),
    # ui-body ------------------------------------------------------------------
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$script(
          defer = NA,
          `data-domain` = "haahka.laji.fi",
          src = "https://plausible.io/js/script.js"
        )
      ),
      # Species observations tab ---------------------------------------------
      tabItems(
        tabItem(
          tabName = "species",
          fluidPage(
            # Inject the JS bit
            tags$script(jscode),
            fluidRow(
              column(
                6,
                box(
                  width = 12,
                  uiOutput("render_species")
                ),
                box(
                  width = 12,
                  withSpinner(
                    uiOutput("render_description"),
                    type = 8,
                    size = 0.5
                  )
                )
              ),
              column(
                6,
                box(
                  width = 12,
                  withSpinner(
                    highchartOutput("migration", height = "300px"),
                    type = 8,
                    size = 0.5
                  ),
                  actionButton(
                    inputId = "migration_info",
                    label = NULL,
                    icon = icon("info", class = "icon-info"),
                    class = "btn-info btn-small-info"
                  )
                ),
                box(
                  width = 12,
                  withSpinner(
                    highchartOutput("local", height = "300px"),
                    type = 8,
                    size = 0.5
                  ),
                  actionButton(
                    inputId = "local_info",
                    label = NULL,
                    icon = icon("info", class = "icon-info"),
                    class = "btn-info btn-small-info"
                  )
                ),
                box(
                  width = 12,
                  withSpinner(
                    highchartOutput("change", height = "300px"),
                      type = 8,
                      size = 0.5
                    ),
                    actionButton(
                      inputId = "change_info",
                      label = NULL,
                      icon = icon("info", class = "icon-info"),
                      class = "btn-info btn-small-info"
                    )
                 ),
                 box(
                    width = 12,
                    uiOutput("render_median"),
                    actionButton(
                      inputId = "median_info",
                      label = NULL,
                      icon = icon("info", class = "icon-info"),
                      class = "btn-info btn-small-info"
                    )
                 ),
                 uiOutput("change_numbers"),
                 uiOutput("records")
              )
            )
          )
        ),
        # Help tab -------------------------------------------------------------
        tabItem(tabName = "help", uiOutput("render_helpsections"))
      )
    )
  )
}

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {

    # Bookmarking --------------------------------------------------------------

    # Bookmarking excludes
    setBookmarkExclude(c("bm1", "change_info", "GetScreenWidth",
                         "language-selectized", "local_info", "median_info",
                         "migration_info", "sidebarCollapsed",
                         "sidebarItemExpanded", "species-selectized"))

    # Unique session token
    session_token <- session$token
    log_info("Started new session {session_token}")

    # REACTIVES ----------------------------------------------------------------

    # i18n() -------------------------------------------------------------------
    i18n <- reactive({
        selected <- input$language

        if (length(selected) > 0 && selected %in% translator$get_languages()) {
            log_debug("Language changed to: {selected}")
            translator$set_translation_language(selected)
        }
        return(translator)
    })

    # get_current_sp -----------------------------------------------------------
    get_current_sp <- reactive({
        shiny::req(input$species)

        return(dplyr::filter(sp_data, Sci_name == input$species))
    })

    # get_current_data ---------------------------------------------------------
    get_current_data <- reactive({
        sp_current <- get_current_sp()
        return(tbl(con, paste0(sp_current$Species_Abb, "_data")))
    })

    # get_current_meta ---------------------------------------------------------
    get_current_meta <- reactive({
      sp_current <- get_current_sp()
      metadata[[sp_current$Species_Abb]]
    })

    # get_current_description --------------------------------------------------
    get_current_description <- reactive({
      sp_current <- get_current_sp()
      descriptions[[sp_current$Species_Abb]]
    })

    # get_current_records ------------------------------------------------------
    get_current_records <- reactive({
        sp_current <- get_current_sp()
        current_stats <- tbl(con, paste0(sp_current$Species_Abb, "_records"))
        return(current_stats)
    })

    # get_current_stats --------------------------------------------------------
    get_current_stats <- reactive({
        sp_current <- get_current_sp()
        current_stats <- tbl(con, paste0(sp_current$Species_Abb, "_stats"))
        return(current_stats)
    })

    # get_species_abbr ---------------------------------------------------------
    get_species_abbr <- reactive({
        current_sp <- get_current_sp()
        sp_abbr <- tolower(current_sp$Species_Abb)
        return(sp_abbr)
    })

    # HELPERS ------------------------------------------------------------------

    create_popup <- function(session, filebody, lang_suffix) {
      content_file <- file.path("www", "infos",
                                paste0(filebody, lang_suffix, ".md"))
      if (file.exists(content_file)) {
        sendSweetAlert(
          session = session,
          title = NULL,
          text = tagList(
            includeMarkdown(content_file)
          ),
          html = TRUE
        )
      }
    }

    get_species_names <- function(lang) {
        if (!is.null(lang)) {
            if (lang == "fi") {
                name_field <- "FIN_name"
            } else if (lang == "en") {
                name_field <- "ENG_name"
            } else if (lang == "se") {
                name_field <- "SWE_name"
          }

            sp_names <- sp_data %>%
                dplyr::select(!!name_field) %>%
                purrr::pluck(1)

            # Get the scientific names; these will be used in the labels and to
            # subset the data
            spps <- sp_data %>%
                dplyr::select(Sci_name) %>%
                purrr::pluck(1)

            sp_names <- paste0(sp_names, " (", spps, ")")

            # Create a named character vector
            names(spps) <- sp_names

            return(spps)
        }
    }

    # OUTPUTS ------------------------------------------------------------------

    # render_sidebarmenu -------------------------------------------------------
    output$render_sidebarmenu <- renderMenu({

        req(input$language)

        sidebarMenu(id = "tabs",
                    menuItem(i18n()$t("Lajikohtaiset havainnot"),
                             tabName = "species",
                             selected = TRUE,
                             icon = icon("binoculars")
                    ),
                    menuItem(i18n()$t("Ohjeet"),
                             tabName = "help",
                             icon = icon("question")
                    )
        )
    })

    # render_sponsors ----------------------------------------------------------
    output$render_sponsors <- renderMenu({

      req(input$language)

      payload <- tagList(
        div(class = "sponsors",
            i18n()$t("Haahkaa tukevat"),
            br(),
            a(href = "https://kordelin.fi/en/frontpage",
              img(src = "img/kordelin_logo_300_173.png")
            ),
            br(),
            a(href = "https://www.tringa.fi",
              img(src = "img/tringa_logo_300_300.png")
            ),
            br(),
            a(href = "https://laji.fi/",
              img(src = "https://cdn.laji.fi/images/logos/LAJI_FI_valk.png")
            ),
            br(),
            a(href = "https://luomus.fi/",
              img(src = "https://cdn.laji.fi/images/partners/luomus_fi_blue_smaller.gif")
            )
        )
      )
      return(payload)
    })

    # render_sidebarfooter -----------------------------------------------------
    output$render_sidebarfooter <- renderMenu({

        req(input$language)

        app_prefix <- i18n()$t("Sovellusversio")

        payload <- tagList(
            HTML("<footer>"),
            div(class = "footer-content",
                strong("Muuttolintuselain Haahka"),
                br(),
                paste0(app_prefix, ": ", VERSION),
                br(),
                i18n()$t("Palaute: "),
                a(href = paste0("mailto:", FEEDBACK), FEEDBACK),
                br(),
                br(),
                a(href = REPO_URL,
                  gsub("https://", "", REPO_URL)),
                br(),
                br(),
                "© 2018 ",
                a(href = paste0("mailto:", AUTHOREMAIL), AUTHOR),
                br(),
                br(),
                a(href = "https://opensource.org/licenses/MIT", "MIT"),
                paste0(" ", tolower(i18n()$t("Lisenssi")))
            ),
            HTML("</footer>")
        )
        return(payload)
    })

    # render_species -----------------------------------------------------------
    output$render_species <- renderUI({

        if (is.null(input$language)) {
            # By default, the names are Finnish
            name_field <- "fi"
        } else {
            name_field <- input$language
        }

        spps <- get_species_names(name_field)

        selected_sp <- sp_data %>%
          dplyr::filter(Species_Abb == DEFAULT_SPECIES) %>%
          dplyr::pull(Sci_name)
        selected_sp <- spps[which(spps == selected_sp)]

        payload <- div(id = "large",
                       selectInput("species",
                                   label = i18n()$t("Valitse laji listasta tai tyhjennä kenttä ja kirjoita lajinimi"),
                                   choices = spps,
                                   selected = selected_sp)
        )
        return(payload)
    })

    # render_citation ----------------------------------------------------------
    output$render_citation <- renderUI({

      req(input$language)

      current_sp <- get_current_sp()

      # Define metadata for the selected species
      now <- format(Sys.time(), format = "%Y-%m-%d")

      title <- i18n()$t("Viittausohje")
      text_fi <- glue::glue("{current_sp$FIN_name}. ",
                            "Helsingin Seudun Lintutieteellinen Yhdistys Tringa ry. Hangon lintuaseman aineisto: päiväsummat.",
                            "[{DATA_URL}] [Viitattu {now}]")
      text_se <- glue::glue("{current_sp$SWE_name}. ",
                            "Helsingforstraktens Ornitologiska Förening Tringa rf. Dataset från Hangö fågelstation: dagliga totalantal.",
                            "[{DATA_URL}] [Citerad {now}]")
      text_en <- glue::glue("{current_sp$ENG_name}. ",
                            "Ornithological society of Helsinki Tringa ry. Data of the Hanko Bird Observatory: Day counts.",
                            "[{DATA_URL}] [Cited {now}]")

      if (input$language == "fi") {
        payload <- tagList(
          h4(title),
          p(text_fi,
            br(),
            text_en)
        )
      } else if (input$language == "se") {
        payload <- tagList(
          h4(title),
          p(text_se,
            br(),
            text_en)
        )
      } else if (input$language == "en") {
        payload <- tagList(
          h4(title),
          p(text_en)
        )
      }

      return(tagList(div(payload, class = "description")))
    })

    # render_image -------------------------------------------------------------
    output$render_image <- renderUI({

        current_sp <- get_current_sp()
        current_meta <- get_current_meta()

        if (!is.null(current_sp)) {
            sp_abbr <- current_sp$Species_Abb

            # The actual file path is needed to figure out if the file exists
            # FIXME: does not work with multiple files!
            img_file <- list.files(file.path("www", "img", "sp_images"),
                                   pattern = paste0("(", sp_abbr, ")"),
                                   full.names = TRUE)

            if (length(img_file) > 0 && file.exists(img_file)) {
                # Photo credit
                caption <- current_meta$caption
                # Get file basename
                file_basename <- basename(img_file)
                # If the file does exist, use tags instead of rendering the image
                # directly. This way the browser will cache the image.
                payload <- shiny::div(shiny::img(src = glue::glue("img/sp_images/{file_basename}"),
                                                 width = "90%", class = "description"),
                                      shiny::p(shiny::HTML(caption),
                                               class = "description"),
                                      shiny::br())
            } else {
                payload <- shiny::p(i18n()$t("Kuvausteksti tulossa myöhemmin"), class = "description")
            }
            return(payload)
        }
    })

    # render_description -------------------------------------------------------
    output$render_description <- renderUI({

        current_sp <- get_current_sp()
        current_desc <- get_current_description()

        description <- parse_description(current_desc, input$language)

        if (!is.null(current_sp)) {
            # Define species names
            sp_abbr <- current_sp$Species_Abb
            sci_name <- current_sp$Sci_name

            if (input$language == "en") {
                common_name <- current_sp$ENG_name
            } else if (input$language == "fi") {
                common_name <- current_sp$FIN_name
            } else if (input$language == "se") {
              common_name <- current_sp$SWE_name
            }

            withTags(
                div(
                    shiny::h2(common_name, class = "description"),
                    shiny::h3(sci_name, class = "description sci-name"),
                    shiny::br(),
                    uiOutput("render_image"),
                    shiny::div(shiny::HTML(description), class = "description"),
                    shiny::br(),
                    uiOutput("render_citation")
                )
            )

        }
    })

    # migration ----------------------------------------------------------------
    output$migration <- renderHighchart({

        obs_current <- get_current_data()

        plot_data <- obs_current %>%
            dplyr::select(day, muutto) %>%
            collect() %>%
            mutate(day = as.Date(paste(2000, day), format = "%Y %j")) %>%
            as_tsibble(index = day) %>%
            tile_observations("day", "muutto", WINDOW_SIZE)

        if (!is.null(plot_data)) {

            # Update highcarts language options
            hcoptslang <- getOption("highcharter.lang")
            hcoptslang$shortMonths <- X_YEARLY_LABELS[[input$language]]
            options(highcharter.lang = hcoptslang)

            hc <- plot_data %>%
                hchart(type = "line",
                       hcaes(x = day, y = value_avgs),
                       name = i18n()$t("Muuttajamäärien keskiarvot"),
                       color = "#1f78b4") %>%
                hc_yAxis(title = list(text = i18n()$t("Yksilöä / havaintopäivä"))) %>%
                hc_xAxis(title = list(text = ""),
                         type = "datetime",
                         min = XMIN,
                         max = XMAX,
                         dateTimeLabelFormats = list(month = '%b'),
                         tickInterval = X_AXIS_TIME_UNITS,
                         plotBands = PB_LIST) %>%
                hc_plotOptions(line = list(marker = list(enabled = input$show_markers)),
                               spline = list(marker = list(enabled = input$show_markers))) %>%
                hc_title(text = i18n()$t("Muuttajamäärien keskiarvot")) %>%
                hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                           xDateFormat = "%b %d") %>%
                hc_exporting(enabled = TRUE) %>%
                hc_chart(zoomType = "xy")

            return(hc)
        }
    })

    # local --------------------------------------------------------------------
    output$local <- renderHighchart({

        obs_current <- get_current_data()

        plot_data <- obs_current %>%
            dplyr::select(day, paik) %>%
            collect() %>%
            mutate(day = as.Date(paste(2000, day), format = "%Y %j")) %>%
            as_tsibble(index = day) %>%
            tile_observations("day", "paik", WINDOW_SIZE)

        if (!is.null(plot_data)) {

            # Update highcarts language options
            hcoptslang <- getOption("highcharter.lang")
            hcoptslang$shortMonths <- X_YEARLY_LABELS[[input$language]]
            options(highcharter.lang = hcoptslang)

            hc <- plot_data %>%
                hchart(type = "line",
                       hcaes(x = day, y = value_avgs),
                       name = i18n()$t("Paikallisten määrien keskiarvot"),
                       color = "#1f78b4") %>%
                hc_yAxis(title = list(text = i18n()$t("Yksilöä / havaintopäivä"))) %>%
                hc_xAxis(title = list(text = ""),
                         type = "datetime",
                         min = XMIN,
                         max = XMAX,
                         dateTimeLabelFormats = list(month = '%b'),
                         tickInterval = X_AXIS_TIME_UNITS,
                         plotBands = PB_LIST) %>%
                hc_title(text = i18n()$t("Paikallisten määrien keskiarvot")) %>%
                hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                           xDateFormat = "%b %d") %>%
                hc_exporting(enabled = TRUE) %>%
                hc_chart(zoomType = "xy")

            return(hc)
        }
    })

    # change -------------------------------------------------------------------
    output$change <- renderHighchart({

        obs_current <- get_current_data()

        if (!is.null(obs_current)) {

            # Tile each variable
            plot_data_p1 <- obs_current %>%
                dplyr::select(day, totalp1) %>%
                collect() %>%
                mutate(day = as.Date(paste(2000, day), format = "%Y %j")) %>%
                as_tsibble(index = day) %>%
                tile_observations("day", "totalp1", WINDOW_SIZE) %>%
                dplyr::rename(totalp1 = value_avgs)

            plot_data_p2 <- obs_current %>%
                dplyr::select(day, totalp2) %>%
                collect() %>%
                mutate(day = as.Date(paste(2000, day), format = "%Y %j")) %>%
                as_tsibble(index = day) %>%
                tile_observations("day", "totalp2", WINDOW_SIZE) %>%
                dplyr::rename(totalp2 = value_avgs)

            plot_data_p3 <- obs_current %>%
                dplyr::select(day, totalp3) %>%
                collect() %>%
                mutate(day = as.Date(paste(2000, day), format = "%Y %j")) %>%
                as_tsibble(index = day) %>%
                tile_observations("day", "totalp3", WINDOW_SIZE) %>%
                dplyr::rename(totalp3 = value_avgs)

            plot_data_p4 <- obs_current %>%
              dplyr::select(day, totalp4) %>%
              collect() %>%
              mutate(day = as.Date(paste(2000, day), format = "%Y %j")) %>%
              as_tsibble(index = day) %>%
              tile_observations("day", "totalp4", WINDOW_SIZE) %>%
              dplyr::rename(totalp4 = value_avgs)

            plot_data <- plot_data_p1 %>%
                dplyr::left_join(., plot_data_p2, by = c("day" = "day")) %>%
                dplyr::left_join(., plot_data_p3, by = c("day" = "day")) %>%
                dplyr::left_join(., plot_data_p4, by = c("day" = "day")) %>%
                tidyr::gather(epoch, value, -day) %>%
                dplyr::mutate(
                  epoch = forcats::fct_relevel(
                    epoch, "totalp1", "totalp2", "totalp3", "totalp4"
                  )
                )

            # Update highcarts language options
            hcoptslang <- getOption("highcharter.lang")
            hcoptslang$shortMonths <- X_YEARLY_LABELS[[input$language]]
            options(highcharter.lang = hcoptslang)

            hc <- plot_data %>%
                hchart(type = "line",
                       hcaes(x = day, y = value, group = epoch),
                       # order of epochs c("begin", "end", "med")
                       name = c("1979-1999", "2000-2009", "2010-2019", "2020-"),
                       color = ggsci::pal_d3("category10")(4)) %>%
                hc_yAxis(title = list(text = i18n()$t("Yksilöä / havaintopäivä"))) %>%
                hc_xAxis(title = list(text = ""),
                         type = "datetime",
                         min = XMIN,
                         max = XMAX,
                         dateTimeLabelFormats = list(month = '%b'),
                         tickInterval = X_AXIS_TIME_UNITS,
                         plotBands = PB_LIST) %>%
                hc_plotOptions(line = list(marker = list(enabled = FALSE))) %>%
                hc_title(text = i18n()$t("Runsauksien muutokset")) %>%
                hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                           shared = TRUE, xDateFormat = "%b %d") %>%
                hc_exporting(enabled = TRUE) %>%
                hc_chart(zoomType = "xy")

            return(hc)
        }
    })

    # change_numbers -----------------------------------------------------------
    output$change_numbers <- renderUI({

        stats_current <- get_current_stats() %>% collect()

        # Sort out the the trend numbers and UI components
        # Long term
        if (!is.na(stats_current$slopeLong) & stats_current$slopeLong > 0) {
            lt_number_color <- "green"
            lt_number_icon <- icon("caret-up")
            lt_number <- paste0("+", stats_current$slopeLong, "%")
        } else if (!is.na(stats_current$slopeLong) & stats_current$slopeLong < 0) {
            lt_number_color <- "red"
            lt_number_icon <- icon("caret-down")
            lt_number <- paste0(stats_current$slopeLong, "%")
        } else if (is.na(stats_current$slopeLong) | stats_current$slopeLong == 0) {
            lt_number_color <- "gray"
            lt_number_icon <- icon(NULL)
            lt_number <- "-"
        }
        # Short term
        if (!is.na(stats_current$slopeShort) & stats_current$slopeShort > 0) {
            st_number_color <- "green"
            st_number_icon <- icon("caret-up")
            st_number <- paste0("+", stats_current$slopeShort, "%")
        } else if (!is.na(stats_current$slopeShort) & stats_current$slopeShort < 0) {
            st_number_color <- "red"
            st_number_icon <- icon("caret-down")
            st_number <- paste0(stats_current$slopeShort, "%")
        } else if (is.na(stats_current$slopeShort) | stats_current$slopeShort == 0) {
            st_number_color <- "gray"
            st_number_icon <-icon(NULL)
            st_number <- "-"
        }

        payload <- box(width = 12,
                       solidHeader = FALSE,
                       title = i18n()$t("Runsauden muutokset"),
                       background = NULL,
                       status = "danger",
                       footer = tagList(
                           p(
                               i18n()$t("Pitkän aikavälin muutos = keskirunsauden muutos aikajaksolta 1979-1999 aikajaksolle 2020-."),
                               br(),
                               i18n()$t("Lyhyen aikavälin muutos = keskirunsauden muutos aikajaksolta 2010-2019 aikajaksolle 2020-.")
                           )
                       ),
                       fluidRow(
                           column(
                               width = 6,
                               descriptionBlock(
                                   number = lt_number,
                                   numberColor = lt_number_color,
                                   numberIcon = lt_number_icon,
                                   header = "",
                                   text = paste(i18n()$t("Pitkän aikavälin muutos")),
                                   rightBorder = TRUE,
                                   marginBottom = TRUE
                               )
                           ),
                           column(
                               width = 6,
                               descriptionBlock(
                                   number = st_number,
                                   numberColor = st_number_color,
                                   numberIcon = st_number_icon,
                                   header = "",
                                   text = paste(i18n()$t("Lyhyen aikavälin muutos")),
                                   rightBorder = TRUE,
                                   marginBottom = TRUE
                               )
                           ),
                           column(width = 12,
                                  descriptionBlock(
                                      header = paste(i18n()$t("Päivittäiset keskirunsaudet yhteensä")),
                                      rightBorder = TRUE,
                                      marginBottom = FALSE
                                  )
                           ),
                           column(
                               width = 3,
                               descriptionBlock(
                                   header = format(round(stats_current$Np1, 0),
                                                   big.mark = " "),
                                   text = "1979-1999",
                                   rightBorder = TRUE,
                                   marginBottom = FALSE
                               )
                           ),
                           column(
                               width = 3,
                               descriptionBlock(
                                   header = format(round(stats_current$Np2, 0),
                                                   big.mark = " "),
                                   text = "2000-2009",
                                   rightBorder = FALSE,
                                   marginBottom = FALSE
                               )
                           ),
                           column(
                               width = 3,
                               descriptionBlock(
                                   header = format(round(stats_current$Np3, 0),
                                                   big.mark = " "),
                                   text = "2010-2019",
                                   rightBorder = FALSE,
                                   marginBottom = FALSE
                               )
                           ),
                           column(
                               width = 3,
                               descriptionBlock(
                                   header = format(round(stats_current$Np4, 0),
                                                   big.mark = " "),
                                   text = "2020-",
                                   rightBorder = FALSE,
                                   marginBottom = FALSE
                               )
                           )
                       )
        )
        return(payload)
    })

    # render_median ------------------------------------------------------------
    output$render_median <- renderUI({

      # Adjust height based on the initial screen size
      screen_size <- input$GetScreenWidth
      if (screen_size > 600) {
        height <- "200px"
      } else {
        height <- NULL
      }

      payload <- withSpinner(highchartOutput("migration_medians",
                                             height = height),
                             type = 8, size = 0.5)
      return(payload)
    })

    # migration_medians --------------------------------------------------------
    output$migration_medians <- renderHighchart({

        sp_current <- get_current_sp()

        # Define origing date
        origin <- as.Date("2000-01-01")

        # abundance_stats has already been loaded
        plot_data <- get_current_stats() %>%
            collect() %>%
            # Select only median values (Julian days) for i) the three epochs
            # and ii) spring and autumn
            select(sphenp1, sphenp2, sphenp3, sphenp4, aphenp1, aphenp2, aphenp3, aphenp4) %>%
            # Make data tidy (long)
            gather(variable, value) %>%
            # Split variables into two new columns
            separate(col = "variable", into = c("season", "epoch"), sep = "phen") %>%
            # Mutate new variables
            mutate(
                   # Replace "s" and "a" with more informative strings
                   season = ifelse(season == "s", tolower(i18n()$t("Kevät")),
                                   ifelse(season == "a", tolower(i18n()$t("Syys")), NA)),
                   # Make epochs factors
                   epoch = factor(epoch, levels = c("p1", "p2", "p3", "p4"),
                                  labels = rev(c("1979-1999", "2000-2009", "2010-2019", "2020-")),
                                  ordered = TRUE),
                   # Numeric value of the factors is needed so that highcharts
                   # can plot the factors on y-axis. Note that Javascript
                   # indexing starts from 0.
                   epochnum = as.numeric(epoch) - 1,
                   # Convert Julian days into actual dates
                   date = origin + value,
                   # Pretty version of the date for tooltips
                   date_print = make_date_label(date, input$language))

        # Update highcarts language options
        hcoptslang <- getOption("highcharter.lang")
        hcoptslang$shortMonths <- X_YEARLY_LABELS[[input$language]]
        options(highcharter.lang = hcoptslang)

        hc <- plot_data %>%
            hchart(type = "scatter",
                   hcaes(x = date, y = epochnum, group = epoch),
                   # order of epochs c("begin", "end", "med")
                   name = c("1979-1999", "2000-2009", "2010-2019", "2020-"),
                   color = ggsci::pal_d3("category10")(4)) %>%
            hc_yAxis(title = list(text = ""),
                     min = 0,
                     max = 3,
                     categories = rev(levels(plot_data$epoch))) %>%
            hc_xAxis(title = list(text = ""),
                     type = "datetime",
                     min = XMIN,
                     max = XMAX,
                     dateTimeLabelFormats = list(month = '%b'),
                     tickInterval = X_AXIS_TIME_UNITS,
                     plotBands = PB_LIST) %>%
            hc_plotOptions(
                scatter = list(marker = list(symbol = "circle",
                                             radius = 8))
            ) %>%
            hc_title(text = i18n()$t("Muuton ajoittumisen mediaanipäivämäärä")) %>%
            hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                       shared = TRUE, xDateFormat = "%b %d",
                       pointFormat = paste0("{point.season}",
                                            # Not pretty, but needed for
                                            # compound words
                                            ifelse(input$language == "fi", "", " "),
                                            tolower(i18n()$t("Muuton ajoittumisen mediaanipäivämäärä")),
                                            ":", "<br> {point.date_print}")) %>%
            hc_exporting(enabled = TRUE) %>%
            hc_chart(zoomType = "xy")

        return(hc)
    })

    # records ------------------------------------------------------------------
    output$records <- renderUI({

        records_current <- get_current_records() %>%
            dplyr::collect() %>%
            dplyr::mutate(date = as.Date(paste(year, day), "%Y %j"))

        if (nrow(records_current) == 0) {
            payload <- NULL
        } else {

            get_value <- function(season, type, value) {

              season <- switch(season, Spring = "s", Autumn = "a")
              type <- switch(type, Migr = "m", Local = "l")

              record_value <- paste0(season, type)
              idx <- paste0(record_value, "_")
              value <- switch(
                value, date_string = "date", Sum = record_value
              )

                res <- records_current %>%
                    dplyr::filter(as.logical(.data[[idx]])) %>%
                    dplyr::pull(.data[[value]])


                if (is.numeric(res)) {
                    res <- format(res, big.mark = " ")[[1L]]
                } else {
                  res <- paste(res, collapse = i18n()$t(" ja "))
                }

                if (length(res) == 0) {
                    res <- "-"
                }
                return(res)
            }

            payload <- box(width = 12,
                           solidHeader = FALSE,
                           title = i18n()$t("Runsausennätykset"),
                           status = "info",
                           fluidRow(
                               column(
                                   width = 12,
                                   tagList(
                                       h4(i18n()$t("Kevät"), class = "record")
                                   ),
                                   column(
                                       width = 6,
                                       tagList(
                                           h5(i18n()$t("Muuttavat"), class = "record"),
                                           div(class = "record",
                                               column(width = 3,
                                                      icon("trophy",
                                                           class = "icon-record icon-gold")
                                               ),
                                               column(width = 9,
                                                      p(get_value("Spring", "Migr", "Sum"),
                                                        class = "record-number")
                                               ),
                                               column(width = 3,
                                                      icon("calendar", class = "icon-record")
                                               ),
                                               column(width = 9,
                                                      p(get_value("Spring", "Migr", "date_string"))
                                               )
                                           )
                                       )
                                   ),
                                   column(
                                       width = 6,
                                       tagList(
                                           h5(i18n()$t("Paikalliset"), class = "record"),
                                           div(class = "record",
                                               column(width = 3,
                                                      icon("trophy",
                                                           class = "icon-record icon-gold")
                                               ),
                                               column(width = 9,
                                                      p(get_value("Spring", "Local", "Sum"),
                                                        class = "record-number")
                                               ),
                                               column(width = 3,
                                                      icon("calendar", class = "icon-record")
                                               ),
                                               column(width = 9,
                                                      p(get_value("Spring", "Local", "date_string"))
                                               )
                                           )
                                       )
                                   )
                               )
                           ),
                           fluidRow(
                               column(
                                   width = 12,
                                   tagList(
                                       h4(i18n()$t("Syksy"), class = "record")
                                   ),
                                   column(
                                       width = 6,
                                       tagList(
                                           h5(i18n()$t("Muuttavat"), class = "record"),
                                           div(class = "record",
                                               column(width = 3,
                                                      icon("trophy",
                                                           class = "icon-record icon-gold")
                                               ),
                                               column(width = 9,
                                                      p(get_value("Autumn", "Migr", "Sum"),
                                                        class = "record-number")
                                               ),
                                               column(width = 3,
                                                      icon("calendar", class = "icon-record")
                                               ),
                                               column(width = 9,
                                                      p(get_value("Autumn", "Migr", "date_string")
                                                        )
                                               )
                                           )
                                       )
                                   ),
                                   column(
                                       width = 6,
                                       tagList(
                                           h5(i18n()$t("Paikalliset"), class = "record"),
                                           div(class = "record",
                                               column(width = 3,
                                                      icon("trophy",
                                                           class = "icon-record icon-gold")
                                               ),
                                               column(width = 9,
                                                      p(get_value("Autumn", "Local", "Sum"),
                                                        class = "record-number")
                                               ),
                                               column(width = 3,
                                                      icon("calendar", class = "icon-record")
                                               ),
                                               column(width = 9,
                                                      p(get_value("Autumn", "Local", "date_string"))
                                               )
                                           )
                                       )
                                   )
                               )
                           )
            )
        }
        return(payload)
    })

    # render_helpsections ------------------------------------------------------
    output$render_helpsections <- renderUI({

        data_help_file <- file.path("www", "helps",
                                    paste0("data_help-", input$language, ".md"))
        if (file.exists(data_help_file)) {
          data_help_content <- tagList(
                div(class = "help-container",
                    includeMarkdown(data_help_file)
              )
          )
        } else {
          data_help_content <- ""
        }

        app_help_file <- file.path("www", "helps",
                                    paste0("app_help-", input$language, ".md"))
        if (file.exists(app_help_file)) {
          app_help_content <- tagList(
            div(class = "help-container",
                includeMarkdown(app_help_file)
            )
          )
        } else {
          app_help_content <- ""
        }

        payload <- tagList(
          fluidRow(
            column(width = 6,
                   userBox(
                     title = userDescription(
                       title = i18n()$t("Aineisto"),
                       subtitle = i18n()$t("Hangon lintuaseman pitkäaikaisaineisto"),
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
            column(width = 6,
                   userBox(
                     title = userDescription(
                       title = i18n()$t("Verkkosovellus"),
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
      return(payload)
    })

    # OBSERVERS ----------------------------------------------------------------

    observe({
      # Trigger this observer every time an input changes
      reactiveValuesToList(input)
      session$doBookmark()
    })

    # Update URL on each bookmarking
    onBookmarked(function(url) {
      updateQueryString(url)
    })

    # Used only for logging
    observeEvent(input$species, {
        logger::log_debug("Species changed to: {input$species}")
    })

    observeEvent(input$language, {
      updateSelectInput(session, "language", selected = input$language)
    })

    observeEvent(i18n(), {
        updateSelectInput(session, "species", label =  i18n()$t("Valitse laji"),
                          choices = get_species_names(input$language),
                          selected = input$species)
    })

    observeEvent(input$migration_info, {
      req(input$language)
      create_popup(session, "migration_info-", input$language)
    })

    observeEvent(input$local_info, {
      req(input$language)
      create_popup(session, "local_info-", input$language)
    })

    observeEvent(input$change_info, {
      req(input$language)
      create_popup(session, "change_info-", input$language)
    })

    observeEvent(input$median_info, {
      req(input$language)
      create_popup(session, "median_info-", input$language)
    })

    session$onSessionEnded(function() {
      logger::log_info("Session {session_token} stopped")
    })
}

shinyApp(ui, server, enableBookmarking = "url")
