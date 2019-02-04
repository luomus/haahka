library(assertthat)
library(ggsci)
library(glue)
library(highcharter)
library(lubridate)
library(markdown)
library(officer)
library(shiny)
library(shiny.i18n)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(tidyverse)
library(tsibble)
library(yaml)

# Helper functions --------------------------------------------------------

# Return full language names instead of a language code.
#
get_languages <- function(x) {
    
    langs <- list("fi" = "Suomi",
                  "en" = "English",
                  "se" = "Svenska")
    
    assertthat::assert_that(x %in% names(langs))
    
    return(langs[[x]])
}

# Get month names in a given language
# 
get_months <- function(lang = "en", format) {
    
    supported_languages <- c("en", "fi", "se")
    
    assertthat::assert_that(lang %in% supported_languages,
                            msg = paste0("lang must be one of: ", 
                                         paste(supported_languages, 
                                               collapse = ", ")))
    assertthat::assert_that(format %in% c("long", "short"),
                            msg = "format must be either long or short")
    
    month_names <- list(
        "short" = list(
            "en" = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                     "Oct", "Nov", "Dec"),
            "fi" = c("Tammi", "Helmi", "Maalis", "Huhti", "Touko", "Kesä",
                     "Heinä", "Elo", "Syys", "Loka", "Marras", "Joulu"),
            "se" = c("Jan", "Feb", "Mar", "Apr", "Maj", "Jun", "Jul", "Aug", "Sep",
                     "Okt", "Nov", "Dec")
        ),
        "long" = list(
            "en" = c("January", "February", "March", "April", "May", "June", 
                     "July", "August", "September", "October", "November", 
                     "December"),
            "fi" = c("Tammikuu", "Helmikuu", "Maaliskuu", "Huhtikuu", 
                     "Toukokuu", "Kesäkuu", "Heinäkuu", "Elokuu", "Syyskuu", 
                     "Lokakuu", "Marraskuu", "Joulukuu"),
            "se" = c("Januari", "Februari", "Mars", "April", "Maj", "Juni", 
                     "Juli", "Augusti", "September", "Oktober", "November", 
                     "December")
        )
        
    )
    return(month_names[[format]][[lang]])
}

# Return numeric timestamp value based on a Date. Needed for highcharts 
# x-axis settings and plotting.
# 
get_timestamp <- function(x) {
    return(datetime_to_timestamp(as.Date(x, tz = "UTC")))
}

# Check that a value is odd numeric
#
is_odd <- function(x) {
    assertthat::assert_that(is.numeric(x), length(x) == 1)
    x %% 2 == 1
}

# Make a date label for tooltips with language support
# 
make_date_label <- function(x, lang) {
    
    assertthat::see_if(is(x, "Date"))
    
    # Get the name of month in a given language
     month_name <- get_months(lang, "long")[lubridate::month(x)]
     label <- glue::glue("{month_name} {day(x)}")
     return(label)
}

# Parse author
#
# Convert normal name to citation format, e.g. "Aki Aintila" to "Aintila, A"
# FIXME: won't work with names other than "FIRSTNAME LASTNAME"
#
parse_author <- function(x) {
  tokens <- unlist(strsplit(x, " "))
  initial <- paste0(substring(tokens[1], 1, 1), ".")
  return(paste(c(tokens[2], initial), collapse = ", "))
}


# Parse description file
# 
parse_description <- function(style_name, text) {
    if (is.na(style_name) || is.na(text)) {
      return(invisible(NULL))
    }
    style_name <- tolower(style_name)
  
    # NOTE: style is hard coded and needs to be adjusted if the style changes
    # Check both the element style and content
    if (style_name == "no spacing" & text != "") {
        citation_pattern <- "\\((.+?)\\)"
      
        if (grepl(citation_pattern, text)) {
          citations <- unlist(stringr::str_extract_all(text, citation_pattern))
          for (citation in citations) {
            i_citation <- gsub("\\(", "<i>", citation)
            i_citation <- gsub("\\)", "</i>", i_citation)
            text <- gsub(citation, i_citation, text)
          }
          # Contsruct valid HTML
          text <- paste0("<p class='endnote'>", text, "</p>")
          element <- shiny::HTML(text)
        } else {
          element <- shiny::p(text, class = "description")  
        }
        
    } else if (style_name == "endnote text" || style_name == "list paragraph" && text != "") {
        # Make hyperlinks acutal hyperlinks
        if (grepl("http", text)) {
          # Get rid of the unneed prefix/suffix characters
          text <- gsub("(<|>)", "", text)
          url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
          url <- stringr::str_extract(text, url_pattern)
          # Contsruct valid HTML
          text <- gsub(url, paste0("<a href='", url, "'>", url, "</a>"), text)
          text <- paste0("<p class='endnote'>", text, "</p>")
          element <- shiny::HTML(text)
        } else {
          element <- shiny::p(text, class = "endnote")
        }
        
    } else if (style_name == "heading 3" & text != "") {
        element <- shiny::h4(text, class = "description")
    } else {
        element <- invisible(NULL)
    }
    return(element)
}

# Capitalize the first letter of a sentence.
# 
simple_cap <- Vectorize(
    function(x) {
        s <- strsplit(x, " ")[[1]]
        s <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
                   sep = "", collapse = " ")
        return(s)
    }, SIMPLIFY = TRUE, USE.NAMES = FALSE)

# Calculate a tiled average (non-overlapping windows) for yearly observations. 
# 
# An average is calculated over a specified window and assigned to the day in
# the middle of the windows. For example, if size is 5, then the average is 
# assigned to day at index 3 etc. 
# 
# 
# @param x a tsibble
# @parame day string name of the variable containing datetime values
# @param value string name of the variable containing numeric observation values
# @param size numeric size of the window over which average is calculated. Must
#             be an odd value.
# 
# @return a tibble with dates and corresponding (rounded) averages.
# 
tile_observations <- function(x, day, value, size) {
    
    # Coerce size to integer
    size <- as.integer(size)
    
    # x must be a tsibble
    assertthat::see_if(is(x) == "tbl_ts")
    
    # size must be an odd integer
    assertthat::on_failure(is_odd) <- function(call, env) {
        paste0(deparse(call$x), " is even. size must be odd.")
    }
    
    assertthat::assert_that(is_odd(size))
    
    # If size is 1, no need to calculate anything
    if (size == 1) {
        days <- x[[day]]
        avgs <- x[[value]]
    } else {
        # NOTE: the number of days is hard coded here and include leap day
        n_days <- 366
        
        # Construct a day index, i.e. index vector defining which days are to be
        # kept. Staring point is the middle (median) value of the window.
        day_index <- seq(median(1:size), nrow(x), by = size)
        
        # Find the remainder given the size
        remainder <- n_days %% size
        # If there is a remainder, the last tile is not the same size as the others
        # and the index vector needs to be augmented. Simple add 366 as the
        # last value in the index vector.
        if (remainder != 0) {
            day_index <- c(day_index, n_days)
        }
        
        # Get correct days based on the index vector
        days <- x[[day]][day_index]
        # Calculate the averages
        avgs <- tsibble::tile_dbl(x[[value]], ~ mean(., na.rm = TRUE), 
                                  .size = size)
        avgs <- round(avgs, 2)
    }
    
    return(tibble::tibble(day = days, value_avgs = avgs))
}

# Load data ---------------------------------------------------------------

load("data/sp_yearly_1_2.RData")

# FIXME: distinct shoulnd't be needed
# FIXME: perhaps move all pre-processing to halias-observations
dat <- dat %>% 
    # Remove duplicate rows
    dplyr::distinct() %>% 
    # Round all numeric columns (numbers of observed individuals)
    dplyr::mutate_if(is.numeric, round, digits = 2)
    
# Read species definition data
sp_data <- readr::read_csv("data/Halias_sp_v1.2.csv") %>% 
    dplyr::arrange(Species_code) %>% 
    dplyr::mutate(FIN_name = simple_cap(FIN_name),
                  SWE_name = simple_cap(SWE_name))

# Read pre-processed abundance and phenology stats
abundance_stats <- readr::read_csv("data/Halias_trend20181230.csv") %>% 
    dplyr::select(-X1) %>% 
    dplyr::mutate(slopeShort = ifelse(is.infinite(slopeShort), NA, slopeShort))

# Read pre-processed record stats
record_stats <- readr::read_csv("data/Halias_record20181230.csv") %>% 
    dplyr::select(-X1)

# Translation data
translator <- shiny.i18n::Translator$new(translation_json_path = "data/translation.json")

# Text and image metadata
metadata <- readr::read_csv("data/text_and_image_reference.csv") %>% 
  # Rename Kuvauspaikka
  dplyr::rename(Kuvauspaikka = dplyr::starts_with("Kuvauspaikka")) %>% 
  # If kuvauspaikka is NA, the place is Halias
  dplyr::mutate(Kuvauspaikka = ifelse(is.na(Kuvauspaikka), 
                                      "Halias", Kuvauspaikka))
  
# Global variables --------------------------------------------------------

# Get the app metadata from the DESCRIPTION file
METADATA <- yaml::yaml.load_file("DESCRIPTION")
VERSION <- METADATA[["Version"]]
REPO_URL <- METADATA[["URL"]]
LICENSE <- METADATA[["License"]]
AUTHOR <- METADATA[["Author"]]
AUTHOREMAIL <- METADATA[["AuthorEmail"]]

# Where should feedback be sent
FEEDBACK <- "halias@halias.fi"

# FIXME: hard coded for now
DATA_VERSION <- 1.1
DATA_URL <- "https://www.tringa.fi/hangon-lintuasema/hankodata/"

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

# Globals needed to track various states
INTENDED_LANGUAGE <- "fi"
INTENDED_SPECIES <- DEFAULT_SPECIES
REQUEST_FROM_URL <- FALSE
ALREADY_RENDERED <- FALSE

# This Javascript is needed for resizing the median day graph dynamically 
# depeding on the size of the current viewport
jscode <-
  '$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
  Shiny.onInputChange("GetScreenWidth",jsWidth);
});
'

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
    
    title = "Haahka - muuttolintuselain",
    
    # ui-header ----------------------------------------------------------------
    dashboardHeader(
        title = tags$a(href = "https://www.halias.fi",
                       tags$img(src = "browser_logo.png", height = "40")
        )
    ),
    # ui-sidebar ---------------------------------------------------------------
    dashboardSidebar(collapsed = FALSE,
                     uiOutput("render_language"),
                     hr(),
                     sidebarMenuOutput("sidebarmenu"),
                     uiOutput("render_sidebarfooter")
    ),
    # ui-body ------------------------------------------------------------------
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            includeScript("google-analytics.js")
        ),

        # Species observations tab ---------------------------------------------
        tabItems(
            tabItem(tabName = "species",
            fluidPage(
                # Inject the JS bit
                tags$script(jscode),
                fluidRow(
                    column(6,
                           box(width = 12,
                               uiOutput("render_species")
                           ),
                           box(
                             width = 12,
                             withSpinner(uiOutput("render_description"), type = 8, 
                                         size = 0.5)
                           )
                    ),
                    column(6,
                           box(
                               width = 12,
                               withSpinner(highchartOutput("migration", height = "300px"),
                                           type = 8, size = 0.5),
                               actionButton(
                                 inputId = "migration_info",
                                 label = NULL,
                                 icon = icon("info-circle",
                                             class = "icon-info"),
                                 class = "btn-info btn-small-info"
                               )
                               
                           ),
                           box(
                               width = 12,
                               withSpinner(highchartOutput("local", height = "300px"),
                                           type = 8, size = 0.5),
                               actionButton(
                                 inputId = "local_info",
                                 label = NULL,
                                 icon = icon("info-circle",
                                             class = "icon-info"),
                                 class = "btn-info btn-small-info"
                               )
                           ),
                           box(
                               width = 12,
                               withSpinner(highchartOutput("change", height = "300px"),
                                           type = 8, size = 0.5),
                               actionButton(
                                 inputId = "change_info",
                                 label = NULL,
                                 icon = icon("info-circle",
                                             class = "icon-info"),
                                 class = "btn-info btn-small-info"
                               )
                           ),
                           box(
                               width = 12,
                               uiOutput("render_median"),
                               actionButton(
                                 inputId = "median_info",
                                 label = NULL,
                                 icon = icon("info-circle",
                                             class = "icon-info"),
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
        tabItem(tabName = "help",
                uiOutput("render_helpsections")
        )
      )
    )
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
    
    # REACTIVES ----------------------------------------------------------------
    
    # i18n() -------------------------------------------------------------------
    i18n <- reactive({
        selected <- input$language
        
        if (length(selected) > 0 && selected %in% translator$languages) {
            message("Language changed to: ", selected)
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
        return(dplyr::filter(dat, sp == sp_current$Species_Abb) )
    })
    
    # get_current_meta ---------------------------------------------------------
    get_current_meta <- reactive({
      sp_current <- get_current_sp()
      current_meta <-  metadata %>% 
        dplyr::filter(Lajilyhenne == sp_current$Species_Abb)
      return(current_meta)
    })
    
    # get_current_records ------------------------------------------------------
    get_current_records <- reactive({
        sp_current <- get_current_sp()
        current_stats <- record_stats %>% 
            dplyr::filter(sp == sp_current$Species_Abb)
        return(current_stats)
    })
    
    # get_current_stats --------------------------------------------------------
    get_current_stats <- reactive({
        sp_current <- get_current_sp()
        current_stats <- abundance_stats %>% 
            dplyr::filter(sp == sp_current$Species_Abb)
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
                dplyr::filter(Sp == 1) %>% 
                dplyr::select(!!name_field) %>% 
                purrr::pluck(1)
            
            # Get also the scientific names; these will be used in the labels and
            # to subset the data
            spps <- sp_data %>% 
                dplyr::filter(Sp == 1) %>% 
                dplyr::select(Sci_name) %>% 
                purrr::pluck(1)
            
            sp_names <- paste0(sp_names, " (", spps, ")")
            
            # Create a named character vector
            names(spps) <- sp_names
            
            return(spps)
        }
    }
    
    # OUTPUTS ------------------------------------------------------------------
    
    # render_lanugage ----------------------------------------------------------
    output$render_language <- renderUI({
        
        choices <- translator$languages
        names(choices) <- purrr::map_chr(choices, get_languages)
        
        payload <- selectInput("language",
                               label = i18n()$t("Kieli"),
                               choices = choices, 
                               selected = input$language)
        return(payload)
    })
    
    # render_sidebarmenu -------------------------------------------------------
    output$sidebarmenu <- renderMenu({
      
        req(input$language)
        
        sidebarMenu(id = "tabs", 
                    menuItem(i18n()$t("Lajikohtaiset havainnot"), 
                             tabName = "species", 
                             selected = TRUE,
                             icon = icon("binoculars")
                    ),
                    menuItem(i18n()$t("Ohjeet"), 
                             tabName = "help", 
                             icon = icon("question-circle")
                    )
        )
    })
    
    # render_sidebarfooter -----------------------------------------------------
    output$render_sidebarfooter <- renderMenu({
        
        req(input$language)
        
        app_prefix <- i18n()$t("Sovellusversio")
        data_prefix <- i18n()$t("Aineistoversio")
        
        payload <- tagList(
            HTML("<footer>"),
            div(class = "footer-content",
                strong("Muuttolintuselain Haahka"),
                br(),
                paste0(app_prefix, ": ", VERSION),
                br(),
                paste0(data_prefix, ": ", DATA_VERSION),
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
                a(href = "https://opensource.org/licenses/MIT", "MIT"),
                paste0(" ", tolower(i18n()$t("Lisenssi")))
            ),
            HTML("</footer>")
        )
        return(payload)
    })
    
    # render_species ----------------------------------------------------------
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
      
      current_meta <- get_current_meta()
      current_sp <- get_current_sp()
      
      # Define metadata for the selected species
      author <- parse_author(current_meta$Kirjoittaja)
      year <- current_meta$Vuosi
      data_version <- current_meta$Aineistoversio
      now <- format(Sys.time(), format = "%Y-%m-%d")
      
      title <- i18n()$t("Viittausohje")
      text_fi <- glue::glue("{author} {year}: {current_sp$FIN_name}. ",
                            "Julkaisussa: Hangon lintuasema: Asemalla havaittujen lintulajien esiintyminen. ",
                            "Versio {data_version} [{DATA_URL}] [Viitattu {now}]")
      text_se <- glue::glue("{author} {year}: {current_sp$SWE_name}. ",
                            "I: Hangö fågelstation: Förekomst av arter vid fågelstationen.",
                            "Version {data_version} [{DATA_URL}] [Citerad {now}]")
      text_en <- glue::glue("{author} {year}: {current_sp$ENG_name}. ",
                            "In: Hanko Bird Observatory: Occurrence of species at the observatory. ",
                            "Version {data_version} [{DATA_URL}] [Cited {now}]")
            
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
                                   pattern = paste0("[0-9]{3}(-|_)(", sp_abbr, ")"),
                                   full.names = TRUE)
            
            if (length(img_file) > 0 && file.exists(img_file)) {
                # Photo credit
                photo_credit <- current_meta$Kuvaaja
                photo_date <- current_meta$Päivämäärä
                photo_date <- ifelse(is.na(photo_date), "", photo_date)
                photo_place <- current_meta$Kuvauspaikka
                if (photo_date == "") {
                  photo_date_place <- paste0("(", photo_place, ")")
                } else {
                  photo_date_place <- paste0("(", 
                                             paste0(c(photo_date, photo_place),
                                                    collapse = ", "), ")")
                }
                # Get file basename
                file_basename <- basename(img_file)
                # If the file does exist, use tags instead of rendering the image
                # directly. This way the browser will cache the image.
                payload <- shiny::div(shiny::img(src = glue::glue("img/sp_images/{file_basename}"),
                                                 width = "90%", class = "description"),
                                      shiny::p(glue::glue("Ⓒ {photo_credit} {photo_date_place}"), 
                                               class = "description"),
                                      shiny::br())
            } else {
                payload <- shiny::p(i18n()$t("Kuvausteksti tulee kevään aikana"), class = "description")
            }
            return(payload)
        }        
    })
    
    # render_description -------------------------------------------------------
    output$render_description <- renderUI({
        
        current_sp <- get_current_sp()
        current_meta <- get_current_meta()
        
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
            
            # Try reading the description docx file
            docx_file <- list.files(file.path("data", "descriptions"),
                                    pattern = paste0("[0-9]{3}-(", sp_abbr, ")"),
                                    full.names = TRUE)
            
            if (length(docx_file) > 0 && file.exists(docx_file) && input$language == "fi") {
                
                docx_content <- officer::docx_summary(officer::read_docx(docx_file))

                payload <- withTags(
                    div(
                        shiny::h2(common_name, class = "description"),
                        shiny::h3(sci_name, class = "description sci-name"),
                        shiny::br(),
                        uiOutput("render_image"),
                        docx_content %>% 
                            dplyr::rowwise() %>% 
                            do(row = parse_description(.$style_name, .$text)) %>% 
                            as.list(),
                        br(),
                        uiOutput("render_citation")
                    )
                )
            } else {
                payload <- withTags(
                    shiny::div(
                        shiny::h2(common_name, class = "description"),
                        shiny::h3(sci_name, class = "description sci-name"),
                        shiny::br(),
                        uiOutput("render_image"),
                        shiny::p(i18n()$t("Kuvausteksti tulee kevään aikana"), class = "description")
                    )
                )
            }
            
            return(payload)            
        }
    })
    
    # migration ----------------------------------------------------------------
    output$migration <- renderHighchart({
        
        obs_current <- get_current_data()
        
        plot_data <- obs_current %>% 
            dplyr::select(sp, day, muutto) %>% 
            as_tsibble(key = id(sp), index = day) %>% 
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
            dplyr::select(sp, day, paik) %>% 
            as_tsibble(key = id(sp), index = day) %>% 
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
        
            epochs <- obs_current %>% 
                dplyr::select(day, begin, med, end)
            
            # Tile each variable
            plot_data_begin <- obs_current %>% 
                dplyr::select(sp, day, begin) %>% 
                as_tsibble(key = id(sp), index = day) %>% 
                tile_observations("day", "begin", WINDOW_SIZE) %>% 
                dplyr::rename(begin = value_avgs)
            
            plot_data_med <- obs_current %>% 
                dplyr::select(sp, day, med) %>% 
                as_tsibble(key = id(sp), index = day) %>% 
                tile_observations("day", "med", WINDOW_SIZE) %>% 
                dplyr::rename(med = value_avgs)
            
            plot_data_end <- obs_current %>% 
                dplyr::select(sp, day, end) %>% 
                as_tsibble(key = id(sp), index = day) %>% 
                tile_observations("day", "end", WINDOW_SIZE) %>% 
                dplyr::rename(end = value_avgs)
            
            plot_data <- plot_data_begin %>% 
                dplyr::left_join(., plot_data_med, by = c("day" = "day")) %>% 
                dplyr::left_join(., plot_data_end, by = c("day" = "day")) %>%
                tidyr::gather(epoch, value, -day) %>% 
                dplyr::mutate(epoch = forcats::fct_relevel(epoch, "begin", "med", "end"))
            
            # Update highcarts language options
            hcoptslang <- getOption("highcharter.lang")
            hcoptslang$shortMonths <- X_YEARLY_LABELS[[input$language]]
            options(highcharter.lang = hcoptslang)
            
            hc <- plot_data %>% 
                hchart(type = "line", 
                       hcaes(x = day, y = value, group = epoch),
                       # order of epochs c("begin", "end", "med")
                       name = c("1979-1999", "2000-2010", "2011-2017"),
                       color = ggsci::pal_d3("category10")(3)) %>% 
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
        
        stats_current <- get_current_stats()
        
        # Sort out the the trend numbers and UI components
        # Long term
        if (!is.na(stats_current$slopeLong) & stats_current$slopeLong > 0) {
            lt_number_color <- "green"
            lt_number_icon <- "fa fa-caret-up"
            lt_number <- paste0("+", stats_current$slopeLong, "%")
        } else if (!is.na(stats_current$slopeLong) & stats_current$slopeLong < 0) {
            lt_number_color <- "red"
            lt_number_icon <- "fa fa-caret-down"
            lt_number <- paste0(stats_current$slopeLong, "%")
        } else if (is.na(stats_current$slopeLong) | stats_current$slopeLong == 0) {
            lt_number_color <- "grey"
            lt_number_icon <- NA
            lt_number <- "-"
        }
        # Short term
        if (!is.na(stats_current$slopeShort) & stats_current$slopeShort > 0) {
            st_number_color <- "green"
            st_number_icon <- "fa fa-caret-up"
            st_number <- paste0("+", stats_current$slopeShort, "%")
        } else if (!is.na(stats_current$slopeShort) & stats_current$slopeShort < 0) {
            st_number_color <- "red"
            st_number_icon <- "fa fa-caret-down"
            st_number <- paste0(stats_current$slopeShort, "%")
        } else if (is.na(stats_current$slopeShort) | stats_current$slopeShort == 0) {
            st_number_color <- "grey"
            st_number_icon <- NA
            st_number <- "-"
        }
        
        payload <- box(width = 12,
                       solidHeader = FALSE,
                       title = i18n()$t("Runsauden muutokset"),
                       background = NULL,
                       status = "danger",
                       footer = tagList(
                           p(
                               i18n()$t("Pitkän aikavälin muutos = keskirunsauden muutos aikajaksolta 1979-1999 aikajaksolle 2011-2017."),
                               br(),
                               i18n()$t("Lyhyen aikavälin muutos = keskirunsauden muutos aikajaksolta 2000-2010 aikajaksolle 2011-2017.")
                           )
                       ),
                       fluidRow(
                           column(
                               width = 6,
                               descriptionBlock(
                                   number = lt_number, 
                                   number_color = lt_number_color, 
                                   number_icon = lt_number_icon,
                                   header = "", 
                                   text = paste(i18n()$t("Pitkän aikavälin muutos")), 
                                   right_border = TRUE,
                                   margin_bottom = TRUE
                               )
                           ),
                           column(
                               width = 6,
                               descriptionBlock(
                                   number = st_number, 
                                   number_color = st_number_color, 
                                   number_icon = st_number_icon,
                                   header = "", 
                                   text = paste(i18n()$t("Lyhyen aikavälin muutos")), 
                                   right_border = TRUE,
                                   margin_bottom = TRUE
                               )
                           ),
                           column(width = 12,
                                  descriptionBlock(
                                      header = paste(i18n()$t("Päivittäiset keskirunsaudet yhteensä")), 
                                      right_border = TRUE,
                                      margin_bottom = FALSE
                                  )
                           ),
                           column(
                               width = 4,
                               descriptionBlock(
                                   header = format(round(stats_current$Nbegin, 0),
                                                   big.mark = " "),
                                   text = "1979-1999", 
                                   right_border = TRUE,
                                   margin_bottom = FALSE
                               )
                           ),
                           column(
                               width = 4,
                               descriptionBlock(
                                   header = format(round(stats_current$Nmed, 0),
                                                   big.mark = " "),
                                   text = "2000-2010", 
                                   right_border = FALSE,
                                   margin_bottom = FALSE
                               )
                           ),
                           column(
                               width = 4,
                               descriptionBlock(
                                   header = format(round(stats_current$Nend, 0),
                                                   big.mark = " "), 
                                   text = "2011-2017",
                                   right_border = FALSE,
                                   margin_bottom = FALSE
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
        sp_abb <- sp_current$Species_Abb
        
        # Define origing date
        origin <- as.Date("2000-01-01")
        
        # abundance_stats has already been loaded
        plot_data <- abundance_stats %>%
            # Filter with selected species
            filter(sp == sp_abb) %>%
            # Select only median values (Julian days) for i) the three epochs
            # and ii) spring and autumn
            select(sp, sphen_begin, sphen_med, sphen_end, aphen_begin, 
                   aphen_med, aphen_end) %>% 
            # Make data tidy (long)
            gather(variable, value, -sp) %>%
            # Split variables into two new columns
            separate(col = "variable", into = c("season", "epoch"), sep = "_") %>% 
            # Mutate new variables
            mutate(
                   # Replace "sphen" and "aphen" with more informative strings
                   season = ifelse(season == "sphen", tolower(i18n()$t("Kevät")), 
                                   ifelse(season == "aphen", tolower(i18n()$t("Syys")), NA)),
                   # Make epochs factors
                   epoch = factor(epoch, levels = c("begin", "med", "end"),
                                  labels = rev(c("1979-1999", "2000-2010", "2011-2017")),
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
                   name = c("1979-1999", "2000-2010", "2011-2017"),
                   color = ggsci::pal_d3("category10")(3)) %>% 
            hc_yAxis(title = list(text = ""),
                     min = 0,
                     max = 2,
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
            dplyr::mutate(Date = lubridate::dmy(Date)) %>% 
            dplyr::group_by(sp, Sum) %>% 
            dplyr::mutate(date_string = paste(Date, collapse = i18n()$t(" ja "))) %>% 
            dplyr::select(-Date) %>% 
            dplyr::distinct()
        
        if (nrow(records_current) == 0) {
            payload <- NULL
        } else {
            
            get_value <- function(season, type, value) {
                res <- records_current %>%
                    dplyr::filter(Season == season & Type == type) %>% 
                    dplyr::pull(!!value)
                
                if (is.numeric(res)) {
                    res <- format(res, big.mark = " ")
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
                   widgetUserBox(
                     title = i18n()$t("Aineisto"),
                     subtitle = i18n()$t("Hangon lintuaseman pitkäaikaisaineisto"),
                     type = NULL,
                     width = 12,
                     src = "database.png",
                     color = "navy-active",
                     closable = FALSE,
                     collapsible = FALSE,
                     data_help_content
                   )
            ),
            column(width = 6,
                   widgetUserBox(
                     title = i18n()$t("Verkkosovellus"),
                     subtitle = "Muuttolintuselain Haahka",
                     type = NULL,
                     width = 12,
                     src = "settings.png",
                     color = "navy-active",
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
    
    # Update values based on an URL query
    observe({
        # Parse the query string from the current URL
        query <- parseQueryString(session$clientData$url_search)
        # Update species selector based on the query.
        # NOTE: nothing will happen if the 3+3 abbreviation provided as an
        # URL parameter is not found.
        if (!is.null(query[['species']])) {
            # Capitalize for join
            sp_abb <- toupper(query[['species']])
            # Get the same name format as used in the species selector
            spps <- get_species_names("fi")
            # Join to get the 3+3 species abbreviation
            selected_sp <- sp_data %>% 
                dplyr::filter(Species_Abb == sp_abb)
            
            if (tolower(selected_sp$Species_Abb) != INTENDED_SPECIES) {
              # Intended language needs to be update first because the 
              # observeEvent(input$language, {...}) is triggered *before* the
              # value of input$language is changed.
              INTENDED_SPECIES <<- tolower(selected_sp$Species_Abb)
              message("Intended species changed (URL) to: ", INTENDED_SPECIES)
              # Mark the global variable indivating that the change has been
              # changed from the URL
              REQUEST_FROM_URL <<- TRUE
            }
        }
        # Update language selector based in the query.
        if (!is.null(query[['language']])) {
            url_language <- tolower(query[['language']])
            # Global variable INTENDED_LANGUAGE tracks the current intended 
            # language (i.e. the one user wants to use). Default value is "".
            # Change the language selector only if the language provided in the
            # URL query is different to the intended language and if the 
            # provided language actually exists in the translator.
            if (url_language != INTENDED_LANGUAGE & url_language %in% translator$languages) {
                # Intended language needs to be update first because the 
                # observeEvent(input$language, {...}) is triggered *before* the
                # value of input$language is changed.
                INTENDED_LANGUAGE <<- url_language
                updateSelectInput(session, "language",
                                  label = i18n()$t("Kieli"),
                                  selected = url_language)
            }
        }
    })
    
    # Whenever the language selector is used, update the URL to match
    observeEvent(input$language, {
        current_sp <- get_species_abbr()
        current_lang <- input$language
        # Update the intended language if needed
        if (current_lang != INTENDED_LANGUAGE) {
            INTENDED_LANGUAGE <<- current_lang
        }
        query_string <- paste0("?species=", current_sp, 
                               "&language=", INTENDED_LANGUAGE)
        session$updateQueryString(query_string, mode = "push")
        
    })
    
    # Whenever the species selector is used, update the URL to match
    observeEvent(input$species, {
        current_sp <- get_species_abbr()
        current_lang <- input$language
        
        if (current_sp != INTENDED_SPECIES && !REQUEST_FROM_URL) {
          INTENDED_SPECIES <<- current_sp
          message("Intended species changed (observer) to: ", INTENDED_SPECIES)
        }
        
        if (REQUEST_FROM_URL) {
          REQUEST_FROM_URL <<- FALSE
        }
        
        query_string <- paste0("?species=", INTENDED_SPECIES, 
                               "&language=", current_lang)
        session$updateQueryString(query_string, mode = "push")
        
        # Capitalize for join
        sp_abb <- toupper(INTENDED_SPECIES)
        # Get the same name format as used in the species selector
        spps <- get_species_names("fi")
        # Join to get the 3+3 species abbreviation
        selected_sp <- sp_data %>% 
          dplyr::filter(Species_Abb == sp_abb)
        
        updateSelectInput(session, "species", 
                          selected = spps[which(spps == selected_sp$Sci_name)])
        
        message("Species changed to: ", INTENDED_SPECIES)  
    })
    
    observeEvent(i18n(), {
        updateSelectInput(session, "language", label = i18n()$t("Kieli"), 
                          selected = input$language)
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
}

shinyApp(ui, server)
