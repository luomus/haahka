library(assertthat)
library(chron)
library(forcats)
library(glue)
library(highcharter)
library(officer)
library(shiny)
library(shiny.i18n)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(slickR)
library(tidyverse)
library(tsibble)
library(yaml)

# Helper functions --------------------------------------------------------

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
 
# Parse description file
# 
parse_description <- function(style_name, text) {
    # NOTE: style is hard coded and needs to be adjusted if the style changes
    # Check both the element style and content
    if (style_name == "No Spacing" & text != "") {
        element <- shiny::p(text, class = "description")
    } else if (style_name == "Endnote Text" & text != "") {
        element <- shiny::p(text, class = "endnote")
    } else if (style_name == "Heading 3" & text != "") {
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

# Global variables --------------------------------------------------------

# Get the app metadata from the DESCRIPTION file
METADATA <- yaml::yaml.load_file("DESCRIPTION")
VERSION <- METADATA[["Version"]]

# Get photo credits
PHOTO_CREDITS <- yaml::yaml.load_file("www/img/sp_images/attribution.yaml")

# Highcharts options

# # How many milliseconds in a year?
X_AXIS_TIME_UNITS = 30 * 24 * 3600 * 1000
# Year x-axis limits
XMIN <- datetime_to_timestamp(as.Date('2000-01-01', tz = 'UTC'))
XMAX <- datetime_to_timestamp(as.Date('2000-12-31', tz = 'UTC'))

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


# UI ----------------------------------------------------------------------
ui <- dashboardPage(
    
    title = "Halias Browser",
    
    # ui-header ----------------------------------------------------------------
    dashboardHeader(
        title = tags$a(href = "https://www.tringa.fi/hangon-lintuasema/hankodata",
                       tags$img(src = "browser_logo.png", height = "40")
        )
    ),
    # ui-sidebar ---------------------------------------------------------------
    dashboardSidebar(collapsed = TRUE,
                     uiOutput("render_sidebar")
        ),
    # ui-body ------------------------------------------------------------------
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        fluidPage(
            fluidRow(
                column(6,
                       box(width = 12,
                           uiOutput("render_selector")
                       )
                ),
                column(6,
                       box(width = 12,
                           column(4,
                                  sliderInput("tile_selector", 
                                              "Tile window (days) for averaging",
                                              min = 1, max = 7, step = 2, value = 5,
                                              ticks = TRUE)
                                  ),
                           column(4,
                                  radioButtons("line_type", "Line type", 
                                               choiceNames = c("Line", "Spline"),
                                               choiceValues = c("line", "spline"))
                            ),
                           column(4,
                                  checkboxInput("show_markers", "Show line markers", value = FALSE),
                                  checkboxInput("show_plotbands", "Show month plot bands", value = FALSE)
                                  )
                        )
                )
            ),
            fluidRow(
                column(6,
                       #uiOutput("render_image"),
                       box(
                           width = 12,
                           withSpinner(uiOutput("description"), type = 8, 
                                       size = 0.5)
                       )
                ),
                column(6,
                       box(
                           width = 12,
                           withSpinner(highchartOutput("migration", height = "300px"),
                                       type = 8, size = 0.5)
                           
                       ),
                       box(
                           width = 12,
                           withSpinner(highchartOutput("local", height = "300px"),
                                       type = 8, size = 0.5)
                       ),
                       box(
                           width = 12,
                           withSpinner(highchartOutput("change", height = "300px"),
                                       type = 8, size = 0.5)
                       ),
                       box(
                           width = 12,
                           withSpinner(highchartOutput("migration_medians", height = "200px"),
                                       type = 8, size = 0.5)
                       ),
                       uiOutput("change_numbers"),
                       uiOutput("records")
                )
            )
        )
    )
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
    
    # Reactives ---------------------------------------------------------------
    i18n <- reactive({
        selected <- input$language
        if (length(selected) > 0 && selected %in% translator$languages) {
            message("Language changed to: ", selected)
            translator$set_translation_language(selected)
        }
        translator
    })
    
    get_current_sp <- reactive({
        shiny::req(input$selector)
        
        return(dplyr::filter(sp_data, Sci_name == input$selector))
    })
    
    get_current_data <- reactive({
        sp_current <- get_current_sp()
        return(dplyr::filter(dat, sp == sp_current$Species_Abb) )
    })
    
    get_current_records <- reactive({
        sp_current <- get_current_sp()
        current_stats <- record_stats %>% 
            dplyr::filter(sp == sp_current$Species_Abb)
        return(current_stats)
    })
    
    get_current_stats <- reactive({
        sp_current <- get_current_sp()
        current_stats <- abundance_stats %>% 
            dplyr::filter(sp == sp_current$Species_Abb)
        return(current_stats)
    })
    
    get_images <- reactive({
        current_sp <- get_current_sp()    
        
        sp_abbr <- tolower(current_sp$Species_Abb)
        
        # The actual dir path is needed to figure out if the files exists
        img_dir <- file.path("www", "img", "sp_images", sp_abbr)
        # Photo credit
        #photo_credit <- PHOTO_CREDITS[[sp_abbr]]
        imgs <- list.files(img_dir, pattern = ".jpg", full.names = TRUE)
        return(imgs)
    })
    
    # Helper functions ---------------------------------------------------------
    
    no_images <- function(files) {
        if (length(files) == 0) {
            i18n()$t("Ei kuvia tälle lajille")
        } else {
            NULL
        }
    }
    
    get_species_names <- function(lang) {
        if (!is.null(lang)) {
            if (lang == "fi") {
                name_field = "FIN_name"
            } else if (lang == "en") {
                name_field = "ENG_name"
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
    
    # Outputs ------------------------------------------------------------------

    # render_sidebar -----------------------------------------------------------    
    output$render_sidebar <- renderUI({
        tagList(
            div(style = "text-align: center",
                h4(VERSION)
            ),
            selectInput("language",
                        label = i18n()$t("Kieli"),
                        choices = translator$languages, 
                        selected = input$language)
        )
    })
    
    # render_selector ----------------------------------------------------------
    output$render_selector <- renderUI({
        
        if (is.null(input$language)) {
            # By default, the names are Finnish
            name_field <- "fi"            
        } else {
            name_field <- input$language
        }
        
        spps <- get_species_names(name_field)
        
        tagList(
            selectInput("selector", 
                        label = i18n()$t("Valitse laji"),
                        choices = spps,
                        selected = input$selector)
        )
    })
    
    # render_carousel ----------------------------------------------------------
    output$render_carousel <- renderUI({
        
        imgs <- get_images()
        
        if (length(imgs) == 0) {
            collapsed = TRUE           
        } else {
            collapsed = FALSE
        }
        
        payload <- tagList(
            box(
                width = 12, collapsible = TRUE, collapsed = collapsed,
                slickROutput("image_slider")
            )
        )
        return(payload)
    })
    
    # image_slider -------------------------------------------------------------
    output$image_slider <- renderSlickR({
        
        imgs <- get_images()
        
        shiny::validate(
            no_images(imgs)
        )
        
        slickR::slickR(imgs, width = "100%")
    })
    
    # render_image -------------------------------------------------------------
    output$render_image <- renderUI({
        
        imgs <- get_images()
        
        if (length(imgs) == 0) {
            collapsed = TRUE           
        } else {
            collapsed = FALSE
        }
        
        payload <- tagList(
            box(
                width = 12, collapsible = TRUE, collapsed = collapsed,
                uiOutput("image")
            )
        )
        return(payload)
    })
    
    # image --------------------------------------------------------------------
    output$image <- renderUI({
        
        current_sp <- get_current_sp()

        if (!is.null(current_sp)) {
            sp_abbr <- tolower(current_sp$Species_Abb)
            
            # The actual file path is needed to figure out if the file exists
            img_file <- file.path("www", "img", "sp_images", sp_abbr, 
                                  paste0(sp_abbr, ".jpg"))
            
            if (file.exists(img_file)) {
                # Photo credit
                photo_credit <- PHOTO_CREDITS[[sp_abbr]]
                # If the file does exist, use tags instead of rendering the image
                # directly. This way the browser will cache the image.
                payload <- shiny::div(shiny::img(src = glue::glue("img/sp_images/{sp_abbr}/{sp_abbr}.jpg"),
                                                 width = "95%", class = "description"),
                                      shiny::p(glue::glue("Ⓒ {photo_credit}"), 
                                               class = "description"),
                                      shiny::br())
            } else {
                payload <- shiny::p(i18n()$t("Kuvaa ei löydy"), class = "description")
            }
            return(payload)
        }        
    })
    
    # description --------------------------------------------------------------
    output$description <- renderUI({
        
        current_sp <- get_current_sp()
        
        if (!is.null(current_sp)) {
            # Define species names
            sp_abbr <- tolower(current_sp$Species_Abb)
            sci_name <- current_sp$Sci_name
            
            if (input$language == "en") {
                common_name <- current_sp$ENG_name
            } else if (input$language == "fi") {
                common_name <- current_sp$FIN_name
            }
            
            # Try reading the description docx file
            docx_file <- file.path("data", "descriptions", paste0(tolower(sp_abbr), ".docx"))
            
            if (file.exists(docx_file)) {
                
                docx_content <- officer::docx_summary(officer::read_docx(docx_file))
                
                payload <- withTags(
                    div(
                        shiny::h2(common_name, class = "description"),
                        shiny::h3(sci_name, class = "description sci-name"),
                        shiny::br(),
                        uiOutput("image"),
                        docx_content %>% 
                            dplyr::rowwise() %>% 
                            do(row = parse_description(.$style_name, .$text)) %>% 
                            as.list()
                    )
                )
            } else {
                payload <- withTags(
                    shiny::div(
                        shiny::h2(common_name, class = "description"),
                        shiny::h3(sci_name, class = "description sci-name"),
                        shiny::br(),
                        uiOutput("image"),
                        shiny::p(i18n()$t("Kuvausta ei löydy"), class = "description")
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
            tile_observations("day", "muutto", input$tile_selector)
        
        if (!is.null(plot_data)) {
            
            if (input$show_plotbands) {
                pb_list <- PB_LIST
            } else {
                pb_list <- NA
            }
            
            hc <- plot_data %>% 
                hchart(type = input$line_type, 
                       hcaes(x = day, y = value_avgs),
                       name = i18n()$t("Muuttavien keskiarvot"),
                       color = "#1f78b4") %>% 
                hc_yAxis(title = list(text = i18n()$t("Yksilölkm."))) %>% 
                hc_xAxis(title = list(text = ""),
                         type = "datetime", 
                         min = XMIN,
                         max = XMAX,
                         dateTimeLabelFormats = list(month = '%b'),
                         tickInterval = X_AXIS_TIME_UNITS,
                         plotBands = pb_list) %>% 
                hc_plotOptions(line = list(marker = list(enabled = input$show_markers)),
                               spline = list(marker = list(enabled = input$show_markers))) %>% 
                hc_title(text = i18n()$t("Muuttavien keskiarvot")) %>% 
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
            tile_observations("day", "paik", input$tile_selector)
        
        if (!is.null(plot_data)) {
            
            if (input$show_plotbands) {
                pb_list <- PB_LIST
            } else {
                pb_list <- NA
            }
            
            hc <- plot_data %>% 
                hchart(type = input$line_type, 
                       hcaes(x = day, y = value_avgs),
                       name = i18n()$t("Paikallisten keskiarvot"),
                       color = "#1f78b4") %>% 
                hc_yAxis(title = list(text = i18n()$t("Yksilölkm."))) %>% 
                hc_xAxis(title = list(text = ""),
                         type = "datetime", 
                         min = XMIN,
                         max = XMAX,
                         dateTimeLabelFormats = list(month = '%b'),
                         tickInterval = X_AXIS_TIME_UNITS,
                         plotBands = pb_list) %>%
                hc_plotOptions(line = list(marker = list(enabled = input$show_markers)),
                               spline = list(marker = list(enabled = input$show_markers))) %>% 
                hc_title(text = i18n()$t("Paikallisten keskiarvot")) %>% 
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
                tile_observations("day", "begin", input$tile_selector) %>% 
                dplyr::rename(begin = value_avgs)
            
            plot_data_med <- obs_current %>% 
                dplyr::select(sp, day, med) %>% 
                as_tsibble(key = id(sp), index = day) %>% 
                tile_observations("day", "med", input$tile_selector) %>% 
                dplyr::rename(med = value_avgs)
            
            plot_data_end <- obs_current %>% 
                dplyr::select(sp, day, end) %>% 
                as_tsibble(key = id(sp), index = day) %>% 
                tile_observations("day", "end", input$tile_selector) %>% 
                dplyr::rename(end = value_avgs)
            
            plot_data <- plot_data_begin %>% 
                dplyr::left_join(., plot_data_med, by = c("day" = "day")) %>% 
                dplyr::left_join(., plot_data_end, by = c("day" = "day")) %>%
                tidyr::gather(epoch, value, -day) %>% 
                dplyr::mutate(epoch = forcats::fct_relevel(epoch, "begin", "med", "end"))
            
            if (input$show_plotbands) {
                pb_list <- PB_LIST
            } else {
                pb_list <- NA
            }
            
            hc <- plot_data %>% 
                hchart(type = input$line_type, 
                       hcaes(x = day, y = value, group = epoch),
                       # order of epochs c("begin", "end", "med")
                       name = c("1979-1999", "2000-2010", "2011-2018"),
                       color = c("#66c2a5", "#8da0cb", "#fc8d62")) %>% 
                hc_yAxis(title = list(text = i18n()$t("Yksilölkm."))) %>% 
                hc_xAxis(title = list(text = ""),
                         type = "datetime", 
                         min = XMIN,
                         max = XMAX,
                         dateTimeLabelFormats = list(month = '%b'),
                         tickInterval = X_AXIS_TIME_UNITS,
                         plotBands = pb_list) %>% 
                hc_plotOptions(line = list(marker = list(enabled = input$show_markers)),
                               spline = list(marker = list(enabled = input$show_markers))) %>% 
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
                       title = i18n()$t("Runsauksien muutokset numeroina"),
                       background = NULL,
                       status = "danger",
                       footer = tagList(
                           p(
                               strong("Pitkän aikavälin muutos"), "=",
                               " keskirunsausden muutos aikajaksolta 1979-1999",
                               " aikajaksolle 2011-2017.",
                               br(),
                               strong("Lyhyen aikavälin muutos"), "=",
                               " keskirunsausden muutos aikajaksolta 2000-2010",
                               " aikajaksolle 2011-2017."
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
                                      header = paste(i18n()$t("Keskirunsaus tarkastelujaksolla")), 
                                      right_border = TRUE,
                                      margin_bottom = FALSE
                                  )
                           ),
                           column(
                               width = 4,
                               descriptionBlock(
                                   header = format(round(stats_current$Nbegin, 0),
                                                   big.mark = " "),
                                   text = "1970-1999", 
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
                   season = ifelse(season == "sphen", "spring", 
                                   ifelse(season == "aphen", "autumn", NA)),
                   # Make epochs factors
                   epoch = factor(epoch, levels = rev(c("begin", "med", "end")),
                                  labels = rev(c("1979-1999", "2000-2010", "2011-2017")),
                                  ordered = TRUE),
                   # Numeric value of the factors is needed so that highcharts
                   # can plot the factors on y-axis. Note that Javascript
                   # indexing starts from 0.
                   epochnum = as.numeric(epoch) - 1, 
                   # Convert Julian days into actual dates
                   date = origin + value, 
                   # Pretty version of the date for tooltips
                   date_print = format(date, "%b %d"))
        
        if (input$show_plotbands) {
            pb_list <- PB_LIST
        } else {
            pb_list <- NA
        }
        
        hc <- plot_data %>% 
            hchart(type = "scatter", 
                   hcaes(x = date, y = epochnum, group = epoch),
                   # order of epochs c("begin", "end", "med")
                   name = c("1979-1999", "2000-2010", "2011-2017"),
                   color = c("#66c2a5", "#8da0cb", "#fc8d62")) %>% 
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
                     plotBands = pb_list) %>% 
            hc_plotOptions(
                scatter = list(marker = list(symbol = "circle",
                                             radius = 8))
            ) %>% 
            hc_title(text = "Median observation date") %>% 
            hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                       shared = TRUE, xDateFormat = "%b %d",
                       pointFormat = "{point.season} migration median date:<br> {point.date_print}") %>% 
            hc_exporting(enabled = TRUE) %>% 
            hc_chart(zoomType = "xy")
        
        return(hc)
    })
    
    # records ------------------------------------------------------------------
    output$records <- renderUI({
        
        records_current <- get_current_records() %>% 
            dplyr::mutate(Date = lubridate::dmy(Date)) %>% 
            dplyr::group_by(sp, Sum) %>% 
            dplyr::mutate(date_string = paste0(Date, collapse = ", ")) %>% 
            dplyr::select(-Date) %>% 
            dplyr::distinct()
        
        if (nrow(records_current) == 0) {
            payload <- NULL
        } else {
            
            get_value <- function(season, type, value) {
                res <- records_current %>%
                    dplyr::filter(Season == season & Type == type) %>% 
                    dplyr::pull(!!value)
                if (length(res) == 0) {
                    res <- "-"
                }
                return(res)
            }
            
            payload <- box(width = 12,
                           solidHeader = FALSE,
                           title = i18n()$t("Havaintoennätykset"),
                           status = "info",
                           fluidRow(
                               column(
                                   width = 6,
                                   tagList(
                                       h4("Kevät", class = "record")
                                   ),
                                   column(
                                       width = 6,
                                       tagList(
                                           h5("Muuttavat", class = "record"),
                                           div(class = "record",
                                               column(width = 3,
                                                      icon("trophy", class = "icon-record")
                                               ),
                                               column(width = 9,
                                                      p(get_value("Spring", "Migr", "Sum"))                                 
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
                                           h5("Paikalliset", class = "record"),
                                           div(class = "record",
                                               column(width = 3,
                                                      icon("trophy", class = "icon-record")
                                               ),
                                               column(width = 9,
                                                      p(get_value("Spring", "Local", "Sum"))                                 
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
                               ),
                               column(
                                   width = 6,
                                   tagList(
                                       h4("Syksy", class = "record")
                                   ),
                                   column(
                                       width = 6,
                                       tagList(
                                           h5("Muuttavat", class = "record"),
                                           div(class = "record",
                                               column(width = 3,
                                                      icon("trophy", class = "icon-record")
                                               ),
                                               column(width = 9,
                                                      p(get_value("Autumn", "Migr", "Sum"))                                 
                                               ),
                                               column(width = 3,
                                                      icon("calendar", class = "icon-record")
                                               ),
                                               column(width = 9,
                                                      p(get_value("Autumn", "Migr", "date_string"))                                 
                                               )
                                           )
                                       )
                                   ),
                                   column(
                                       width = 6,
                                       tagList(
                                           h5("Paikalliset", class = "record"),
                                           div(class = "record",
                                               column(width = 3,
                                                      icon("trophy", class = "icon-record")
                                               ),
                                               column(width = 9,
                                                      p(get_value("Autumn", "Local", "Sum"))                                 
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

    # Observers ----------------------------------------------------------------
    
    observeEvent(i18n(), {
        updateSelectInput(session, "language", label =  i18n()$t("Kieli"), selected = input$language)
        updateSelectInput(session, "selector", label =  i18n()$t("Valitse laji"), 
                          choices = get_species_names(input$language), selected = input$selector)
    })
}

shinyApp(ui, server)
