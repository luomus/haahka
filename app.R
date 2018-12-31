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
library(yaml)

# Helper functions --------------------------------------------------------

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

simple_cap <- Vectorize(
    function(x) {
        s <- strsplit(x, " ")[[1]]
        s <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
                   sep = "", collapse = " ")
        return(s)
    }, SIMPLIFY = TRUE, USE.NAMES = FALSE)

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

# How many milliseconds in a year?
X_AXIS_TIME_UNITS = 30 * 24 * 3600 * 1000
# Get the app metadata from the DESCRIPTION file
METADATA <- yaml::yaml.load_file("DESCRIPTION")
VERSION <- METADATA[["Version"]]

# Get photo credits
PHOTO_CREDITS <- yaml::yaml.load_file("www/img/sp_images/attribution.yaml")

ui <- dashboardPage(
    dashboardHeader(
        title = tags$a(href = "https://www.tringa.fi/hangon-lintuasema/hankodata",
                       tags$img(src = "browser_logo.png", height = "40")
        )
    ),
    dashboardSidebar(collapsed = TRUE,
                     uiOutput("render_sidebar")
        ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        fluidPage(
            fluidRow(
                column(12,
                       box(width = 12,
                           uiOutput("render_selector")
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
                       uiOutput("change_numbers")
                )
            )
        )
    )
)

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
        #browser()
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
    
    output$image_slider <- renderSlickR({
        
        imgs <- get_images()
        
        shiny::validate(
            no_images(imgs)
        )
        
        slickR::slickR(imgs, width = "100%")
    })
    
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
            #browser()
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
    
    output$migration <- renderHighchart({
        
        obs_current <- get_current_data()
        
        if (!is.null(obs_current)) {
            hc <- obs_current %>% 
                hchart(type = "spline", 
                       hcaes(x = day, y = muutto),
                       name = i18n()$t("Muuttavien keskiarvot"),
                       color = "#1f78b4") %>% 
                hc_yAxis(title = list(text = i18n()$t("Yksilölkm."))) %>% 
                hc_xAxis(title = list(text = ""),
                         type = "datetime", 
                         dateTimeLabelFormats = list(month = '%b'),
                         tickInterval = X_AXIS_TIME_UNITS) %>% 
                hc_title(text = i18n()$t("Muuttavien keskiarvot")) %>% 
                hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                           xDateFormat = "%b %d") %>% 
                hc_exporting(enabled = TRUE) %>% 
                hc_chart(zoomType = "xy")
            
            return(hc)
        }
    })
    
    output$local <- renderHighchart({
        
        obs_current <- get_current_data()

        if (!is.null(obs_current)) {
            hc <- obs_current %>% 
                hchart(type = "spline", 
                       hcaes(x = day, y = paik),
                       name = i18n()$t("Paikallisten keskiarvot"),
                       color = "#1f78b4") %>% 
                hc_yAxis(title = list(text = i18n()$t("Yksilölkm."))) %>% 
                hc_xAxis(title = list(text = ""),
                         type = "datetime", 
                         dateTimeLabelFormats = list(month = '%b'),
                         tickInterval = X_AXIS_TIME_UNITS) %>% 
                hc_title(text = i18n()$t("Paikallisten keskiarvot")) %>% 
                hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                           xDateFormat = "%b %d") %>% 
                hc_exporting(enabled = TRUE) %>% 
                hc_chart(zoomType = "xy")
            
            return(hc)
        }
    })
    
    output$change <- renderHighchart({
        
        obs_current <- get_current_data()

        if (!is.null(obs_current)) {    
        
            epochs <- obs_current %>% 
                dplyr::select(day, begin, med, end) %>% 
                tidyr::gather(epoch, value, -day) %>% 
                dplyr::mutate(epoch = forcats::fct_relevel(epoch, "begin", "med", "end"))
            
            hc <- epochs %>% 
                hchart(type = "spline", 
                       hcaes(x = day, y = value, group = epoch),
                       # order of epochs c("begin", "end", "med")
                       name = c("1979-1999", "2000-2010", "2011-2018"),
                       color = c("#66c2a5", "#8da0cb", "#fc8d62")) %>% 
                hc_yAxis(title = list(text = i18n()$t("Yksilölkm."))) %>% 
                hc_xAxis(title = list(text = ""),
                         type = "datetime", 
                         dateTimeLabelFormats = list(month = '%b'),
                         tickInterval = X_AXIS_TIME_UNITS) %>% 
                hc_title(text = i18n()$t("Runsauksien muutokset")) %>% 
                hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                           shared = TRUE, xDateFormat = "%b %d") %>% 
                hc_exporting(enabled = TRUE) %>% 
                hc_chart(zoomType = "xy")
            
            return(hc)
        }
    })
    
    output$change_numbers <- renderUI({
        
        stats_current <- get_current_stats()
        
        # Sort out the the trend numbers and UI components
        # Long term
        if (!is.na(stats_current$slopeLong) & stats_current$slopeLong > 0) {
            lt_number_color <- "green"
            lt_number_icon <- "fa fa-caret-up"
            lt_number <- paste0(stats_current$slopeLong, "%")
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
            st_number <- paste0(stats_current$slopeShort, "%")
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
                                   margin_bottom = FALSE
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
                                   margin_bottom = FALSE
                               )
                           ),
                           column(
                               width = 4,
                               descriptionBlock(
                                   header = round(stats_current$Nbegin, 0),
                                   text = paste(i18n()$t("Keskirunsaus"), "1970-1999"), 
                                   right_border = TRUE,
                                   margin_bottom = FALSE
                               )
                           ),
                           column(
                               width = 4,
                               descriptionBlock(
                                   header = round(stats_current$Nmed, 0), 
                                   text = paste(i18n()$t("Keskirunsaus"), "2000-2010"), 
                                   right_border = FALSE,
                                   margin_bottom = FALSE
                               )
                           ),
                           column(
                               width = 4,
                               descriptionBlock(
                                   header = round(stats_current$Nend, 0), 
                                   text = paste(i18n()$t("Keskirunsaus"), "2011-2017"),
                                   right_border = FALSE,
                                   margin_bottom = FALSE
                               )
                           )
                       )
        )
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
