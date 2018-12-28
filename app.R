library(forcats)
library(glue)
library(highcharter)
library(officer)
library(shiny)
library(shiny.i18n)
library(shinycssloaders)
library(shinydashboard)
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

spps <- sp_data$Sci_name

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
                column(6,
                       box(
                           width = 12,
                           column(6,
                                  uiOutput("render_selector")
                           ),
                           column(6
                                  #checkboxInput("fixy", "Fix y-axis")
                           )
                       ),
                       box(
                           width = 12,
                           withSpinner(highchartOutput("migration", height = "300px"),
                                       type = 4, size = 0.8)
                           
                       ),
                       box(
                           width = 12,
                           withSpinner(highchartOutput("local", height = "300px"),
                                       type = 4, size = 0.8)
                       ),
                       box(
                           width = 12,
                           withSpinner(highchartOutput("change", height = "300px"),
                                       type = 4, size = 0.8)
                       )
                ),
                column(6,
                       box(
                           width = 12, collapsible = TRUE,
                           uiOutput("image")
                       ),
                       box(
                           width = 12,
                           uiOutput("description")
                       )
                )
            )
        )
    )
)

server <- function(input, output, session) {
    
    i18n <- reactive({
        selected <- input$language
        if (length(selected) > 0 && selected %in% translator$languages) {
            message("Language changed to: ", selected)
            translator$set_translation_language(selected)
        }
        translator
    })
    
    get_current_sp <- reactive({
        if (!is.null(input$selector)) {
            return(dplyr::filter(sp_data, Sci_name == input$selector))
        } else {
            return(NULL)
        }
    })
    
    get_current_data <- reactive({
        sp_current <- get_current_sp()
        if (!is.null(sp_current)) {
            return(dplyr::filter(dat, sp == sp_current$Species_Abb) )
        } else {
            return(NULL)
        }
    })
    
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
        tagList(
            selectInput("selector", 
                        label = i18n()$t("Valitse laji"),
                        choices = spps,
                        selected = input$selector)
        )
    })
    
    observeEvent(i18n(), {
        updateSelectInput(session, "language", label =  i18n()$t("Kieli"), selected = input$language)
        updateSelectInput(session, "selector", label =  i18n()$t("Valitse laji"), selected = input$selector)
    })
    
    output$image <- renderUI({
        
        current_sp <- get_current_sp()

        if (!is.null(current_sp)) {
            sp_abbr <- tolower(current_sp$Species_Abb)
            
            # The actual file path is needed to figure out if the file exists
            img_file <- file.path("www", "img", "sp_images", paste0(sp_abbr, ".jpg"))
            
            if (file.exists(img_file)) {
                # Photo credit
                photo_credit <- PHOTO_CREDITS[[sp_abbr]]
                # If the file does exist, use tags instead of rendering the image
                # directly. This way the browser will cache the image.
                payload <- shiny::div(shiny::img(src = glue::glue("img/sp_images/{sp_abbr}.jpg"),
                                                 width = "100%"),
                                      shiny::p(glue::glue("Ⓒ {photo_credit}")))
            } else {
                payload <- shiny::p("No image found")
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
                        h2(common_name, class = "description"),
                        h4(sci_name, class = "description"),
                        br(),
                        docx_content %>% 
                            dplyr::rowwise() %>% 
                            do(row = parse_description(.$style_name, .$text)) %>% 
                            as.list()
                    )
                )
            } else {
                payload <- withTags(
                    div(
                        h2(common_name),
                        h4(sci_name),
                        br(),
                        p("No description found.")
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
                       name = "Migrating",
                       color = "#1f78b4") %>% 
                hc_yAxis(title = list(text = "Individuals")) %>% 
                hc_xAxis(title = list(text = ""),
                         type = "datetime", 
                         dateTimeLabelFormats = list(month = '%b'),
                         tickInterval = X_AXIS_TIME_UNITS) %>% 
                hc_title(text = "Migrants") %>% 
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
                       name = "Locals",
                       color = "#1f78b4") %>% 
                hc_yAxis(title = list(text = "Individuals")) %>% 
                hc_xAxis(title = list(text = ""),
                         type = "datetime", 
                         dateTimeLabelFormats = list(month = '%b'),
                         tickInterval = X_AXIS_TIME_UNITS) %>% 
                hc_title(text = "Locals") %>% 
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
                hc_yAxis(title = list(text = "Individuals")) %>% 
                hc_xAxis(title = list(text = ""),
                         type = "datetime", 
                         dateTimeLabelFormats = list(month = '%b'),
                         tickInterval = X_AXIS_TIME_UNITS) %>% 
                hc_title(text = "Change in all individuals") %>% 
                hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                           shared = TRUE, xDateFormat = "%b %d") %>% 
                hc_exporting(enabled = TRUE) %>% 
                hc_chart(zoomType = "xy")
            
            return(hc)
        }
    })
}

shinyApp(ui, server)
