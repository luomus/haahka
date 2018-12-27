library(forcats)
library(glue)
library(highcharter)
library(lubridate)
library(officer)
library(shiny)
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

parse_metadata <- function() {
    return(yaml::yaml.load_file("DESCRIPTION"))    
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
    dplyr::arrange(Species_code)

spps <- sp_data$Sci_name

# Global variables --------------------------------------------------------

# How many milliseconds in a year?
X_AXIS_TIME_UNITS = 30 * 24 * 3600 * 1000
# Get the app metadata from the DESCRIPTION file
METADATA <- parse_metadata()
VERSION <- METADATA[["Version"]]

ui <- dashboardPage(
    dashboardHeader(
        title = tags$a(href = "https://www.tringa.fi/hangon-lintuasema/hankodata",
                       tags$img(src = "browser_logo.png", height = "40")
        )
    ),
    dashboardSidebar(collapsed = TRUE,
                     div(style = "text-align: center",
                         h4(glue("version: {VERSION}"))
                     )),
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
                                  selectInput("selector", "Select species", 
                                              choices = spps)
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
                           width = 12,
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

server <- function(input, output) {
    
    output$image <- renderUI({
        current_sp <- sp_data %>% 
            filter(Sci_name == input$selector)
        common_name <- current_sp$ENG_name
        sci_name <- current_sp$Sci_name
        sp_abbr <- tolower(current_sp$Species_Abb)
        
        # The actual file path is needed to figure out if the file exists
        img_file <- file.path("www", "img", "sp_images", paste0(sp_abbr, ".jpg"))
        
        if (file.exists(img_file)) {
            # If the file does exist, use tags instead of rendering the image
            # directly. This way the browser will cache the image.
            payload <- shiny::div(shiny::img(src = glue::glue("img/sp_images/{sp_abbr}.jpg"),
                                  width = "100%"),
                                  shiny::p(""))
        } else {
            payload <- shiny::p("No image found")
        }
        return(payload)
        
    })
    
    output$description <- renderUI({
        
        current_sp <- sp_data %>% 
            filter(Sci_name == input$selector)
        common_name <- current_sp$ENG_name
        sci_name <- current_sp$Sci_name
        sp_abbr <- current_sp$Species_Abb
        
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
    })
    
    output$migration <- renderHighchart({
        
        sp_current <- sp_data %>% 
            dplyr::filter(Sci_name == input$selector)
        
        obs_current <- dat %>% 
            dplyr::filter(sp == sp_current$Species_Abb) 
        
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
    })
    
    output$local <- renderHighchart({
        
        sp_current <- sp_data %>% 
            dplyr::filter(Sci_name == input$selector)
        
        obs_current <- dat %>% 
            dplyr::filter(sp == sp_current$Species_Abb) 
        
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
    })
    
    output$change <- renderHighchart({
        
        sp_current <- sp_data %>% 
            dplyr::filter(Sci_name == input$selector)
        
        obs_current <- dat %>% 
            dplyr::filter(sp == sp_current$Species_Abb) 
        
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
    })
    
    output$plot1 <- renderPlot({
        
        sp_current <- sp_data %>% 
            dplyr::filter(Sci_name == input$selector)
        
        sp_name <- sp_current$Sci_name
        
        obs_current <- dat %>% 
            dplyr::filter(sp == sp_current$Species_Abb)
        
        # Make a subselectiong of the data containing two different epochs:
        # 1979-1999 and 2009-
        epochs <- obs_current %>% 
            dplyr::select(day, begin, end) %>% 
            tidyr::gather(variable, value, -day)
        
        
        p1 <- ggplot(obs_current, aes(x = day, y = paik)) +
            geom_line() + xlab("Day of Year") + ylab("Yks./pvm - Ind./day") +
            ggtitle(paste0(sp_name, ", Paikalliset / Lokal / Locals")) +
            scale_x_date(labels = date_format('%e %b')) + theme_bw()
        
        p2 <- ggplot(obs_current, aes(x = day, y = muutto)) +
            geom_line() + xlab("Day of Year") + ylab("Yks./pvm - Ind./day") +
            ggtitle(paste0(sp_name, ", Muuttavat / Flyttande / Migrants")) +
            scale_x_date(labels = date_format('%e %b')) + theme_bw()
        
        p3 <- ggplot(epochs, aes(x = day, y = value, color = variable)) +
            geom_line() + xlab("Day of Year") + ylab("Yks./pvm - Ind./day") +
            ggtitle("Muutos / Förändring / Change") +
            scale_color_manual(values = c("red", "blue"),
                               labels = c("1979-1999", "2009-")) +
            scale_x_date(labels = date_format('%e %b')) +
            theme_bw() + theme(legend.title = element_blank(), legend.position = c(0.9, 0.75))
        
        p4 <- grid.arrange(p1, p2, p3, nrow = 3, ncol = 1)
    })
}

shinyApp(ui, server)
